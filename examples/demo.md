# Demo Scenario: AirLogic Refund Policy

## The Policy Context

AirLogic Airlines has a complex refund policy with multiple paths to eligibility:

**Refund Eligibility Rules (simplified from a 40-page policy document):**

1. **Airline-Caused Disruptions**: Full refund if flight cancelled by airline, or delay > 3 hours
2. **Flexible Fare**: Full refund if requested > 24 hours before departure
3. **Standard Fare**: Partial refund (minus $150 fee) if requested > 72 hours before departure
4. **Basic Economy**: No refund, but credit voucher if medical emergency (with documentation)
5. **Bereavement**: Partial refund for any fare class with death certificate of immediate family member
6. **EU261 Override**: Full refund regardless of fare class if flight departs from EU and delay > 5 hours

**Additional Guards:**
- Refunds over $500 require supervisor approval
- Any refund requires user to confirm they understand ticket becomes invalid
- Vouchers require user to accept T&C for voucher program

---

## Formal Rulebase (Excerpt)

```prolog
%% === TOOL BINDINGS ===

% Data tools - how base facts are established
user_bookings(U, Bookings) :-
    call(get_user_bookings, {user_id: U}) ~> $.bookings -> Bookings.

flight_status(F, Status) :-
    call(get_flight_details, {flight_id: F}) ~> $.status -> Status.

flight_delay_minutes(F, Minutes) :-
    call(get_flight_details, {flight_id: F}) ~> $.delay_minutes -> Minutes.

flight_departure_airport(F, Airport) :-
    call(get_flight_details, {flight_id: F}) ~> $.departure_airport -> Airport.

booking_fare_class(B, FareClass) :-
    call(get_booking, {booking_id: B}) ~> $.fare_class -> FareClass.

booking_amount(B, Amount) :-
    call(get_booking, {booking_id: B}) ~> $.total_amount -> Amount.

booking_flight(B, F) :-
    call(get_booking, {booking_id: B}) ~> $.flight_id -> F.

user_loyalty_tier(U, Tier) :-
    call(get_user_profile, {user_id: U}) ~> $.loyalty_tier -> Tier.

hours_until_departure(B, Hours) :-
    booking_flight(B, F),
    call(get_time_to_departure, {flight_id: F}) ~> $.hours -> Hours.

is_eu_airport(Airport) :-
    call(get_airport_info, {code: Airport}) ~> $.is_eu -> true.

has_supervisor_approval(B) :-
    call(check_approval_queue, {booking_id: B, type: "refund"}) ~> $.approved -> true.

%% === ASKABLE PREDICATES ===
% These can be established by asking the user

askable user_confirms_cancellation_understanding(U).
askable user_claims_medical_emergency(U).
askable user_claims_bereavement(U).
askable user_accepts_voucher_terms(U).
askable user_confirms_upload_documentation(U, DocType).

%% === DERIVED PREDICATES ===

% Flight disruption eligibility
airline_at_fault(B) :-
    booking_flight(B, F),
    flight_status(F, cancelled).

airline_at_fault(B) :-
    booking_flight(B, F),
    flight_delay_minutes(F, M),
    M > 180.  % 3 hours

% EU261 protection
eu261_eligible(B) :-
    booking_flight(B, F),
    flight_departure_airport(F, Airport),
    is_eu_airport(Airport),
    flight_delay_minutes(F, M),
    M > 300.  % 5 hours

% Fare-based eligibility
fare_allows_refund(B, full) :-
    booking_fare_class(B, "flexible").

fare_allows_refund(B, full) :-
    booking_fare_class(B, "flexible"),
    hours_until_departure(B, H),
    H > 24.

fare_allows_refund(B, partial) :-
    booking_fare_class(B, "standard"),
    hours_until_departure(B, H),
    H > 72.

fare_allows_refund(B, voucher) :-
    booking_fare_class(B, "basic"),
    user_claims_medical_emergency(current_user),
    user_confirms_upload_documentation(current_user, "medical_certificate").

% Bereavement path
bereavement_eligible(B) :-
    user_claims_bereavement(current_user),
    user_confirms_upload_documentation(current_user, "death_certificate").

% Main refund eligibility
eligible_for_refund(B, full, "airline_fault") :-
    airline_at_fault(B).

eligible_for_refund(B, full, "eu261") :-
    eu261_eligible(B).

eligible_for_refund(B, full, "flexible_fare") :-
    fare_allows_refund(B, full).

eligible_for_refund(B, partial, "standard_fare") :-
    fare_allows_refund(B, partial).

eligible_for_refund(B, partial, "bereavement") :-
    bereavement_eligible(B).

eligible_for_refund(B, voucher, "medical_basic") :-
    fare_allows_refund(B, voucher).

%% === GUARDS ===

guard(issue_refund(B, Amount)) :-
    eligible_for_refund(B, _, _),
    user_confirms_cancellation_understanding(current_user),
    (Amount < 500 ; has_supervisor_approval(B)).

guard(issue_voucher(B)) :-
    eligible_for_refund(B, voucher, _),
    user_accepts_voucher_terms(current_user).
```

## Demo Scenario A: Successful Refund (Complex Path)

### Setup

- **User**: Sarah Chen (Gold loyalty member)
- **Booking**: BK-2847 for flight AL-445 LHR->JFK
- **Fare class**: Standard ($892)
- **Flight status**: Delayed 6 hours (360 minutes)
- **Departure**: From London Heathrow ("Post-Brexit EU airport")

### Conversation Flow

#### TURN 1: User Request

**Sarah**: "Hi, I'd like a refund for my flight to New York. It's been delayed forever and I've made other arrangements."

#### SYSTEM: Fact Store Initialization

Pre-seeded facts (from authenticated session):
- `current_user = "usr_sarah_chen"`
- `current_time = "2025-01-15T14:30:00Z"`

Context slots (declared in rulebase, initially unbound):
- `booking_of_interest = null`

#### LLM AGENT: Identifies booking from user's description

User mentioned "flight to New York" - need to find matching booking.

**TOOL CALL**: `get_user_bookings({user_id: current_user})`

Tool returns:
```json
{
  "bookings": [
    {"booking_id": "BK-2847", "flight_id": "AL-445", "route": "LHR->JFK",
     "destination_city": "New York", "departure": "2025-01-15T18:00:00Z"},
    {"booking_id": "BK-3012", "flight_id": "AL-221", "route": "LHR->CDG",
     "destination_city": "Paris", "departure": "2025-02-20T09:00:00Z"}
  ]
}
```

LLM reasoning: User said "New York" -> likely matches BK-2847 (LHR->JFK). Will attempt to query eligibility.

#### LLM AGENT: Formulates symbolic query

Query: `eligible_for_refund(booking_of_interest, RefundType, Reason)`

Note: Purely symbolic - no literals like "BK-2847" or "$892".

#### SOLVER: Proof attempt #1 - BLOCKED on missing context

**Goal**: `eligible_for_refund(booking_of_interest, RefundType, Reason)`

**SOLVER RESULT**:
```
Blocked {
  missing_context: ["booking_of_interest"],
  reason: "Context slot 'booking_of_interest' is unbound",
  askable: true,
  candidates: [
    {"booking_id": "BK-2847", "description": "LHR→JFK (New York), Jan 15"},
    {"booking_id": "BK-3012", "description": "LHR→CDG (Paris), Feb 20"}
  ]
}
```

#### LLM AGENT: Requests context establishment

The solver indicates `booking_of_interest` must be explicitly established. The LLM issues a context establishment request:

**CONTEXT REQUEST**: `establish_context(booking_of_interest)`
```json
{
  "context_slot": "booking_of_interest",
  "question": "Which booking would you like to request a refund for?",
  "options": [
    {"value": "BK-2847", "label": "Flight to New York (LHR→JFK) - Jan 15, 2025"},
    {"value": "BK-3012", "label": "Flight to Paris (LHR→CDG) - Feb 20, 2025"}
  ],
  "suggested": "BK-2847"
}
```

#### UI: Context Selection Form

A mini-form appears in the chat:

```
┌─────────────────────────────────────────────────────┐
│ Which booking would you like to request a refund for? │
│                                                       │
│  ◉ Flight to New York (LHR→JFK) - Jan 15, 2025       │
│  ○ Flight to Paris (LHR→CDG) - Feb 20, 2025          │
│                                                       │
│                              [Confirm]                │
└─────────────────────────────────────────────────────┘
```

#### USER: Confirms selection

Sarah clicks "Confirm" with the pre-selected New York flight.

#### SYSTEM: Context established

```
context_store.establish(
  booking_of_interest,
  "BK-2847",
  established_via: "user_selection",
  timestamp: "2025-01-15T14:30:15Z"
)
```

Context binding: `booking_of_interest <- "BK-2847"` (user-confirmed)

#### SOLVER: Proof attempt #2 - Context now available

**Goal**: `eligible_for_refund(BK-2847, RefundType, Reason)`

Try rule: `eligible_for_refund(B, full, "airline_fault") :- airline_at_fault(B)`
- Subgoal: `airline_at_fault(BK-2847)`
  - Subgoal: `flight_status(F, cancelled)`
    - Need `booking_flight(BK-2847, F)`
      - **TOOL CALL**: `get_booking({booking_id: "BK-2847"})`
      - **GUARD CHECK**: `user_owns_booking(current_user, B)` -> ALLOWED
      - Tool returns: `{flight_id: "AL-445", fare_class: "standard", total_amount: 892}`
      - Facts added:
        - `booking_flight(BK-2847, AL-445)`
        - `booking_fare_class(BK-2847, "standard")`
        - `booking_amount(BK-2847, 892)`
    - **TOOL CALL**: `get_flight_details({flight_id: "AL-445"})`
      - Tool returns: `{status: "delayed", delay_minutes: 360, departure_airport: "LHR"}`
      - Facts added:
        - `flight_status(AL-445, "delayed")`
        - `flight_delay_minutes(AL-445, 360)`
        - `flight_departure_airport(AL-445, "LHR")`
  - `flight_status` is "delayed" not "cancelled"
  - Try: `flight_delay_minutes > 180`? 360 > 180 -> YES

**airline_at_fault(BK-2847) PROVEN**

**eligible_for_refund(BK-2847, full, "airline_fault") PROVEN**

Also checking `eu261_eligible` for additional grounds:
- `is_eu_airport("LHR")`?
  - **TOOL CALL**: `get_airport_info({code: "LHR"})`
  - Tool returns: `{is_eu: true, country: "UK", ...}`
  - Note: UK airports still in EU261 scheme post-Brexit for flights departing UK
- delay > 300? 360 > 300 -> YES

**eu261_eligible(BK-2847) PROVEN** (additional grounds)

**SOLVER RESULT**:
```
Success {
  bindings: {RefundType: full, Reason: "airline_fault"},
  alternative_bindings: [{RefundType: full, Reason: "eu261"}],
  trace: <proof tree>
}
```

#### LLM AGENT: Attempts to issue refund

Tool call: `issue_refund(booking_of_interest, booking_amount_of_interest)`

Resolved: `issue_refund("BK-2847", 892)`

#### GUARD EVALUATOR: Checking issue_refund guard

Guard:
```prolog
eligible_for_refund(B, _, _),
user_confirms_cancellation_understanding(current_user),
(Amount < 500 ; has_supervisor_approval(B))
```

- Check 1: `eligible_for_refund(BK-2847, _, _)` -> Already proven in fact store
- Check 2: `user_confirms_cancellation_understanding(current_user)`
  - Not in fact store
  - No tool can establish this
  - Marked as ASKABLE
  - **BLOCKED** - need user confirmation
- Check 3: `(892 < 500 ; has_supervisor_approval(BK-2847))`
  - 892 < 500 is FALSE
  - Check `has_supervisor_approval`...
    - **TOOL CALL**: `check_approval_queue({booking_id: "BK-2847"})`
    - Tool returns: `{approved: false, pending: false}`
  - No supervisor approval
  - **BLOCKED** - need approval for amount > $500

**GUARD RESULT**:
```
Denied {
  reason: [
    "user_confirms_cancellation_understanding not established",
    "Amount $892 exceeds $500 limit, supervisor approval required"
  ],
  askable: [user_confirms_cancellation_understanding(usr_sarah_chen)],
  can_request_escalation: true
}
```

#### TURN 2: LLM Response (asking user + requesting escalation)

**Agent**: "Good news, Sarah! Your flight AL-445 has been delayed over 6 hours, which qualifies you for a full refund of $892 under both our airline disruption policy and EU261 passenger rights.

Before I process this, I need to confirm: Do you understand that once refunded, your booking will be cancelled and the ticket cannot be used?

Also, since this refund exceeds $500, I'm requesting supervisor approval now - this usually takes just a moment."

[SYSTEM: Automatically triggered supervisor approval request]

#### TURN 3: User confirms

**Sarah**: "Yes, I understand. I've already booked another flight."

#### LLM AGENT: Establishes askable fact

```
fact_store.add_fact(
  user_confirms_cancellation_understanding(usr_sarah_chen)
)
```

Allowed: predicate is marked askable

#### LLM AGENT: Re-attempts refund

Tool call: `issue_refund(booking_of_interest, booking_amount_of_interest)`

#### GUARD EVALUATOR: Re-checking issue_refund guard

- Check 1: `eligible_for_refund(BK-2847, _, _)` -> PASS
- Check 2: `user_confirms_cancellation_understanding(current_user)` -> Now in fact store! PASS
- Check 3: `(892 < 500 ; has_supervisor_approval(BK-2847))`
  - **TOOL CALL**: `check_approval_queue({booking_id: "BK-2847"})`
  - Tool returns: `{approved: true, approver: "sup_jones", timestamp: "2025-01-15T14:32:00Z"}`
  - Supervisor approved! PASS

**GUARD RESULT**: `Allowed { trace: <full proof tree with all justifications> }`

#### TOOL EXECUTION: issue_refund("BK-2847", 892)

Tool returns:
```json
{
  "success": true,
  "refund_id": "REF-99281",
  "amount": 892,
  "method": "original_payment",
  "estimated_arrival": "3-5 business days"
}
```

#### TURN 4: Final response

**Agent**: "All done, Sarah! I've processed your full refund of $892.

- Refund reference: REF-99281
- Amount: $892.00
- Method: Back to your original payment method
- Expected arrival: 3-5 business days

Your refund was approved based on:
- Flight AL-445 delayed 6+ hours (airline disruption policy)
- Additionally protected under EU261 (departure from LHR)

Is there anything else I can help you with?"

### Audit Trail Generated

```yaml
refund_decision:
  booking_id: BK-2847
  outcome: approved
  amount: 892.00

  eligibility_proof:
    primary_path: airline_fault
    grounds:
      - rule: "Flight delay > 3 hours triggers full refund"
        facts:
          - flight_delay_minutes(AL-445, 360)
          - "360 > 180"
    secondary_grounds:
      - rule: "EU261 protection for delays > 5 hours from EU airports"
        facts:
          - flight_departure_airport(AL-445, "LHR")
          - is_eu_airport("LHR") = true
          - flight_delay_minutes(AL-445, 360)
          - "360 > 300"

  guard_satisfaction:
    - predicate: eligible_for_refund(BK-2847, full, airline_fault)
      status: proven
    - predicate: user_confirms_cancellation_understanding(usr_sarah_chen)
      status: established_via_user_confirmation
      timestamp: "2025-01-15T14:31:45Z"
    - predicate: has_supervisor_approval(BK-2847)
      status: proven
      approver: sup_jones
      timestamp: "2025-01-15T14:32:00Z"

  context_establishment:
    booking_of_interest:
      established_via: "user_selection"
      tool_call: get_user_bookings(usr_sarah_chen) @ 14:30:01
      candidates_presented: ["BK-2847 (LHR->JFK)", "BK-3012 (LHR->CDG)"]
      user_selected: "BK-2847"
      timestamp: "2025-01-15T14:30:15Z"

  tool_calls_made:
    - get_user_bookings(usr_sarah_chen) @ 14:30:01
    - get_booking(BK-2847) @ 14:30:02
    - get_flight_details(AL-445) @ 14:30:03
    - get_airport_info(LHR) @ 14:30:04
    - check_approval_queue(BK-2847) @ 14:30:05 -> false
    - check_approval_queue(BK-2847) @ 14:32:01 -> true
    - issue_refund(BK-2847, 892) @ 14:32:02
```

---

## Demo Scenario B: Refund Denied (Clear Explanation)

### Setup

- **User**: Marcus Webb
- **Booking**: BK-5521 for flight AL-892 ORD->LAX
- **Fare class**: Basic Economy ($289)
- **Flight status**: On time
- **Departure**: In 18 hours
- **Request**: Wants refund because plans changed

### Conversation Flow

#### TURN 1: User Request

**Marcus**: "I need to cancel my flight tomorrow and get a refund. My plans changed and I can't travel anymore."

#### SYSTEM: Fact Store Initialization

Pre-seeded facts (from authenticated session):
- `current_user = "usr_marcus_webb"`
- `current_time = "2025-01-15T16:00:00Z"`

Context slots (declared in rulebase, initially unbound):
- `booking_of_interest = null`

#### LLM AGENT: Identifies booking from user's description

User mentioned "flight tomorrow" - need to find matching booking.

**TOOL CALL**: `get_user_bookings({user_id: current_user})`

Tool returns:
```json
{
  "bookings": [
    {"booking_id": "BK-5521", "flight_id": "AL-892", "route": "ORD->LAX",
     "destination_city": "Los Angeles", "departure": "2025-01-16T10:00:00Z"}
  ]
}
```

LLM reasoning: User said "tomorrow" (Jan 16) -> likely matches BK-5521. Will attempt to query eligibility.

#### LLM AGENT: Formulates symbolic query

Query: `eligible_for_refund(booking_of_interest, RefundType, Reason)`

Note: Purely symbolic - context slot `booking_of_interest` is not yet bound.

#### SOLVER: Proof attempt #1 - BLOCKED on missing context

**Goal**: `eligible_for_refund(booking_of_interest, RefundType, Reason)`

**SOLVER RESULT**:
```
Blocked {
  missing_context: ["booking_of_interest"],
  reason: "Context slot 'booking_of_interest' is unbound",
  askable: true,
  candidates: [
    {"booking_id": "BK-5521", "description": "ORD→LAX (Los Angeles), Jan 16"}
  ]
}
```

#### LLM AGENT: Requests context establishment

Even with only one booking, the context must be explicitly established:

**CONTEXT REQUEST**: `establish_context(booking_of_interest)`
```json
{
  "context_slot": "booking_of_interest",
  "question": "Please confirm the booking you'd like to cancel:",
  "options": [
    {"value": "BK-5521", "label": "Flight to Los Angeles (ORD→LAX) - Jan 16, 2025"}
  ],
  "suggested": "BK-5521"
}
```

#### UI: Context Selection Form

A mini-form appears in the chat (single option, but still requires confirmation):

```
┌─────────────────────────────────────────────────────┐
│ Please confirm the booking you'd like to cancel:     │
│                                                       │
│  ◉ Flight to Los Angeles (ORD→LAX) - Jan 16, 2025   │
│                                                       │
│                              [Confirm]                │
└─────────────────────────────────────────────────────┘
```

#### USER: Confirms selection

Marcus clicks "Confirm".

#### SYSTEM: Context established

```
context_store.establish(
  booking_of_interest,
  "BK-5521",
  established_via: "user_selection",
  timestamp: "2025-01-15T16:00:10Z"
)
```

Context binding: `booking_of_interest <- "BK-5521"` (user-confirmed)

#### SOLVER: Proof attempt #2 - Context now available

**Goal**: `eligible_for_refund(BK-5521, RefundType, Reason)`

**PATH 1: airline_fault**

Try: `eligible_for_refund(B, full, "airline_fault") :- airline_at_fault(B)`
- Need to establish: `airline_at_fault(BK-5521)`
  - Try: `flight_status(F, cancelled)`
    - **TOOL CALL**: `get_booking({booking_id: "BK-5521"})`
      - Returns: `{flight_id: "AL-892", fare_class: "basic", ...}`
    - **TOOL CALL**: `get_flight_details({flight_id: "AL-892"})`
      - Returns: `{status: "on_time", delay_minutes: 0, ...}`
    - FAIL: status = "on_time", not "cancelled"
  - Try: `flight_delay_minutes(F, M), M > 180`
    - delay_minutes = 0
    - FAIL: 0 > 180 is FALSE

**airline_at_fault FAILED**

**PATH 2: eu261**

Try: `eligible_for_refund(B, full, "eu261") :- eu261_eligible(B)`
- Need: `is_eu_airport(departure_airport)`
  - **TOOL CALL**: `get_airport_info({code: "ORD"})`
  - Returns: `{is_eu: false, country: "USA", ...}`
  - FAIL: ORD is not an EU airport

**eu261_eligible FAILED**

**PATH 3: flexible_fare**

Try: `fare_allows_refund(B, full) :- booking_fare_class(B, "flexible")`
- `booking_fare_class(BK-5521, "basic")`
- FAIL: "basic" != "flexible"

**flexible_fare FAILED**

**PATH 4: standard_fare**

Try: `fare_allows_refund(B, partial) :- booking_fare_class(B, "standard"), hours_until_departure(B, H), H > 72`
- `booking_fare_class(BK-5521, "basic")`
- FAIL: "basic" != "standard"

**standard_fare FAILED**

**PATH 5: bereavement**

Try: `bereavement_eligible(B) :- user_claims_bereavement(current_user), user_confirms_upload_documentation(current_user, "death_certificate")`
- `user_claims_bereavement` is ASKABLE
- Not established - could ask user, but context suggests "plans changed" not bereavement

**bereavement BLOCKED** on askable - low relevance given context

**PATH 6: medical_basic (voucher)**

Try: `fare_allows_refund(B, voucher) :- booking_fare_class(B, "basic"), user_claims_medical_emergency(current_user), user_confirms_upload_documentation(current_user, "medical_cert")`
- `booking_fare_class(BK-5521, "basic")` -> PASS
- `user_claims_medical_emergency` is ASKABLE
- Not established - context says "plans changed"

**medical_basic BLOCKED** on askable - low relevance given context

**SOLVER RESULT**:
```
Failure {
  missing: [
    airline_at_fault(BK-5521),
    eu261_eligible(BK-5521),
    fare_allows_refund(BK-5521, full),
    fare_allows_refund(BK-5521, partial)
  ],
  askable_but_unlikely: [
    user_claims_bereavement(usr_marcus_webb),
    user_claims_medical_emergency(usr_marcus_webb)
  ],
  failure_explanations: [
    {path: "airline_fault", reason: "Flight AL-892 is on time with no delays"},
    {path: "eu261", reason: "Flight departs from ORD (Chicago), not an EU airport"},
    {path: "flexible_fare", reason: "Booking is Basic Economy, not Flexible fare"},
    {path: "standard_fare", reason: "Booking is Basic Economy, not Standard fare"},
    {path: "bereavement", reason: "Requires death certificate of immediate family member"},
    {path: "medical_basic", reason: "Could qualify for travel voucher with medical certificate"}
  ],
  trace: <full exploration tree>
}
```

#### TURN 2: LLM Response (with policy explanation)

**Agent**: "I understand plans can change unexpectedly, Marcus. Unfortunately, I'm not able to process a refund for this booking. Let me explain why:

Your booking BK-5521 is a Basic Economy fare on flight AL-892, which is currently on time. Here's what I checked:

- Airline disruption: Your flight is operating normally (no cancellation or significant delay)
- EU passenger rights: Doesn't apply - your flight departs from Chicago (ORD), not an EU airport
- Fare class: Basic Economy tickets are non-refundable for voluntary changes. Flexible ($+180) or Standard ($+90) fares would have allowed this.

There are two exceptions that *could* apply:

- Medical emergency: If you can provide a medical certificate, you'd qualify for a travel voucher (not cash refund)
- Bereavement: If this is due to a death in your immediate family, we offer a partial refund with documentation

Would either of these situations apply to you? Or would you like me to help you explore rebooking options instead?"

#### TURN 3: User provides more context

**Marcus**: "Actually, I hurt my back pretty badly playing basketball yesterday. My doctor said I shouldn't fly. Does that count?"

#### LLM AGENT: Establishes askable fact based on user statement

```
fact_store.add_fact(user_claims_medical_emergency(usr_marcus_webb))
```

Allowed: predicate is marked askable

LLM re-queries: `eligible_for_refund(booking_of_interest, RefundType, Reason)`

#### SOLVER: Re-attempt with new fact

**PATH 6 (revisited): medical_basic**

Try: `fare_allows_refund(B, voucher) :- booking_fare_class(B, "basic"), user_claims_medical_emergency(current_user), user_confirms_upload_documentation(current_user, "medical_cert")`
- `booking_fare_class(BK-5521, "basic")` -> PASS
- `user_claims_medical_emergency(usr_marcus_webb)` -> PASS (just established)
- `user_confirms_upload_documentation` is ASKABLE
- **BLOCKED** - need user to confirm they will upload documentation

**SOLVER RESULT**:
```
Failure {
  missing: [...previous paths still failed...],
  askable: [
    user_confirms_upload_documentation(usr_marcus_webb, "medical_cert")
  ],
  potential_outcomes: [
    {
      if_established: "user_confirms_upload_documentation",
      then: "eligible_for_refund(BK-5521, voucher, medical_basic)"
    }
  ]
}
```

#### TURN 4: LLM explains voucher option

**Agent**: "I'm sorry to hear about your back, Marcus. Yes, a medical issue documented by your doctor does qualify you for our medical emergency exception.

For Basic Economy bookings, this means you're eligible for a travel voucher for the full $289 value (valid for 1 year), rather than a cash refund.

To process this, I'll need you to upload a medical certificate or doctor's note confirming you're unfit to fly. Do you have that documentation available, or can you get it from your doctor?"

#### TURN 5: User decides not to proceed

**Marcus**: "Hmm, a voucher isn't really what I wanted. And getting a doctor's note is a hassle. Is there really no way to get actual money back?"

#### TURN 6: Final explanation

**Agent**: "I completely understand, and I wish I could offer more flexibility.

Unfortunately, Basic Economy fares are designed as our lowest-cost option specifically because they're non-refundable for voluntary cancellations. The only paths to a cash refund are:

- If the airline cancels or significantly delays your flight
- Bereavement (death of immediate family member)

The voucher is the best I can offer for a medical situation on this fare type.

A few alternatives to consider:

1. Keep the booking and rebook for a later date ($75 change fee)
2. Check if your travel insurance covers this
3. Some credit cards offer trip cancellation protection

Would you like me to check rebooking availability, or help with anything else?"

### Denial Audit Trail

```yaml
refund_decision:
  booking_id: BK-5521
  outcome: denied

  paths_evaluated:
    - path: airline_fault
      result: failed
      reason: "Flight AL-892 status is 'on_time', delay_minutes = 0"
      required: "status = 'cancelled' OR delay_minutes > 180"

    - path: eu261
      result: failed
      reason: "Departure airport ORD is_eu = false"
      required: "Departure from EU airport with delay > 300 minutes"

    - path: flexible_fare
      result: failed
      reason: "fare_class = 'basic', required 'flexible'"

    - path: standard_fare
      result: failed
      reason: "fare_class = 'basic', required 'standard'"

    - path: bereavement
      result: not_claimed
      reason: "User did not claim bereavement circumstances"
      would_require: "Death certificate of immediate family member"

    - path: medical_basic
      result: partial_match
      reason: "User claimed medical emergency but declined documentation"
      would_yield: "Travel voucher (not cash refund)"
      blocked_on: "user_confirms_upload_documentation"

  customer_context:
    stated_reason: "Plans changed"
    later_disclosed: "Back injury from basketball"

  available_options_offered:
    - "Travel voucher with medical documentation"
    - "Rebooking with $75 change fee"
    - "Third-party travel insurance claim"

  context_establishment:
    booking_of_interest:
      established_via: "user_selection"
      tool_call: get_user_bookings(usr_marcus_webb) @ 16:00:01
      candidates_presented: ["BK-5521 (ORD->LAX)"]
      user_selected: "BK-5521"
      timestamp: "2025-01-15T16:00:10Z"
      note: "Single candidate still required user confirmation"

  tool_calls_made:
    - get_user_bookings(usr_marcus_webb) @ 16:00:01
    - get_booking(BK-5521) @ 16:00:02
    - get_flight_details(AL-892) @ 16:00:03
    - get_airport_info(ORD) @ 16:00:04
```

---

## What This Demo Showcases

- **Explicit context establishment via user confirmation**
  - Context slots (like `booking_of_interest`) cannot be implicitly bound by the LLM
  - Solver returns `Blocked` with `missing_context` when unbound
  - LLM must issue `establish_context` request with options
  - UI presents a mini-form for user selection/confirmation
  - Even single-candidate contexts require explicit user confirmation
  - Scenario A: User selects from 2 bookings via form
  - Scenario B: User confirms single booking via form

- **Multi-layer guard checking**
  - Scenario A: Refund guard triggers approval check

- **Tool calls during guard eval**
  - Scenario A: `check_approval_queue` called

- **Askable predicates**
  - Scenario A: Cancellation confirmation
  - Scenario B: Medical claim, documentation

- **Returning to user**
  - Scenario A: For confirmation
  - Scenario B: For documentation request

- **Supervisor escalation**
  - Scenario A: Amount > $500 triggered

- **Purely symbolic queries**
  - Scenario A: `booking_of_interest` not literals
  - Scenario B: Same

- **Multiple eligibility paths**
  - Scenario A: airline_fault + EU261
  - Scenario B: All 6 paths explored

- **Clear denial explanation**
  - Scenario B: Each path explained

- **Audit trail**
  - Scenario A: Full proof trace
  - Scenario B: Full failure trace

- **Dynamic path discovery**
  - Scenario B: Medical path opened mid-conversation

- **Policy-to-outcome traceability**
  - Scenario A: EU261 rule cited
  - Scenario B: Basic Economy terms cited
