# Sentinel Architecture

## Vocabulary & Terminology

- *LLM Agent*: An autonomous system built around a LLM that can perceive, reason, and act to achieve goals. In Sentinel, this is the conversational component that interprets user intent and orchestrates governance checks.

- *Turn*: A single interaction cycle: user input → agent processing → agent response. One complete exchange in the conversation; may involve multiple solver invocations and tool calls.

- *Tool/Function Calling*: LLM's ability to invoke external functions/APIs with structured arguments. How the agent requests data retrieval or performs actions (via MCP).

- *MCP (Model Context Protocol)*: Standardized protocol for connecting LLMs to external data sources and tools. (Like OpenAPI/Swagger, but specialised to tool usage, no HTTP verbs, etc.)

- *Prompt Context*: Information provided to the LLM with each request to guide its behavior. Includes fact store summary, available bindings, conversation history, and RAG from natural language policy documents.

- *Hallucination*: When an LLM generates plausible-sounding but factually incorrect information. Prevented by symbolic-only queries and tool-based fact establishment.

- *Rulebase*: The compiled formal policy: rules, guards, bindings, and annotations.

- *Fact Store*: Working memory of established ground predicates for the session.

- *Askable Predicate*: A predicate representing user intent/confirmation, establishable by asking the user.

- *Guard*: A precondition that must hold before a tool can be invoked.

- *Context*: A named rebindable reference declared in the rulebase that tracks conversational focus. Examples: `booking_of_interest`, `flight_of_interest`.

- *Fact TTL (Time To Live)*: How long a fact remains valid before requiring refresh. Examples: flight status (60s), user tier (session-scoped).

- *Focus Shift*: When conversational attention moves to a different entity, triggering context slot rebinding. Example: "What about my *other* booking?"

- *Solver Block*: When the solver cannot proceed without additional information. Either missing tool data (call tool) or missing confirmation (ask user).

- *Guard Violation*: When a tool call attempt fails precondition check. Example: trying to refund a booking for a non-cancelled flight.

## Overview

The system acts as a **governance middleware** that sits between an LLM agent
and its tools (via MCP). It enforces formally verified business rules by
controlling what facts the agent can establish and what actions it can take. The LLM agent is also made aware of currently established facts, and can query sentinel directly (non-tool use), just for information purposes.

## Initial Inputs

1. **Policy Documents**: Natural language business rules, terms & conditions, compliance requirements
2. **MCP Tool Metadata**: Schema of available tools (JSON-in, JSON-out with schemas):
   - **Data tools**: Retrieve facts (e.g., `get_flight_details(flight_id) → {status, departure_time, ...}`)
   - **Action tools**: Perform effects (e.g., `cancel_booking(booking_id) → {success, cancellation_id}`)

## Authored (but also generated)

**Rulebase**: A formal policy language, specifying:
- **Rules** for inferring derived predicates/values (e.g., `eligible_for_refund(User, Booking) :- ...`)
- **Tool bindings**: How base facts map to tool calls, with JSON extraction
- **Guards**: Preconditions on each tool
- **Askability annotations**: Which predicates can be established by asking the user
- **Context variable declarations**: Rebindable references like `booking_of_interest`, similar to Catala scope?
- **TTL annotations**: Cache validity for tool base facts.

## Runtime Components (Open Source)

### Fact Store

A database of ground predicates established during the session.

- Context-independent facts, with TTL. E.g. `booking_cancelled(ref123)`, `user_id(abc123)`.
- Context, e.g. _booking of interest_ (corresponds to a "scope" in Catala)

Context variables are declared in the rulebase. They solve the problem of "which booking are we talking about?" while keeping queries symbolic.

- The LLM can rebind these when conversational focus shifts (e.g., "actually, what about my other booking?")
- Queries remain purely symbolic: `can_refund(current_user, booking_of_interest)`

Facts derived from tools calls carry validity metadata. When the solver needs a fact:
1. Check store, if present and TTL valid, use it
2. If expired or missing, invoke bound tool to re-establish

The fact store also has methods for summarising the current state in text format for the LLM context.

The fact store is seeded when the agent inits, e.g. with the logged in user.

### Solver

A **stateless backtracking/concurrent solver** that proves goals against rulebase + fact store + tool-calls.

- Derives facts: Uses rules to prove derived predicates from base facts.
- Triggers tool calls: When proof requires a base fact not in store (or expired), invokes bound tool. Fact store is updated.
- Identifies askable gaps: When blocked on askable predicate (after exhausting tools), reports it.
- Returns trace:
  * On success: derivation.
  * On failure: what could unblock
- Should explore several branches concurrently, especially if they involve tool use for establishing base facts.

- Stateless across invocations: returns complete result, no continuation preserved between turns.
- Fast (apart from tool I/O): re-running is cheap

### Guard Evaluator

Sentinel sits between the LLM agent and the MCP server. Any tool calls are intercepted and guarded. The guards recursively trigger data-fetching tools (also guarded).

## Integration with LLM Agent Loop

The main LLM agent:
- Receives `fact_store.summarize_for_llm()` in context
- Receives natural language policy via RAG.
- Formulates symbolic queries using available bindings
- Detects focus shifts → updates context variables
- Handles askable facts (asks user, adds confirmations)
- Re-invokes solver as needed
- Invokes tools (guarded)

Sentinel:
- MCP middleware, intercepts and guards any tool calls
- Adds facts to the database, after any tool call
- Responds to queries

## Example: Focus Shift Mid-Conversation

- **Step 1: System:** Init: `current_user(u_123)`, `today(2025-01-15)`
- **Step 2: User:** "Check my booking BK-789"
- **Step 3: LLM:** Fetches user's bookings via tool, sets `booking_of_interest → bk_789`
- **Step 4: LLM:** Query: `booking_status(booking_of_interest, Status)` → confirmed
- **Step 5: User:** "Actually, what about my other booking?"
- **Step 6: LLM:** Detects focus shift, rebinds: `booking_of_interest → bk_456`
- **Step 7: LLM:** Same symbolic query: `booking_status(booking_of_interest, Status)` → now resolves to BK-456

## Remarks

- A derived fact/value that depends on base predicates that have become invalid (TTL expired) are also invalidated.
- **All queries are symbolic**: This ensures the query is meaningful, depends only on established facts and context, and prevents the use of hallucinated inputs.
- **All tool calls are guarded**: Including solver's own tool calls during proof. Note that direct tool calls by the LLM may be allowed (and contain literals), depending on the registered guard. In a stricted mode, all tool calls must go through sentinel, and be purely symbolic.
- **Solver is stateless**: At the end of a turn, the solver does not store a continuation, even if it returns with "ask this askable datum from the user". Indeed the conversation may just take another path completely. If the user does provide the data, and this is added to the fact store, it is the LLM agent's role to re-invoke the solver (via re-invoking the tool, for example). The solver will not be blocked (or blocked differently) the second time around.
- **Tools before questions**: The solver should exhaust tool usage in its backtracking, before resorting to asking questions of the user.
