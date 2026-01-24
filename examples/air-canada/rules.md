# Refund rules

## 24-Hour Cancellation

`full_refund` and `no_penalty` if:
- Purchased through Air Canada
- Cancel within 24 hours of purchase
- Have not started travel

## Legal or Military Duty

`legal_duty_refund` if:
- Purchased through Air Canada
- passenger is called to military duty, jury duty, OR is subpoenaed
- passenger presents documentation (exhaustive list)
- the duty conflicts with scheduled travel

`full_refund` and `no_penalty` if:
- `legal_duty_refund`
- refund request is made within ticket validity period (per A.3.a)

`full_refund` if:
- `legal_duty_refund`
- applicable over-aged fee is paid (TODO)

## Bereavement Rule

`immediate_family_member(p)` if:
  ... (exhaustive list)

`bereavement_refund` if:
- `deceased(p)` and (`immediate_family(p)` or `travelling_companion(p)` or `passenger(p)`)
- provide documentation for `deceased(p)`, within 90 days of the return date
- death occurs after ticket purchase
- Purchased through Air Canada

`full_refund` and `no_penalty` if:
- `bereavement_refund`
- death occurs before travel commences

`partial_refund` and `no_penalty` if:
- `bereavement_refund`
- travel already commenced

## Schedule Irregularity (delay or cancellation) or Involuntary Denied Boarding

`schedule_irregularity` if any of:
- flight delayed or canceled caused by the airline (e.g. aircraft maintenance, no crew, etc.)
- Required for Safety: A delay or cancellation required by law or for safety (e.g., mechanical safety issues)
- Outside Air Canada's Control: A delay or cancellation caused by external factors (e.g., weather, air traffic control, strikes, force majeure).
- `involuntary_denied_boarding`: You are denied boarding due to overselling or aircraft substitution, despite having a confirmed reservation and checking in on time.

`involuntary_refund` if all of:
- `schedule_irregularity`
- You refuse the alternate travel arrangements offered by Air Canada because they "do not accommodate [your] travel needs" and choose to "no longer travel"
- You communicate this to Air Canada _before_ the start of the _alternative_ travel.

`full_refund` if:
- `involuntary_refund`
- no part of the fare has been traveled

`full_refund` and `reservation_back_to_origin` if:
- `involuntary_refund`
- at a connection point (partially flown)
- the travel "no longer serves a purpose"

`partial_refund` (prorated based on mileage) if:
- `involuntary_refund`
- at a connection point (partially flown)
- continue travel by other means (*not* arranged by Air Canada)
