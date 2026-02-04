# Sentinel

_MCP guardrailing for LLM agents using logic programming_

Sentinel sits between an LLM agent and its tools/MCP, enforcing business rules
through a logic solver. Instead of hoping the LLM interprets policy documents
correctly, Sentinel uses a solver to generate proofs that tool actions are
permitted, according to a policy. If they are not, the LLM agent is guided
towards a solution.

## The Problem

LLMs can't be deployed in regulated rule-based environments without better
reasoning skills. When [Air Canada's chatbot invented a refund
policy](https://arstechnica.com/tech-policy/2024/02/air-canada-must-honor-refund-policy-invented-by-airlines-chatbot/)
and the airline was held legally liable, it demonstrated a fundamental gap: LLMs
are probabilistic text generators, but business rules require deterministic
enforcement. No amount of prompt engineering can guarantee an LLM won't
hallucinate a policy that doesn't exist.

## How Sentinel Works

```
User ←→ LLM Agent ←→ Sentinel ←→ Tools (MCP)
                           │
                      ┌────┴────┐
                      │  Solver,│
                      │  Facts, │
                      │  Guards │
                      └─────────┘
```

- Rules are expressed as composable predicates with backtracking proof search,
  not natural language in a prompt.
- When the agent wants to offer refund advice or issue a refund, it first checks with Sentinel. The solver recursively calls other MCP tools to gather the data needed to make a decision. If the action is permitted, a proof is produced (and explained to the user if relevant). Otherwise a set of _blockers_ is returned, guiding the LLM towards a solution.
- Facts come from tool calls or explicit user confirmation ("askable" predicates). The LLM formulates symbolic queries, it never injects literal values into the reasoning chain.

## Policy Language

Policies are currently specified using an embedded DSL (eDSL) in Haskell. This
gives full access to Haskell's type system and tooling, but ties policy authoring
to the host language.

We plan to develop a standalone policy language, so that policies can be written
and audited independently of the Haskell codebase.

We also plan to support calling out to third-party solvers — such as
[z3](https://github.com/Z3Prover/z3),
[Clingo](https://potassco.org/clingo/), and
[s(CASP)](https://utdallas.edu/~gupta/scasp/) — enabling richer reasoning
(e.g. arithmetic constraints, answer-set programming, abductive reasoning) where
the built-in proof search is not sufficient.

## Examples

So far we have developed two examples:
- AirCanada refund logic.
- UK passport application logic (based on [this blog post](https://jameshaydon.github.io/passport/)).

More on the way!

## Getting Started

### Prerequisites

- [Nix](https://nixos.org/) (recommended) or GHC 9.12 + cabal
- An OpenAI API key (set `OPENAI_API_KEY` environment variable)

### Build

```bash
# With Nix (recommended)
nix develop          # Enter dev shell
just build           # Build the project

# Or directly
nix build            # Build the Nix package
```

### Run

```bash
# Air Canada refund policy demo
cabal run repl -- --example aircanada --user usr_james_doe

# AirLogic airlines demo
cabal run repl -- --example airlogic --user usr_sarah_chen

# UK passport eligibility demo
cabal run repl -- --example passport --user james

# See all options
cabal run repl -- --help
```

### Try It

In the Air Canada example, try asking:

> "I'd like a refund for my delayed flight."

The agent will fetch your bookings, establish context, run the solver to check
eligibility, and either process the refund (with guard checks) or explain
exactly which policy paths were explored and why they failed.
