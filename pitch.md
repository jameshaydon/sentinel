# Policy as code: Enforcement for Agentic Systems

# Executive Summary

We provide a correct-by-construction control layer for Large Language Models (LLMs) in enterprise environments. We bridge the gap between probabilistic token generation and deterministic business logic by introducing a logic-programming DSL (Domain Specific Language) that serves as a governance engine for agentic systems.

# The Core Problem

Current model alignment techniques broadly fail to provide soundness guarantees for specific business logic (e.g., the [Air Canada hallucinated refund policy)](https://arstechnica.com/tech-policy/2024/02/air-canada-must-honor-refund-policy-invented-by-airlines-chatbot/). Enterprises cannot deploy autonomous agents if they rely on probabilistic inference to execute in environments with strict contractual or regulatory rules.

# The Solution: A Formal Verification DSL

We are building a logic-based DSL (inspired by Prolog, s(CASP) and Catala) designed to express business constraints as executable formal logic.

* Isomorphic Policy Authoring: like Catala, the language syntax mirrors legal/policy text structures, enabling LLM-assisted formalization. This maintains a verified “chain of custody” where incremental changes in natural language policy map directly to updates in the rulebase. This language allows for companies to maintain policy as code. We call this a “rulebase”.  
* The Logic Solver: Unlike standard RAG (Retrieval-Augmented Generation), our engine does not rely only on feeding natural language rules into the context window for interpretation. Instead, the LLM interacts with the outside world while also submitting abstract queries to our logic solver, for any action it may want to perform, or data it may want access to.  
  * Input: The LLM proposes an intent (e.g., “offer refund”).  
  * Verification: The solver checks satisfiability against the formal rule set. If satisfied, returns an audit trace.  
  * Rejection Handling: If the intent violates policy, the solver returns a counter-factual trace or a set of sub-goals (e.g., "Missing precondition: flight\_cancelled or bereavement\_proof").  
* Hallucination firewall: The solver rejects literal value injection. The LLM cannot assert “flight.time \= 08:00"; it must pass a reference to a trusted oracle (e.g., a database capability via MCP).

# Architecture: Trusted MCP Governance

We position our engine as a middleware interceptor for the [Model Context Protocol](https://modelcontextprotocol.io) (MCP).

* Capability Guarding: Every tool call (database read, API write) is guarded by a logical predicate.  
* Stateful Logic: The solver maintains the ground truth. The LLM is restricted to the role of a semantic interface and planner, stripped of the authority to establish facts, or execute effects.  
* Static Analysis: We run static analysis at compile time to detect conflicting policies or logical unreachable states before deployment.

# Business strategy

**Free and open source core.** Developers download the CLI, write rules locally, integrate the solver into their agent (using SDKs). This builds the ecosystem, gets developer buy-in.

**Commercial tier: “Policy Ops”.** Customer service/Terms and Conditions/business teams use the web dashboard to create policies from scratch (from natural language documents), collaborate on and refine policies using our “isomorphic editor”. They set-up and run simulations and test suites.

There are several components to this:

* A web-based interface to create rulebases from scratch, using natural language documents. This acts as a versioned database of all the rules in effect, tracing them to the original natural language policy documents. Rulebases can be maintained by the business domain experts (non-coders, e.g. customer service team, legal team, etc.), and exported for the production system of the company. This web app allows one to test the policy, create dummy scenarios, etc.  
* Meta-level verification. Specialised formal methods tools which continuously check and prove meta-properties about the ruleset. E.g.  
  * The ruleset is in compliance with some EU regulations.  
  * The ruleset never issues refunds of more than 500USD without human approval.  
  * No redundant rules.  
  * The ruleset doesn’t have contradictions, and doesn't get stuck. E.g. finding conflicts such as:  
    * “VIP customers get instant refunds for items under 500USD”  
    * “There are never any refunds over 300USD without human agent approval”  
  * Why it’s needed: the rule-language is not as generally expressive as the “meta property” language, because it has to be executable/tractable during agent interaction.  
* Change Impact Analysis, analytics: given some proposed update to the ruleset, assess impact:  
  * Outcomes of previous conversations that change (E.g. “30% of automated refunds in the past year would not have been allowed”)  
  * This change would violate meta-policy “Fraud Check Level 2” (ties into above).  
* Agent host. For those smaller or less technical businesses which want to deploy externally facing agents, we offer hosting of the agents and the solver.

# Market Analysis

## Market Size

The global AI guardrails market size is estimated between 0.7B to 2.4B in 2024, and expected to rise to 15B-100B in 2033\.

[Market.us](https://market.us/report/ai-guardrails-market/):  
“The Global AI Guardrails Market size is expected to be [worth around USD 109.9 Billion by 2034](https://market.us/report/ai-guardrails-market/), from USD 0.7 Billion in 2024, growing at a CAGR of 65.8% during the forecast period from 2025 to 2034.”  
[Dataintelo.com](https://dataintelo.com/report/ai-guardrails-market):  
“According to our latest research, the global AI Guardrails market size reached USD 2.4 billion in 2024, reflecting the rapid adoption of responsible AI solutions across industries. The market is expected to expand at a robust CAGR of 21.7% from 2025 to 2033, positioning it to achieve a value of USD 17.2 billion by 2033\. This impressive growth is primarily driven by the escalating need for ethical, safe, and compliant AI systems in critical sectors such as finance, healthcare, and government, where regulatory scrutiny and the risk of reputational damage are particularly high.”  
[www.precedenceresearch.com](https://www.precedenceresearch.com/ai-governance-market)  
“The global AI governance market size is valued at USD 309.01 million in 2025 and is predicted to increase from USD 419.45 million in 2026 to approximately USD 4,834.44 million by 2034, expanding at a CAGR of 35.74% from 2025 to 2034\. The market growth is attributed to the rising adoption of AI across various industries and the need for transparent, accountable decision-making frameworks.”

The need for AI guardrailing split quite cleanly into probabilistic and non-probabilistic categories. A deployed system will need one or the other, or both, for different reasons. For example, for an AI system that deals with decision regarding human, guardrailing is desirable (and required by the EU AI Act) for:

* E.g. avoiding racial bias in AI-driven hiring decision-making (probabilistic)  
* Making sure all proper procedures are followed (non-probabilistic)

These are two fundamentally different problems, and we tackle only the non-probabilistic case. Some reports have more precise numbers for “regulatory and legal / rule-based AI guardrails”.  
(TODO: buy the report)

The pressure to use solutions such as ours comes from:

* Companies unable to deploy LLMs in environments where mistakes can be expensive,  
* Regulatory pressure, e.g. EU AI Act.

## Competitive Landscape

Most AI Guardrail systems are probabilistic in nature, and don’t offer the hard guarantees and determinism needed to operate in a regulated environment. This analysis is focused on direct competition that uses deterministic systems.

### [NVIDIA NeMo Guardrails](https://github.com/NVIDIA-NeMo/Guardrails)

Despite marketing as "programmable" and "rule-based," NeMo Guardrails is fundamentally a framework system that relies heavily on model inference rather than formal reasoning. Its main utility is for keeping conversations on-topic, content-moderation, jailbreak detection, etc. Its fact-checking is explicitly probabilistic: retrieval-grounded validation asks whether claims are "supported by" retrieved documents, and LLM-as-judge patterns use a secondary model to evaluate factuality. NeMo remains in beta with official warnings against production deployment, though enterprises including Amdocs, Cerence AI, and Lowe's are experimenting with it. Open-sourced with 5K+ GitHub stars, it serves the probabilistic guardrailing use case well but does not compete in environments requiring deterministic policy enforcement or formal verification.

### [Amazon Bedrock Automated Reasoning](https://docs.aws.amazon.com/bedrock/latest/userguide/guardrails-automated-reasoning-checks.html) (ABAR)

ABAR represents the most advanced production deployment of formal verification of AI guardrails. This service by amazon aims to check LLM generated responses using SMT solvers (satisfiability modulo theories, e.g. Z3). We believe ABAR has been designed for decision-making in domains such as issuing mortgages or insurance policies, where the structure of the rules is mostly a flat list of constraints. We think it is unsuitable for larger structured/nested policy documents.

* SMT does not scale to very large complex policies. They acknowledge this:  
  * Variable limits: Policies with excessive numbers of variables or overly complex rule interactions may hit processing limits or return TOO\_COMPLEX results.  
  * While Z3 can be suitable for some domains, we believe bureaucratic regulations, policies and law are not well handled by SMT type tools. This is the raison d’etre of [Catala](https://catala-lang.org/) for example, or why [Blawx](https://law.mit.edu/pub/blawxrulesascodedemonstration/release/1) uses s(CASP).  
* We believe the architecture we have come up with, guarding MCP, is more flexible. The solver can interactively request more information, whereas ABAR needs everything upfront.  
* No exposed policy language. While we also aim for the commercial tier to generate rules automatically from natural language policy documents, this is primarily to solve the “blank page” problem. The formal language is then exposed to the policy domain experts via the isomorphic editor. Rules are stabilised via a back and forth between natural and formal language. We don’t believe large complex policies, that are critically important for correct agent decision-making, can rely entirely on automated interpretation of natural language documents.

### [Invariantlabs](https://invariantlabs.ai/blog/guardrails)

Invariant Labs, acquired by Snyk in 2025, is a direct competitor in the MCP guardrailing space. Their system provides rule-based constraints on LLM tool calls via a transparent proxy, architecturally similar to our approach. But they are targeting a fundamentally different space: security engineering vs policy-driven decision-making.

Invariant's guardrailing language operates on \*traces\*: sequences of events (messages, tool calls, tool outputs) generated during agent execution. Rules follow a \`raise "error" if:\` pattern, where conditions match events and their properties. For example, \`(call: ToolCall)\` binds a variable to each tool call, and \`call is tool:send\_email({ to: "alice@mail.com" })\` matches specific invocations with regex-capable parameter matching. The language includes a flow operator (\`-\>\`) to express ordering constraints between events, enabling rules like "block email sends if the agent previously ingested a prompt-injected message." Built-in detectors for PII, secrets, prompt injection, and content moderation can be imported and composed with pattern rules. It's a well-designed DSL for security engineers, expressive enough for dataflow constraints and ML-augmented detection.

* Invariant is fundamentally built for security engineers to detect violations. Rules pattern-match event traces after the LLM attempts an action, blocking bad behavior but providing no guidance toward valid alternatives. By contrast, Rulebase is a \*policy reasoning engine\*. Our logic solver proves satisfiability before actions and returns counter-factual traces on failure ("Missing precondition: flight\_cancelled OR bereavement\_proof"), enabling agents to resolve blockers rather than simply fail.  
* Invariant evaluates rules at runtime only. Contradictory policies (e.g., "VIPs get instant refunds under $500" vs. "No refunds over $300 without approval") can coexist silently until they cause production failures. Rulebase performs static analysis at compile time, detecting logical contradictions, unreachable states, and meta-policy violations before deployment.  
* On the authoring side, Invariant rules are code written by developers. There's no tooling for business or legal teams to manage policies. Our isomorphic editor maintains bidirectional traceability between natural language documents and formal rules, enabling domain experts to author, review, and run impact analysis on policy changes.

The Snyk acquisition validates enterprise demand for deterministic AI guardrails, but Snyk's core business is developer security tooling. We expect Invariant to remain positioned for engineering teams rather than evolving toward regulatory compliance workflows or business-user policy governance. This leaves our target enterprise segment underserved. In short: Invariant blocks bad actions for security teams. We unblock decision-making capabilities for legal, compliance, and business teams managing policy at scale.

### [Galini.ai](http://Galini.ai)

TODO  
YC cohort 2024

Most direct competitor in terms of the problem they are trying to solve.

Unclear what their underlying tech /idea is. The founders don't have background in formal methods, languages, logic, so I'm guessing the approach is quite different.

# Team

(Some as advisors)

* James Haydon: PhD mathematics.  
* Arnaud Spiwack: PhD computer science.  
* Jeremy Dubut: PhD Computer Science  
* Ichiro Hasuo: PhD Computer Science