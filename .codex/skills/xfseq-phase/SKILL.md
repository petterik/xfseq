---
name: xfseq-phase
description: Plan, run, or finally review exactly one phase of xfseq Implementation #1 using the project's evidence-first Clojure-core contribution workflow.
---

# xfseq Phase Workflow

Work on exactly one phase from
[`docs/01-transducer-backed-lazy-seqs.md`](../../../docs/01-transducer-backed-lazy-seqs.md).
Planning, implementation, and final review are separate sessions because they
use different model and reasoning setups.

## Invocation

Use one mode:

```text
$xfseq-phase plan <phase number or name>
$xfseq-phase run <phase-plan path>
$xfseq-phase review <phase-plan path>
```

If the phase or plan is ambiguous, ask one concise question. Never advance into
the next phase without a new invocation.

## Plan mode

Expected setup: `gpt-5.6-sol`, high reasoning. Do not implement production code.

Read these sibling skills completely and apply them in order:

1. [`approximate`](../approximate/SKILL.md) — reconstruct the real problem.
2. [`confidence`](../confidence/SKILL.md) — expose facts, assumptions, unknowns,
   failure modes, and decisive evidence.
3. [`prioritize`](../prioritize/SKILL.md) — decide what belongs in this phase.
4. [`review-plan`](../review-plan/SKILL.md) — review once, then again after any
   material redesign or unresolved phase-critical uncertainty.
5. [`xfseq-review`](../xfseq-review/SKILL.md)
   — perform the pre-implementation gate.
6. [`plain-english`](../plain-english/SKILL.md) — summarize what matters.

Evaluate the problem and options from at least these points of view: a Clojure
core maintainer, a library user relying on seq semantics, a performance/JVM
engineer, and the future contributor who must explain and maintain the patch.

Create or update one durable plan under:

```text
docs/plans/01-phase-<number>-<short-name>.md
```

The plan must contain:

- status and link to the parent #1 design;
- plain-English problem statement;
- phase goal, non-goals, and upstream relevance;
- current repository facts and baseline behavior;
- options and trade-offs, including the simplest viable option;
- Impact/Effort/Value priorities;
- confidence ledger and decisive experiments;
- ordered implementation slices with ownership boundaries;
- semantic validation and performance methodology;
- direct-linking mode and exact runtime matrix where performance is involved;
- exit criteria, decision log, validation evidence, and agent run log.

Accept and incorporate every applicable review finding. If two findings
conflict, or repository evidence contradicts one, keep the plan in draft and ask
the user to resolve the named conflict. The pre-implementation `$xfseq-review`
verdict must be `ready for implementation` before status becomes `Ready for
implementation`.

## Run mode

Expected orchestrator setup: `gpt-5.6-sol`, medium reasoning. Read and follow
[`run-plan`](../run-plan/SKILL.md). Use sequential `luna_worker` agents. After
the parent reviews and accepts each worker's slice, commit that slice and
record its SHA before starting the next worker; do not push automatically.
End with the selected plan marked `Awaiting final review`, not complete.

## Review mode

Expected setup: `gpt-5.6-sol`, high reasoning.

Read the parent design, original phase plan, complete diff, tests, benchmark
harness, and raw results. Apply
[`xfseq-review`](../xfseq-review/SKILL.md)
as the final gate, plus a direct audit of every phase exit criterion.

Fix small in-scope defects and rerun validation. If a finding requires material
redesign, mark the plan `Needs replanning`; do not patch around it. Mark the
phase `Complete` only when all semantic, simplicity, build, and performance
criteria required by that phase have evidence. Finish with a short summary
using [`plain-english`](../plain-english/SKILL.md).

Stop after this phase.
