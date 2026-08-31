---
name: run-plan
description: Execute exactly one reviewed xfseq phase plan through sequential Luna workers, with parent integration checks and durable handoff state.
---

# Run One Phase Plan

Use this in the implementation session after `$xfseq-phase plan` has marked one
phase plan `Ready for implementation`.

Expected setup:

- orchestrator: `gpt-5.6-sol`, medium reasoning;
- workers: `luna_worker` agents;
- concurrency: exactly one worker at a time.

Invoking this skill explicitly authorizes sequential sub-agents for the selected
phase. It does not authorize work from later phases.

## Preflight

1. Read root `AGENTS.md`, the complete #1 design, and the complete selected
   phase plan.
2. Read every source, test, or predecessor path linked by the plan.
3. Check `git status --short`; preserve pre-existing user changes.
4. Confirm the plan names one phase, has passed plan review and the
   pre-implementation `$xfseq-review` gate, and has executable exit criteria.
5. Capture baseline validation and performance data required by the plan before
   changing the implementation.

If these conditions are not met, stop and return the plan to planning. Do not
invent missing design during implementation.

## Sequential worker loop

Spawn one `luna_worker` for the next coherent slice. Tell every worker:

- its exact file/responsibility ownership;
- it is not alone in the repository and must preserve other changes;
- it must not spawn sub-agents;
- the selected phase and plan are its complete scope;
- correctness, simplicity, and performance evidence are all deliverables;
- it must update the phase plan's run log and validation evidence;
- it must stop at a natural checkpoint or a genuine blocker.

After each worker returns, the orchestrator must inspect the diff, run the
relevant validation, reconcile the plan, and fix or reject the slice before
starting another worker. Once that parent review passes, commit the accepted
worker slice before spawning the next worker. The commit must contain the
worker's accepted code, evidence, and plan updates only; preserve unrelated
pre-existing changes, record the commit SHA in the phase run log, and do not
push it automatically. If the slice is rejected or blocked, do not create the
checkpoint commit until the issue is resolved or the phase is returned to
planning. Never run workers concurrently, even on disjoint files.

Use as many sequential workers as the phase naturally needs. Do not create work
just to use another worker.

## Durable handoff

The phase plan is the source of truth. Maintain:

- task status;
- decisions and reasons;
- commands and exact results;
- benchmark environment and raw-result locations;
- unresolved questions and blockers;
- a compact chronological run log.

Do not turn it into a chat transcript. Check off work only when its code,
correctness evidence, performance evidence, and documentation are complete.

## End of run stage

Run the phase validation suite and compare the implementation with both direct
Clojure core and the fastest correct retained Java candidate. Mark the plan
`Awaiting final review`; do not mark the phase complete. The separate
`gpt-5.6-sol` high-reasoning review stage owns final acceptance.

Stop after this phase. Do not begin the next phase automatically.
