# xfseq agent guide

## Mission

Prepare Implementation #1 for a credible Clojure core contribution. The seq
library is foundational. A replacement is useful only if it is simpler,
semantically equivalent, materially faster across a broad release-equivalent
matrix, and engineered to Clojure's unusually high quality bar.

The central hypothesis is that leaning further into transducers can simplify
the seq implementation and improve performance. A future `consume`/`drain`
fusion path is a possible consequence, not permission to change ordinary seq
semantics or expand #1.

## Source of truth

- `docs/01-transducer-backed-lazy-seqs.md`: #1 design and phase definitions.
- `docs/plans/01-phase-*.md`: one-phase execution plans and durable run state.
- `docs/02-primitive-specialized-pipelines.md`: later work; keep it out of #1
  unless explicitly requested.

Preserve the hand-written Java variants. Repair and benchmark them as production
candidates; the fastest correct variant is an internal baseline.

## Phase workflow

Work on one phase at a time with `$xfseq-phase`:

1. `plan` — `gpt-5.6-sol`, high reasoning. Apply confidence and prioritization,
   review the plan once or twice, then pass the pre-implementation
   `$xfseq-review` gate.
2. `run` — `gpt-5.6-sol`, medium reasoning orchestrator with sequential
   `luna_worker` agents. Never run workers concurrently.
3. `review` — `gpt-5.6-sol`, high reasoning. Perform the final semantic,
   performance, simplicity, and maintainability gate.

Stop at each stage and after each phase. Do not begin later-phase work
implicitly.

## Non-negotiable evidence

- Direct Clojure behavior is the semantic oracle.
- Performance claims require reproducible forked benchmarks and allocation
  evidence, not intuition.
- Direct-linking-on, symmetric builds decide release performance;
  direct-linking-off results are separate diagnostics.
- Do not hide a failed design behind flags, shims, special cases, generated
  machinery, or duplicated paths.
- Preserve user changes and record decisions, commands, versions, and raw result
  locations in the active phase plan.
