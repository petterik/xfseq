---
name: confidence
description: Build calibrated, evidence-backed confidence in an xfseq phase strategy by exposing assumptions, loopholes, failure modes, and the experiments needed to resolve them.
---

# Confidence

Do not claim certainty that the evidence cannot support. Performance requires
measurement; semantic equivalence requires tests and traces.

For each material decision, record:

- **Fact:** supported by current code, a test, bytecode, an official source, or
  a reproducible benchmark.
- **Assumption:** plausible but not yet demonstrated.
- **Unknown:** capable of changing the design or priority.
- **Failure mode:** how the strategy could appear to work while being wrong.
- **Resolution:** inspection, experiment, test, benchmark, or explicit user
  decision that closes the gap.
- **Confidence:** high, medium, or low, with a short reason.

Repeat the check after plan review. Planning is ready only when every
phase-critical unknown has a concrete resolution step before the decision that
depends on it. A fallback is acceptable for research uncertainty; it is not
evidence that a proposed core replacement succeeds.

Pay special attention to lazy realization, chunk consumption, completion,
`Reduced`, allocation, JIT warmup, direct-linking symmetry, benchmark setup,
and differences among source and sink shapes.
