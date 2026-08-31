---
name: prioritize
description: Prioritize candidate work within one xfseq phase by Impact, Effort, and Value without letting easy implementation displace semantic or performance evidence.
---

# Prioritize

Create a compact table with these columns:

| Item | Impact | Effort | Value | Dependency/evidence | Decision |
|---|---|---|---|---|---|

Use high, medium, or low ratings and explain only non-obvious ratings.

- **Impact:** effect on semantics, broad performance, simplicity, or upstream
  acceptability.
- **Effort:** uncertainty, design complexity, API consequences, benchmarking,
  and review burden. Do not estimate primarily from lines of code.
- **Value:** expected improvement and how much important later work it unlocks.
- **Decision:** now, later phase, experiment only, or reject.

Correctness gates and measurements needed to make a decision outrank convenient
implementation. Keep prioritization inside the selected phase; record tempting
out-of-scope ideas without pulling them forward.
