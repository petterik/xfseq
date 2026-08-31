---
name: approximate
description: Reconstruct the real goal behind approximate requirements before planning an xfseq phase, especially when literal instructions conflict with simplicity, performance, semantics, or upstream quality.
---

# Approximate Requirements

Treat user prose as directional evidence, not an excuse for literal but broken
implementation.

Before planning:

1. State the actual problem in one sentence.
2. Separate desired outcomes from suggested mechanisms.
3. Check the suggestions against the repository, Clojure semantics, and the
   intended upstream contribution.
4. Surface contradictions and missing decisions instead of hiding them in a
   flag, shim, special case, or second implementation path.
5. Re-derive the design from first principles when an assumption fails.

For xfseq, the invariant is: a core candidate must be semantically equivalent,
materially faster over a broad release-equivalent benchmark matrix, and simpler
enough to meet Clojure's quality bar. If those goals conflict, describe the
trade-off plainly and resolve it in the phase plan before coding.

Do not use this skill to expand the selected phase.
