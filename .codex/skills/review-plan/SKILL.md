---
name: review-plan
description: Review one xfseq implementation phase against the current repository, Clojure semantics, performance goals, and upstream constraints before implementation.
---

# Review One Phase Plan

Read the complete #1 design document, the selected phase plan, and every source
or test file on which that phase relies. Review with fresh eyes.

Start with the problem statement. A technically coherent solution to the wrong
problem is a failed plan. Then look for:

- assumptions contradicted by the current code or Clojure implementation;
- simpler designs that remove branches, classes, states, or mechanisms;
- semantic gaps in laziness, chunking, completion, reduction, concurrency, or
  ordinary sequence behavior;
- benchmark asymmetry, missing source/workload/sink cases, or claims that cannot
  be measured;
- work pulled in from later phases without a prerequisite reason;
- choices that make an eventual Clojure core patch harder to review or accept.

Report:

1. **Uncertain decisions:** numbered by impact, with options, trade-offs, a
   recommendation, and the evidence needed to decide.
2. **Confident changes:** lettered, brief, and directly actionable.

Use plain English. As part of `$xfseq-phase plan`, accept every applicable
finding and update the plan. If findings are mutually exclusive or one is
factually contradicted by repository evidence, do not choose silently: keep the
plan in draft and surface the conflict to the user. No finding may disappear.
Run a second review when the first causes material redesign or leaves a
phase-critical uncertainty.
