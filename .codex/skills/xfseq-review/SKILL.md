---
name: xfseq-review
description: Perform an exceptionally strict pre-implementation or final review of an xfseq phase for simplicity, semantic fidelity, benchmark validity, and Clojure-core-level maintainability.
---

# xfseq Review

Use this only when explicitly requested or when the `$xfseq-phase` workflow
reaches its pre-implementation or final gate.

The review is adversarial toward complexity, not toward people. Passing tests is
necessary but insufficient. Search for a design that feels inevitable in
hindsight and delete incidental mechanisms where possible.

## Review order

1. **Problem validity:** Does the phase solve a necessary part of the stated
   problem, or only make the experiment more elaborate?
2. **Semantic fidelity:** Are values, exceptions, laziness, realization order,
   chunk consumption, completion, `Reduced`, metadata, sequence protocols,
   concurrency, and memory retention equivalent where claimed?
3. **Performance validity:** Are comparisons symmetric, forked, warmed up, and
   allocation-aware? Is direct linking equivalent on both sides? Do sources,
   sizes, sinks, selectivity, pipeline depth, JVMs, and candidate Java loops
   cover plausible reversals?
4. **Structural simplicity:** Can a different boundary remove classes,
   branches, flags, duplicate engines, runtime generation, reflection, or
   special cases?
5. **Hot-path quality:** Look for avoidable Var, reflection, interface,
   allocation, boxing, bounds, `instanceof`, and reduced-check costs. Confirm
   with bytecode, JIT/inlining evidence, or focused benchmarks rather than
   intuition alone.
6. **Upstream fitness:** Is the change small, direct, documented, testable, and
   maintainable at Clojure's quality bar? Would a maintainer understand why each
   moving part must exist?

## Pre-implementation gate

Attack the plan before code exists. Require it to identify semantic oracles,
competing implementations, decisive experiments, performance acceptance rules,
and a simpler fallback if its main idea fails. Reject speculative machinery
whose need has not been measured.

## Final gate

Review the actual diff against the original phase plan and current Clojure
behavior. Inspect all production paths, tests, build changes, benchmark code,
and reported results. Re-run relevant validation. Treat these as blockers:

- a known semantic difference hidden by a narrow test;
- a performance claim without reproducible release-equivalent evidence;
- a repeatable material regression in a supported primary case;
- a slower refactor compared only with an easy baseline rather than the fastest
  correct hand-written Java candidate;
- special-case or abstraction growth that a simpler design can eliminate;
- reflection, runtime generation, unbounded caches/classes, or duplicated hot
  paths without demonstrated value;
- phase completion claimed while an exit criterion is unmet.

## Output

List findings by severity and impact. For each blocker, state the evidence, why
it matters, and the smallest clean remedy. Omit cosmetic nits while structural,
semantic, or measurement problems remain. End with one verdict:

- `ready for implementation`;
- `ready to complete the phase`;
- `not ready`, with named blockers.

Then add a short `What matters` summary using `$plain-english` guidance.
