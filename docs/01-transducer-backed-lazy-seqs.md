# Implementation #1: transducer-backed lazy sequences

Status: draft implementation plan  
Last updated: 2026-08-31

## Summary

This document describes how to finish the object-oriented, transducer-backed
lazy-sequence engine independently of primitive specialization.

The first milestone is deliberately narrower than the original experiment:

- Preserve the observable behavior of Clojure's sequence functions.
- Use one transducer-driven implementation for incremental and chunked input.
- Return an ordinary, fully compatible lazy sequence.
- Establish correctness and modern benchmark evidence before adding fusion or
  primitive paths.
- Do not use runtime code generation, ASM, type analysis, primitive buffers, or
  custom primitive seq interfaces in this milestone.

The existing repository has a substantially working stepping and buffering
engine. The main work is to put a correct sequence surface around it, remove
coupling to the primitive experiment, complete the missing arities, and build a
test and benchmark suite strong enough to support a technical article or an
upstream proposal.

## Goals

1. Implement a general `xf-seq` operation that turns a transducer and one input
   collection into a lazy sequence.
2. Make unary `map`, `filter`, `remove`, and `take` use that engine without
   observable semantic regressions.
3. Support all existing `map` collection arities, including multiple input
   collections.
4. Preserve Clojure's important realization behavior:
   - no source access when constructing the lazy result;
   - incremental processing for dechunked input;
   - input-chunk-at-a-time processing for chunked input;
   - prompt termination for reduced results;
   - exactly-once transducer completion.
5. Make the project build and test from a clean checkout on current Clojure and
   supported JDKs.
6. Produce repeatable JMH results for throughput and allocation.
7. Leave an implementation that can be evaluated as a small Clojure core
   change rather than requiring adoption of the primitive-specialization work.

## Non-goals

The following are explicitly deferred to Implementation #2 or later work:

- Inferring `long` or `double` signatures from `IFn` interfaces.
- Primitive seq, chunk, buffer, or reduce protocols.
- Runtime `eval`, generated `deftype` classes, or ASM generation.
- Automatically fusing arbitrary chains returned by ordinary `map` and
  `filter` calls.
- Replacing every function in `clojure.core` in the first milestone.
- Parallel processing.
- Promising that one implementation wins every benchmark. The target is exact
  behavior, no material regressions, and clear wins in identified workloads.

## Current implementation: what to retain

The current code contains several ideas worth preserving:

- A transducer writes into a mutable reducing-function buffer.
- The source driver processes a complete input chunk when one is available.
- Dechunked sources are processed until at least one output is available.
- A continuation is attached only after buffered output, avoiding a lazy-seq
  node per source item.
- Small output batches use `Cons`; larger batches use `ArrayChunk` and
  `ChunkedCons`.
- Expanding transducers can produce more than 32 values without building a
  recursively deep result.

Relevant existing code:

- [`xfseq.core/InitXFSeq`](../src/xfseq/core.clj)
- [`xfseq.gen/generate-xfseq-simple`](../src/xfseq/gen.clj)
- [`ObjectBuffer`](../src-java/xfseq/buffer/ObjectBuffer.java)
- The hand-written Java step variants under [`src-java/xfseq`](../src-java/xfseq)

The hand-written Java variants should be treated as design references. The
generated implementations should not be on the #1 execution path.

## Current implementation: confirmed problems

The implementation must not be considered behaviorally complete until these
issues are resolved:

1. `XFSeqHead` is `Seqable` but is not a normal Clojure sequence. `count` and
   `vec` fail on it.
2. A transducer is applied twice when the sequence is first realized: once for
   interface analysis and once for execution.
3. Completion is not invoked for empty input, so a transducer that emits during
   completion loses that output.
4. `consume` does not invoke ordinary Clojure reducing-function completion.
5. `drain` loses a transformation in a two-stage pipeline.
6. The ASM implementation touches the source during construction, unlike the
   public `xfseq.core` path and `clojure.core/map`.
7. `map` implements only its transducer and one-collection arities.
8. The Java sources have no clean-checkout build step.
9. The test suite checks realized equality but not the sequence API or
   realization timing.

Items 4 and 5 belong to optional fusion work and must not block the initial
object-only engine. They should either be fixed in a later phase or clearly
marked experimental until then.

## Semantic reference

The primary reference is the behavior of the current stable Clojure release,
not merely `clojure.core/sequence`.

This distinction matters. `map` and `filter` preserve input chunk boundaries,
while `sequence` is implemented through `TransformerIterator` and an iterator
to chunked-seq adapter. They can differ in when functions are invoked even when
the final values are equal.

For each replacement candidate, compare against the corresponding direct core
function:

| Candidate | Primary oracle | Secondary oracle |
|---|---|---|
| unary `map` | `clojure.core/map` | `sequence` with `map` transducer |
| multi-input `map` | `clojure.core/map` | multi-input `sequence` |
| `filter` | `clojure.core/filter` | `sequence` with `filter` transducer |
| `remove` | `clojure.core/remove` | `sequence` with `remove` transducer |
| `take` | `clojure.core/take` | `sequence` with `take` transducer |
| generic `xf-seq` | `clojure.core/sequence` | `transduce` for full value output |

Value equality against `sequence` is necessary but not sufficient for replacing
a direct sequence function.

## Required observable behavior

### Result surface

The result returned by every collection arity must behave like an ordinary
Clojure lazy sequence:

- `seq`, `first`, `next`, `rest`, `nth`, `count`, `reduce`, `into`, `vec`, and
  iteration work normally.
- Empty results have the same `seq` and printed behavior as the corresponding
  core function.
- Sequential equality and hashing follow the standard sequence contracts.
- Realization is cached.
- Repeated calls do not rerun transformation functions.

The first implementation should therefore return `clojure.lang.LazySeq`
directly. It should not return a custom head object solely to support
deconstruction.

### Construction laziness

Constructing the result must not:

- call `seq` on the source;
- obtain an iterator;
- apply the transducer to its reducing function;
- call a mapping or predicate function;
- call transducer completion.

All initialization belongs inside the initial `LazySeq` thunk.

### Completion

The transducer must be applied exactly once and completed exactly once.

Completion occurs when:

- the source is exhausted, including an initially empty source; or
- the reducing step returns `Reduced`.

Completion may append output. That output must appear after all step output and
must not be lost on empty input.

### Reduction protocol

The driver must use the transducer's returned accumulator and detect reduction
with `RT.isReduced`/`reduced?`. It must not infer reduction by comparing the
returned object with the buffer by identity.

The buffer is stateful storage, but it is also a reducing function. Its step
arity must return the accumulator it was given. A correct baseline loop is
conceptually:

```clojure
(let [next-acc (rf acc input)]
  (if (reduced? next-acc)
    (complete (unreduced next-acc))
    (continue next-acc)))
```

An identity-based `no-reduced` fast path may be evaluated later for transducers
whose behavior is controlled and proven, but it must be an optimization rather
than the semantic foundation.

### Chunking and realization

For a chunked source:

- Process one complete input chunk per step, unless reduction terminates it.
- Return all output produced by that input chunk before processing the next
  source chunk.
- Do not process past a reduced result within the chunk.

For a dechunked source:

- Process source items until at least one output value exists.
- Return promptly once output exists.
- Continue across rejected inputs for filtering transducers.

This reproduces the useful behavior of Clojure's direct unary `map` and
`filter`: chunked input is eagerly transformed by chunk, while a list or lazy
dechunked input is incremental.

An expanding transform may create more than one output chunk from one input
chunk. Output order must remain stable, and the resulting chain must not grow
the Java stack.

### Early termination

Reduced results must:

- stop before the next source item;
- run completion exactly once;
- return step output followed by completion output;
- leave no continuation that can restart the source.

`take` requires an additional public-function special case. For `n <= 0`, the
direct core function does not inspect the source even when the result is
realized. A generic transducer driver cannot infer that property before reading
input, so the `take` collection arity must preserve it explicitly.

## Proposed architecture

### 1. Initial lazy thunk

Introduce an object-only initializer whose sole job is deferred setup:

```text
ObjectXFSeqInit
  fields: xform, coll
  invoke:
    obtain seq from coll
    create ObjectBuffer
    apply xform to buffer exactly once
    if source exists: invoke first ObjectXFSeqStep
    otherwise: complete and return buffered completion output
```

The initializer implements `IFn` and is passed to `LazySeq`. No part of this
algorithm runs when the `LazySeq` is constructed. Obtaining the source seq
precedes transducer initialization so a source failure is observed before any
transducer-initialization effects, matching the current public engine's order.

### 2. Stateful step

Use one hand-written Java class for the hot loop:

```text
ObjectXFSeqStep
  fields:
    ObjectBuffer buffer
    IFn reducingFn
    Object accumulator
    ISeq source
    boolean completed
```

Its zero-arity invocation:

1. Resume from `source`.
2. Process a chunk or individual source items.
3. Detect `Reduced` explicitly.
4. If terminal, complete and return the buffer as a final tail.
5. If output exists, store the source continuation and return buffered output
   followed by `LazySeq(this)`.
6. If no output exists, continue without allocating an empty seq node.

Mutable state is acceptable here because each continuation is guarded by the
realization semantics of its surrounding `LazySeq`. Add a concurrency test to
verify that concurrent realization of the same node does not invoke the step
twice.

### 3. Object buffer

Keep a single object buffer based on the current `ObjectBuffer`, with these
requirements:

- Initial capacity is benchmarked rather than assumed.
- Capacity growth is bounded and checked for overflow.
- Slots are nulled when values have been transferred, preventing retention.
- One to four results may use `Cons` only if that remains a measured win.
- Five to 32 results use one `ArrayChunk`.
- Larger results are divided into correctly ordered chunks of at most 32.
- Completion output uses the same buffer and ordering rules.
- The buffer never exposes its mutable backing array after resetting it.

The special cases for one to four values are optimization candidates, not part
of correctness. Start by retaining them, but keep the implementation easy to
remove based on allocation and throughput results.

### 4. Public Clojure API

For #1, reuse Clojure's existing transducer arities instead of maintaining
independent copies:

```clojure
(defn map
  ([f] (clojure.core/map f))
  ([f coll] (xf-seq (clojure.core/map f) coll))
  ...)
```

The same applies to `filter`, `remove`, and `take`. This prevents the object-only
work from drifting away from core transducer semantics and cleanly separates it
from the type-rewriting machinery in Implementation #2.

### 5. Multiple input collections

`map` compatibility requires a separate multi-source driver or a generalized
driver mode.

The initial implementation should favor clarity:

- Maintain an array or vector of current `ISeq` values.
- At each step, stop if any source is exhausted.
- Collect one first value from every source.
- Invoke the transducer step with the accumulator followed by those values.
- Advance every source once.
- Return as soon as output exists.

Do not chunk this path initially. Clojure's current multi-collection `map` is
incremental, and a chunked implementation would change realization timing and
could over-consume longer sources when the shortest source ends.

Specialize common arities two and three without `apply` only after the generic
version is correct and JMH identifies invocation overhead as material.

### 6. Fusion boundary

Fusion is not required to validate the lazy-seq engine.

The original `XFSeqHead` exists largely so `consume` and `drain` can recover the
underlying transducer and source. That custom head caused standard sequence
compatibility failures. The initial implementation should remove it from the
public path.

After the engine is stable, evaluate fusion as a separate feature with one of
these designs:

1. An explicit reducible/seqable wrapper similar to `eduction`.
2. A fully implemented custom `ISeq` type that also supports `IReduceInit`.
3. Explicit `consume` operating on an opt-in wrapper, not ordinary lazy seqs.

Do not make ordinary `map` results destructive. Any `drain`-style API must be
clearly named, independently tested, and excluded from the first upstream
proposal.

## Implementation phases

### Phase 0: preserve and characterize

- Tag or branch the current research state.
- Record the existing public API and known failures.
- Keep the primitive and ASM implementation available for later comparison.
- Add a short architecture note pointing from the old code to this document.

Exit criterion: the 2020 implementation remains reproducible while the #1 path
can be simplified aggressively.

### Phase 1: modern build

- Upgrade the stable test target to Clojure 1.12.5.
- Add a Java compilation step using `tools.build` or equivalent.
- Compile library Java classes to `target/classes`, not an IDE directory.
- Add `:test`, `:bench`, and optional `:dev` aliases.
- Use qualified dependency coordinates.
- Add a single command that builds Java and runs all tests from a clean clone.
- Enable reflection warnings and fail CI on unexpected reflection warnings.
- Run CI on Java 17, 21, and 25; optionally include the current Clojure 1.13
  prerelease as an allowed experimental job.

Exit criterion: a fresh checkout can run tests without IDE-produced class
files.

### Phase 2: object-only engine

- Implement `ObjectXFSeqInit` and one `ObjectXFSeqStep`.
- Refactor or replace `ObjectBuffer` behind a small internal interface.
- Apply each transducer once.
- Track the returned accumulator.
- Detect `Reduced` explicitly.
- Complete empty input correctly.
- Return a real `LazySeq` from `xf-seq`.
- Remove primitive analysis and runtime class generation from this execution
  path.

Exit criterion: generic `xf-seq` matches `sequence` for the full differential
value suite, including empty completion and expanding transforms.

### Phase 3: unary core-function compatibility

- Route unary `map`, `filter`, `remove`, and `take` through the object engine.
- Add function-specific realization tests against direct `clojure.core`
  implementations.
- Preserve `take` behavior for non-positive counts.
- Verify chunked and dechunked call counts.
- Verify all standard seq operations on empty and non-empty results.

Exit criterion: no known value, exception, protocol, or realization-timing
difference for the supported arities.

### Phase 4: multi-source map

- Implement the generic multi-source driver.
- Add the two-, three-, and variadic-collection `map` arities.
- Stop at the shortest source without over-consuming the others.
- Test finite/infinite combinations and source exceptions.
- Benchmark specialized two- and three-source invocation only after correctness.

Exit criterion: the replacement has the complete public `map` arity surface.

### Phase 5: broaden the candidate set

Evaluate other core functions that already have transducer arities:

- `mapcat`
- `map-indexed`
- `keep` and `keep-indexed`
- `take-while`, `drop`, and `drop-while`
- `take-nth`
- `replace`
- `partition-all` and `partition-by`
- `distinct` and `dedupe`
- `interpose`

Adopt them individually. Each function needs its own semantic trace comparison;
sharing a transducer engine does not automatically prove equivalent laziness.

Exit criterion: every adopted function passes its direct-core oracle suite and
has benchmark evidence justifying the change.

### Phase 6: optional fusion

- Redesign `consume` completion semantics.
- Replace or remove the broken `drain` composition.
- Decide whether fusion is non-destructive, destructive and explicit, or only
  available through a reducible wrapper.
- Benchmark against `eduction` and `transduce`.

Exit criterion: fusion has a separate, documented contract and cannot break
ordinary lazy-seq behavior.

### Phase 7: upstream extraction

- Identify the smallest useful core change.
- Reproduce benchmarks in a standalone branch against an unmodified Clojure
  checkout.
- Discuss the problem and approach with Clojure maintainers before preparing a
  large patch.
- Keep the proposed change object-only and free of the primitive experiment.
- Present alternatives and tradeoffs, including the existing
  `TransformerIterator` implementation.

Likely proposal shapes, from smallest to largest:

1. Improve the buffering or stepping internals used by `sequence`.
2. Add an internal transducer-to-lazy-seq engine and migrate one function.
3. Migrate the full set of supported unary functions after the shared engine is
   accepted and measured.

## Correctness test plan

### Differential value matrix

Exercise at least these source families:

- `nil` and empty collections
- persistent list
- vector and subvector
- `range`
- hash set and sorted set
- hash map and sorted map
- object array
- Java `Iterable` and `Iterator` adapters
- dechunked lazy seq
- `repeat`, `iterate`, and other infinite sources
- a custom `Seqable`
- a custom chunked seq

Use sizes around every internal boundary:

```text
0, 1, 2, 3, 4, 5, 7, 8, 9, 31, 32, 33, 63, 64, 65, 1,000
```

Exercise transducers with different cardinality behavior:

- one-to-one: `map`
- zero-or-one: `filter`, `keep`
- early termination: `take`, `take-while`
- stateful: `distinct`, `dedupe`, `partition-by`
- expanding: `mapcat`, `cat`, `interpose`
- completion output: `partition-all` and a purpose-built completion-emitting
  transducer
- composed pipelines mixing the above

### Realization trace tests

Value equality will miss the most important regressions. Create traceable
sources and functions that append events such as:

```clojure
[:source-seq]
[:source-first n]
[:source-next n]
[:map n]
[:predicate n]
[:rf-step n]
[:rf-complete]
```

Compare the trace after:

- construction only;
- `seq`;
- `first`;
- `next`;
- consuming an input chunk;
- partial `take` of the result;
- full consumption;
- repeated consumption of an already realized prefix.

### Sequence surface tests

For every public collection arity, cover:

- `seq?` where appropriate and `sequential?`
- `first`, `next`, `rest`, and `nth`
- `count`
- `vec` and `into []`
- `reduce`
- equality and hash equality with a list/vector of the same values
- printing empty and non-empty values
- Java iteration
- repeated realization
- concurrent realization of the same lazy node

### Failure behavior

Compare exception type and invocation point for:

- a source whose `seq`, `first`, or `next` throws;
- a mapping function or predicate that throws;
- a transducer that throws during initialization, step, or completion;
- invalid function arity;
- non-seqable input;
- reduced output in the middle of a chunk.

### Memory-retention tests

Use weak references or heap inspection to verify that:

- emitted object-buffer slots are cleared;
- processed source prefixes are no longer retained after advancing;
- abandoned lazy tails retain no more state than necessary;
- large expanding transforms do not retain an oversized buffer indefinitely
  unless reuse is a measured win.

Property-based tests should supplement these examples once the deterministic
suite is stable.

## Benchmark plan

### Harness

Use JMH 1.37 directly for publishable numbers. Criterium 0.4.6 remains useful
for REPL exploration but should not be the sole source of upstream evidence.

Requirements:

- Run benchmarks in forked JVMs.
- Use multiple warmup and measurement iterations.
- Use at least three forks for the final report.
- Consume results through a JMH `Blackhole` or a checksum returned from the
  benchmark.
- Use the GC profiler to record bytes and allocations per operation.
- Keep collection construction in `@Setup`, outside the measured method.
- Measure startup/class-loading separately rather than mixing it into steady
  state.
- Record CPU, OS, architecture, JVM vendor/version, Clojure version, heap
  settings, and GC.

### Implementations to compare

1. Direct `clojure.core` function.
2. `sequence` with the equivalent transducer.
3. `eduction` followed by the relevant sink.
4. `transduce` for fully eager reductions.
5. The object-only xfseq engine.
6. The preserved 2020 implementation as historical context, not as the target.

Primitive xfseq results belong in Implementation #2 and must not be mixed into
claims about the object-only engine.

### Sources

Benchmark at least:

- list/dechunked lazy seq
- vector
- subvector
- range
- hash set
- hash map entries
- object array
- Java iterable
- `repeat`/`iterate` with an explicit terminating sink

### Workloads

- identity map
- small arithmetic map
- function call with enough work to reduce dispatch dominance
- filter selectivity near 0%, 1%, 50%, 99%, and 100%
- map then filter
- five-stage map pipeline
- early `take` from a large or infinite source
- expanding transforms producing 0, 1, 2, 32, and more than 32 outputs per
  input batch
- stateful transforms
- multi-source map with equally and unequally sized inputs

### Sinks

Measure distinct user behaviors:

- construct only
- `first`
- consume a small prefix
- `dorun`/full traversal
- `into []`
- object reduction/checksum

Construction-only results are important because a faster traversal that adds
class generation or eager source work is not an unconditional improvement.

### Runtime matrix

The primary publication target should use:

- Clojure 1.12.5
- Java 25

Also run compatibility/performance comparisons on Java 17 and 21. Run the
current Clojure 1.13 prerelease as forward-looking data, clearly separated from
stable results.

### Interpretation

Report raw scores and uncertainty, not only ratios. Flag:

- throughput changes;
- allocation changes;
- partial-realization latency;
- source over-consumption;
- startup and class-loading cost;
- regressions isolated to a particular collection or workload.

Do not pool incomparable workloads into one average speedup.

## Acceptance criteria

### Library milestone

- Clean checkout builds and tests with one documented command.
- No required IDE output directories.
- All supported arities return standard lazy-seq-compatible values.
- Transducers are initialized and completed exactly once.
- Empty completion, reduction, stateful, and expanding transforms pass.
- Complete `map` arities are implemented.
- Differential value and realization suites pass on Java 17, 21, and 25.
- No unexpected reflection warnings.
- No runtime code generation on the #1 path.

### Performance milestone

- Final results come from forked JMH runs with allocation measurements.
- No supported primary workload shows a repeatable material regression without
  an understood and accepted tradeoff.
- At least one important workload shows a clear throughput or allocation win
  large enough to justify added implementation complexity.
- Partial consumption is reported separately from full traversal.
- The benchmark source and raw result files are publishable with the article.

For upstream discussion, define "material" before running the final suite. A
reasonable starting policy is to investigate any repeatable regression above
3–5%, rather than declaring success from noisy point estimates.

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| Shared abstraction is slower than specialized `map`/`filter` loops | Keep the hot loop in one hand-written Java class; measure dispatch and allocation separately. |
| Realization timing changes | Treat event traces as first-class compatibility tests. |
| Chunk processing over-consumes input | Preserve existing unary chunk semantics; keep multi-source map dechunked. |
| Expanding transforms retain large arrays | Clear references, reset oversized buffers, and profile retained memory. |
| Fusion complicates the sequence surface | Keep fusion outside the first milestone and upstream proposal. |
| Primitive machinery obscures #1 results | Maintain separate namespaces, aliases, tests, and benchmark result groups. |
| Modern JVM results differ greatly by version | Run Java 17, 21, and 25 and publish the exact runtime matrix. |
| Upstream scope becomes too large | Propose one internal engine or one migrated function first. |

## Documentation and article artifacts

The #1 implementation should produce these durable artifacts:

- A README explaining the idea and its supported API.
- A correctness document describing realization semantics.
- A reproducible benchmark command.
- Machine-readable JMH results plus a small report generator.
- Allocation and CPU profiles for representative wins and regressions.
- A table showing which core functions have been migrated and verified.
- A design comparison with Clojure's current `TransformerIterator`.

A useful article structure is:

1. The duplication between direct lazy-seq functions and transducer arities.
2. The input-aware transducer-seq design.
3. Output buffering and chunk behavior.
4. Compatibility bugs discovered while reviving the prototype.
5. Modern JVM throughput and allocation results.
6. What should and should not move into `clojure.core`.
7. Primitive specialization as separate future work.

## Decision log

These decisions are part of the current plan and should be changed only with a
recorded reason:

1. **Object-only first.** Primitive specialization is excluded from #1.
2. **A real `LazySeq` result.** Deconstruction does not justify an incomplete
   sequence surface.
3. **One hand-written hot loop.** No runtime-generated classes in #1.
4. **Core transducers are the source of truth.** Do not duplicate their bodies
   until #2 requires specialization.
5. **Explicit reduced detection.** Identity comparison is not the baseline
   contract.
6. **Input-aware chunking.** Unary chunked sources process one input chunk;
   dechunked sources remain incremental.
7. **Multi-source map starts dechunked.** Avoid over-consumption and semantic
   surprises.
8. **Fusion is separate.** `consume` and `drain` are not prerequisites for the
   lazy-seq engine.
9. **JMH for claims, Criterium for exploration.** Allocation is a primary
   metric, not an afterthought.
10. **Upstream in small steps.** The first proposal should be independently
    useful and reviewable.

## External references

- [Clojure 1.12.5 `sequence`, `map`, and related source](https://github.com/clojure/clojure/blob/clojure-1.12.5/src/clj/clojure/core.clj)
- [Clojure stable and development releases](https://clojure.org/releases/downloads)
- [OpenJDK Java Microbenchmark Harness](https://github.com/openjdk/jmh)
- [Criterium](https://github.com/hugoduncan/criterium)
- [Clojure development workflow](https://clojure.org/dev/workflow)
- [Clojure patch-development guidance](https://clojure.org/dev/developing_patches)
