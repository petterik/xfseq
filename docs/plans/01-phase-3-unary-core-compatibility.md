# Implementation #1, Phase 3: unary core-function compatibility

Status: Awaiting final review

Parent design: [`docs/01-transducer-backed-lazy-seqs.md`](../01-transducer-backed-lazy-seqs.md)

Planning date: 2026-09-01

## Plain-English problem

The object engine can produce the right values, but replacing Clojure's unary
`map`, `filter`, `remove`, and `take` requires more than value equality. These
functions expose when input advances, how chunked output behaves, what happens
after a lazy computation throws, and whether a later consumer sees a chunk or
one item. The current wrappers already call `xf-seq`, but direct Clojure 1.12.5
probes show that this naïve route is not yet compatible.

The actual Phase 3 problem is:

> Determine whether one small, explicit unary compatibility boundary can make
> the Phase 2 object engine behave like the four direct core functions, then
> measure that exact candidate against direct core before adding any reduction
> protocol or hot-path machinery.

The desired outcomes are exact unary sequence behavior, direct-core throughput
and allocation evidence, and an honest per-function adoption decision. The
suggested mechanisms—core transducers, a no-`Reduced` loop, or a memoizing
`IReduceInit` result—are candidates, not goals. A mechanism is removed or kept
benchmark-only if it does not preserve semantics and earn its complexity.

## Phase goal

1. Make the transducer arities of `xfseq.core/map`, `filter`, `remove`, and
   `take` delegate to the corresponding Clojure 1.12.5 transducer arities.
2. Route each supported unary collection arity through the Phase 2 object
   engine using an internal, non-user-configurable compatibility profile that
   reproduces its direct core oracle.
3. Match direct core for values, exceptions, post-exception behavior,
   construction laziness, source-advance order, input and output chunking,
   downstream realization, sequence protocols, memoization, reduction, and
   concurrency.
4. Add reproducible direct-core JMH 1.37 throughput and allocation comparisons,
   including full reduction with retained and unretained result heads.
5. Compare ordinary core lazy results, the public unary candidate, generic
   `xf-seq`, `sequence`, `eduction`, `transduce`, and applicable repaired Java
   candidates before attributing a gap to the result type.
6. Consider an operation-owned no-`Reduced` path only where the public operation
   proves that reduction cannot occur and the measured benefit clears the
   recorded selection rule.
7. Investigate a memoizing `ISeq` plus `IReduceInit` result only if the completed
   LazySeq matrix and focused JVM evidence identify the ordinary reduction
   cursor as a material cause. Phase 3 may build a benchmark/test prototype;
   evidence supporting production promotion is a material redesign and forces
   a Phase 3 replan plus fresh review gate before product code changes.
8. End with an individual `adopt`, `investigate`, or `reject` recommendation for
   each of the four functions. Semantic compatibility is mandatory, but does
   not by itself justify a core replacement.

## Non-goals

- No multi-source `map`; that remains Phase 4.
- No Phase 5 functions, primitive specialization, type inference, primitive
  buffers/seqs, ASM, runtime generation, or `eval`.
- No `consume`, `drain`, destructive iteration, explicit fusion API, or other
  Phase 6 work.
- No weak or soft references, GC-sensitive replay, finalizer/cleaner behavior,
  or runtime test for whether a caller retained a head.
- No direct-linking-off result. A valid off comparison requires an off-built
  core jar from the same Clojure revision and belongs to later upstream work.
- No alternate JDK, Clojure prerelease, CI matrix, publication result, patched
  Clojure jar, or upstream proposal in this phase.
- No source-class whitelist or claim that one observed `IChunkedSeq` node proves
  the shape of the source tail.
- No public flag selecting retry, chunk, no-reduced, or reduction behavior.
- No copied Clojure implementations kept beside the engine as a hidden fallback.
- No performance conclusion from the Phase 2 public rows; they did not invoke
  the four direct unary core functions as the primary baseline.

## Why this matters upstream

### Clojure core maintainer

A shared engine is interesting only if the replacement stays smaller and more
explainable than the four direct functions it replaces. Phase 3 therefore
treats every semantic mode, branch, class, and protocol as a cost. If exact
compatibility requires a growing set of function-specific loop copies, the
correct result is to reject the shared replacement rather than disguise the
duplication.

### Library user relying on seq semantics

Users can observe chunking through downstream side effects, exceptions,
`chunked-seq?`, and partial realization. They can also retain a lazy head,
reduce it, replay it, or force the same node concurrently. The direct core
function—not `sequence` and not value equality—is the oracle for each public
collection arity.

### JVM and performance engineer

The candidate adds a transducer reducing-function call, a state machine, a
buffer, and `Reduced` handling to operations that core implements directly.
Those costs may reverse across source shapes and sinks. Direct-linked AOT
callers, forked JMH, allocation data, retained/unretained heads, and focused
inlining or allocation evidence must identify the cause before a specialized
loop or result type is added.

### Future contributor

The durable result must explain why every moving part exists, which functions
earned adoption, and which failed. Phase 2's repaired Java variants and raw
evidence remain reproducible. Phase 3 adds its own manifests and results rather
than rewriting the meaning of Phase 2 artifacts.

## Current repository facts and baseline behavior

### Phase handoff and environment

- Phase 2 is `Complete` at repository HEAD
  `5e6e855b0ce9d7032845102cef904e6460a04b9f` (`Ship it: Phase 2 (#7)`).
- The worktree was clean during planning.
- The normal check passes on the handoff: 29 tests, 2,906 assertions, no
  failures or errors.
- The exact local research lane is Clojure library 1.12.5, Clojure CLI
  1.12.5.1664, JMH 1.37, Homebrew OpenJDK 26.0.2.1, macOS 26.2 / Darwin
  25.2.0, arm64.
- `deps.edn` makes the benchmark alias direct-linking-on. `build.clj` AOT
  compiles the benchmark callers with `:direct-linking true` and checks their
  bytecode for Var lookup.

### Current public path

- `xfseq.core/xf-seq` returns
  `LazySeq(ObjectXFSeqInit(xform, coll))`.
- `ObjectXFSeqInit` obtains `RT.seq(coll)` inside the thunk, creates one
  `ObjectBuffer`, applies the xform once, completes empty input, and starts
  `XFSeqStepSimple`.
- `XFSeqStepSimple` is the one canonical mixed, reduced-aware production loop.
  Other repaired Java variants remain test/benchmark candidates.
- `ObjectBuffer` emits one to four values as `Cons`, five to 32 as one
  `ChunkedCons`, and larger output in chunks of at most 32.
- The public collection arities of `map`, `filter`, `remove`, and `take`
  already call `xf-seq`; Phase 3 is a semantic repair and adoption decision,
  not the first insertion of those call sites.
- Their transducer arities currently use `xfseq.analyze` factories and generated
  reducing-function shapes. This couples the public object path to the later
  primitive experiment. The Phase 2 benchmark had to use ordinary core
  transducers because the analyzed map xform could require `IFn.OLO` and fail
  under `TransformerIterator`.
- `take` currently has no public guard before `xf-seq`, so realizing `(take 0
  source)` touches the source and realizing `(take "bad" [])` returns empty
  rather than throwing like direct core.

### Retained candidates and Phase 2 performance context

- Seven repaired object candidates have stable `-v2` IDs: polymorphic, mixed,
  dechunked-only, and chunked-only reduced-aware loops plus mixed,
  dechunked-only, and chunked-only structurally non-reducing loops.
- Phase 2 selected the mixed reduced-aware loop and current buffer because no
  specialization met the combined benefit, regression, allocation, and
  structural-proof rule.
- A restricted non-reducing loop was about 26.9% faster than the mixed
  reduced-aware loop for vector/1,000 map/traverse, but only about 4.3% faster
  for list/1,000 map/prefix8 and was not safe for arbitrary `xf-seq`.
  `map`, `filter`, and `remove` provide a structural operation-owned proof in
  Phase 3, but that fact only makes the candidate eligible for measurement.
- Phase 2 public evidence contained important reversals for subvector, range,
  set, array, iterable, and take/reduce rows. Those rows compared generic
  transducer contexts and support only the recorded `promising` handoff.
- Phase 2 found the shared object step too large to inline in representative
  JIT logs. A new protocol or profile cannot be justified by an assumption
  that HotSpot will erase it.

### Direct Clojure 1.12.5 oracle facts

The installed 1.12.5 `clojure/core.clj` was inspected directly.

- Unary `map` and `filter` are Clojure `lazy-seq` implementations. A chunked
  input is processed as one input chunk and returned through `chunk-cons`.
- `remove` delegates to `filter` with `complement` and therefore inherits its
  realization and output-chunk behavior.
- Unary `take` is item-at-a-time for every input shape. It checks `(pos? n)`
  before calling `seq` on the source, returns an unchunked `Cons` chain, and
  evaluates `(rest s)` for every returned item, including the last one.
- The transducer arities are distinct implementations. Core `map` also supports
  the multi-input reducing step needed by Phase 4.
- Core `LazySeq` does not implement `IReduceInit`; neither does the current
  `xf-seq` result. `eduction` does implement `IReduceInit` and is not an `ISeq`.
- Clojure's `lazy-seq` uses a one-shot thunk. After a direct unary lazy node
  throws, re-forcing that node produces an empty tail rather than replaying the
  failed computation. The Java `AFn` thunks used by the object engine remain
  callable and currently retry.

### Planning oracle probes and known deltas

Small probes were run against direct core and
`xfseq/xf-seq` with the corresponding ordinary core transducer. These are
planning facts, not performance results.

| Case | Direct core 1.12.5 | Current object path | Consequence |
|---|---|---|---|
| `map` over vector | `ChunkedCons` | `ChunkedCons` | Basic map chunk shape agrees. |
| Sparse `filter`/`remove`, four outputs from a 32-input vector chunk | `ChunkedCons` | `Cons` | `chunked-seq?` and downstream realization differ. |
| `first` of `map` over that sparse filter | Mapper called four times | Mapper called once | The buffer's small-result policy changes ordinary composed behavior. |
| `take` 5, 31, 32, or 33 from a vector | Unchunked `Cons` result | `ChunkedCons` result | Output protocol and downstream timing differ. |
| `take 0` over traced `Seqable` | Source untouched | Source `seq` called | Non-positive behavior is wrong. |
| `take "bad" []`, realized | `ClassCastException` before source access | Empty result | Validation and exception order are wrong. |
| Dechunked `filter`, first accepted item | Source `first`, source `next`, predicate | Source `first`, predicate, source `next` | Side-effect and throw order differ. |
| `take 1` over traced `ASeq` | Source `first`, then `next` | Source `first` only | The generic reduced path stops earlier than direct `take`. |
| Mapping/predicate failure in the first vector chunk, then re-forcing | Direct node becomes empty | Engine reruns from the beginning | Post-exception behavior differs. |
| Failure in a later engine continuation | Direct core leaves an empty tail | Engine can retry at the saved failed input | The divergence is not limited to initialization. |

These observations invalidate the simplest literal implementation—merely
replace each current analyzed xform with a core xform and keep the generic
driver unchanged.

## Semantic contract for Phase 3

### Shared requirements

All four collection arities must:

1. Return a `LazySeq` at construction in the baseline design.
2. Avoid source access, function/predicate calls, xform application, and
   completion during construction.
3. Apply the selected core transducer once and complete it once on a successful
   terminal path.
4. Match direct core values and exception class/invocation point.
5. Match direct core after an exception: already returned prefixes remain, the
   failed lazy node becomes an empty tail, and user code/source access is not
   replayed by forcing that same node again.
6. Cache successful realization and never rerun a realized prefix.
7. Preserve standard sequence operations, equality, hashing, metadata behavior,
   printing, Java iteration, and concurrent realization.
8. Keep generic `xf-seq`'s Phase 2 contract unchanged. Unary compatibility is
   an internal semantic profile, not a silent change to the generic oracle.

### `map`

- Dechunked order is source `first`, mapper, source `rest` for each returned
  item.
- Chunked input maps the complete input chunk, then obtains `chunk-rest`, and
  exposes all mapped output as a chunk before the next input chunk.
- A mapper throw does not advance the dechunked source tail for that item.
- The transducer arity is the ordinary core map transducer, including its
  multi-input reducing step; the collection surface remains unary until Phase 4.

### `filter` and `remove`

- Dechunked order is source `first`, source `rest`, then predicate. Rejected
  inputs continue in the same lazy node until output or exhaustion.
- Chunked input evaluates the complete input chunk, obtains `chunk-rest` only
  after the whole chunk succeeds, and returns passing values in a chunk even
  when only one to four values pass.
- An empty output chunk is skipped without an empty sequence node.
- `remove` shares the filter-like driver behavior and uses core's transducer
  arity rather than a second predicate engine.

### `take`

- Realization checks `pos?` before source `seq`. Invalid `n` throws at that
  point even for an empty source; `n <= 0` returns empty without source access.
- Positive `take` processes one `ISeq` item at a time even when the source is
  chunked and emits an unchunked `Cons` chain.
- It advances `rest` after every emitted item, including the last item selected
  by `n`.
- It does not inspect the first value of the following source item.
- The core take transducer remains the transformation source; any driver
  handling needed to match direct `take` is explicit and internal.

### Additional reduction-result contract

The baseline result remains an ordinary `LazySeq`. If the evidence-gated
`IReduceInit` experiment is triggered, its benchmark/test prototype must
demonstrate the following contract before a production replan can be
considered:

- reduce-then-seq and seq-then-reduce values and traces;
- repeated full and early reduction without duplicate user calls;
- a retained head replaying every cached value;
- an unretained head allowing consumed prefixes to become unreachable without
  weak references or GC-dependent branching;
- one-shot sources, completion, exceptions, and concurrent seq/reduce access;
- ordinary equality, hashing, metadata, iteration, printing, and seq methods;
- no destructive or single-use behavior.

An additional `IReduceInit` protocol is acceptable only if all ordinary
sequence behavior remains compatible. It is not acceptable merely because a
benchmark calls `reduce` faster, and this reviewed plan does not authorize its
promotion to the public result.

## Options and trade-offs

### Unary semantic boundary

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| Call generic `xf-seq` with core xforms | Smallest source diff; one existing path | Already contradicted by output chunk, downstream calls, `take`, source order, and exception probes | Reject |
| Add one internal compatibility profile to the existing initializer/state machine | Keeps one engine and buffer; differences are named once at initialization; can share completion and state | Adds explicit map/filter-like/take modes and must avoid per-element policy dispatch where possible | **Choose as the simplest viable baseline** |
| Copy the four direct core lazy implementations and call transducer steps inside them | Likely easiest semantic translation | Recreates function-specific loops and defeats the simplicity hypothesis | Reject unless the shared-profile feasibility gate fails, in which case stop/replan rather than retain copies |
| Add separate hand-written Java driver classes for every function | Allows maximum specialization | Duplicated state/completion/error machinery and high upstream review cost | Experiment only if measurements first prove one shared profile materially inadequate; not a default slice |
| Leave core's implementation for a function that cannot earn a clean replacement | Preserves behavior and simplicity | Produces a negative adoption result | Required fallback |

The compatibility profile is not a user flag. It is a closed internal operation
kind selected by the public function: map-like, filter-like (`filter` and
`remove`), or incremental take. The implementation must keep one initializer,
one auditable state machine, one buffer, and no alternate public engine. Branch
placement is reviewed and measured; a loop copy is not introduced to avoid a
branch without direct evidence.

The profile must provide four explicit facilities only:

1. the take-count guard and exception order before source access;
2. source-advance/batch order required by the operation;
3. output flush shape (`chunk` for chunked map/filter-like batches, `Cons` for
   incremental take/dechunked batches);
4. one-shot failure termination matching direct core while leaving generic
   `xf-seq` unchanged.

If the implementation needs further operation-specific states, source
whitelists, buffer types, or copied completion paths to pass the oracle, stop
the slice and mark the plan `Needs replanning`.

### Transducer source

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| Delegate each transducer arity to `clojure.core` | Exact stable semantics, removes primitive analyzer coupling, prepares multi-input map | Primitive experiment no longer receives hints from the public #1 function | **Choose** |
| Keep analyzed xform factories on the public path | Preserves historical primitive metadata | Duplicates core semantics and is incompatible with ordinary sequence controls | Reject for #1; preserve analyzer/generator sources for Implementation #2 |
| Copy core transducer bodies into `xfseq.core` | Local control | Drift and no Phase 3 value | Reject |

### Operation-owned no-`Reduced` path

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| Always use the repaired reduced-aware compatibility loop | Simplest and supports `take` | Pays an `RT.isReduced` check for map/filter/remove | Baseline |
| Select no-reduced behavior for map/filter/remove once at initialization | Structural proof comes from the owned core operations; Phase 2 found one material vector win | Adds a product mode and may reverse on other sources/sinks | Benchmark candidate; promote only under the cross-source 5%/3% rule |
| Trust metadata or an arbitrary caller assertion | General but unsafe | Reintroduces the unproved Phase 2 escape hatch | Reject |

### Direct reduction result

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| Keep ordinary `LazySeq` | Exact standard surface and simplest upstream story | Full reductions allocate and traverse seq nodes | **Baseline and fallback** |
| Memoizing `ISeq` plus `IReduceInit`, with one shared state/cache | May reduce dispatch/allocation while preserving replay | High semantic, concurrency, retention, and maintenance risk | Conditional bench/test experiment only after diagnosis; positive evidence triggers replanning before promotion |
| Destructive reduction cursor | Fast and releases memory | Breaks retained-head replay | Reject; Phase 6 explicit fusion only |
| Weak/soft cache or runtime GC reachability test | Could appear to distinguish retained heads | Replay and side effects depend on GC | Reject |
| Eduction-style explicit wrapper | Clear reduction semantics | Not an ordinary lazy `ISeq`; changes the public function result | Keep as a benchmark control or future explicit API, not Phase 3 public output |

## Selected approach

1. Freeze direct Clojure 1.12.5 behavior in a reusable unary oracle fixture
   before repairing the candidate.
2. Delegate transducer arities to core and remove analyzer-generated xforms from
   the four #1 public paths without deleting the preserved primitive research.
3. Add the smallest closed unary compatibility profile to the existing object
   initializer/state machine. Keep the generic `xf-seq` constructor and Phase 2
   retry/buffer behavior intact.
4. Make all four public collection arities pass the direct oracle while the
   result is still an ordinary `LazySeq`.
5. Extend the existing JMH infrastructure with phase-aware manifests and a new
   direct-unary class. Preserve all Phase 2 tasks, identities, raw receipts, and
   validation behavior.
6. Run smoke, screen, decision, separate GC, and focused JIT/allocation evidence
   in the single local direct-linking-on lane.
7. Diagnose every repeatable regression over 3%. Test the operation-owned
   no-reduced candidate where applicable, but promote it only for a repeatable
   benefit of at least 5% in both a chunked and dechunked important cell,
   supported by fork/uncertainty and allocation data, with no repeatable
   supported regression over 3%.
8. Trigger an `IReduceInit` experiment only when the compatible LazySeq result
   has a repeatable full-reduction gap over 3% and JIT/allocation evidence
   identifies the ordinary seq reduction cursor as a material cause. A merely
   faster `eduction` or `transduce` row is not enough.
9. Build any result-type experiment only in benchmark/test scope. If it fails
   the result contract, two-cell 5% benefit rule, or 3% no-regression rule,
   remove it, keep `LazySeq`, and record the negative decision. If it passes,
   mark the plan `Needs replanning`; do not promote it under this run plan.
10. Report each function separately. A semantically correct but slower function
    stays useful as research evidence and receives `reject` or `investigate`,
    not an upstream adoption claim.

## Impact / Effort / Value priorities

| Item | Impact | Effort | Value | Dependency/evidence | Decision |
|---|---|---|---|---|---|
| Direct unary oracle fixture | High | Medium | High | Direct Clojure 1.12.5 source and traces | Now, before production repair |
| Core transducer delegation | High | Low | High | Parent decision and Phase 2 incompatibility fact | Now |
| Closed compatibility profile | High | High | High | Known output/order/error deltas | Now, semantic feasibility gate |
| `take` positive/non-positive/invalid contract | High | Medium | High | Direct source and planning probes | Now |
| Sparse filter/remove output-chunk preservation | High | Medium | High | Downstream call-count reversal | Now |
| Post-exception terminal behavior | High | Medium | High | Direct core versus Java thunk probe | Now |
| Direct-core retained/unretained JMH rows | High | High | High | Compatible LazySeq candidate | Now, after semantics |
| Operation-owned no-reduced mode | Medium to high | Medium | Medium | Applicable map/filter/remove rows | Decision-gated optimization |
| Memoizing `ISeq`/`IReduceInit` | Potentially high | High | Unknown | Material diagnosed reduction-cursor gap | Conditional experiment only |
| Source-specialized production dispatch | Medium | High | Low now | Still lacks whole-tail proof | Reject in Phase 3 |
| Multi-source map | High later | High | Low in Phase 3 | Unary phase completion | Phase 4 |
| Direct-linking-off / other JDKs | High later | High | Low in local decision | Promising local adoption set | Later upstream gate |
| Fusion / destructive drain | Separate | High | None for unary compatibility | Explicit API contract | Phase 6 |

Correctness and measurement outrank convenient optimizations. The
`IReduceInit` experiment is last even though it may be more novel.

## Confidence ledger

| ID | Kind | Decision, risk, or evidence | Resolution | Confidence |
|---|---|---|---|---|
| C1 | Fact | Current collection arities already route through `xf-seq`. | `src/xfseq/core.clj` inspection. | High. |
| C2 | Fact | Public transducer arities are analyzer-generated rather than core delegates. | `src/xfseq/core.clj` and Phase 2 benchmark failure note. | High. |
| C3 | Fact | Sparse chunked filter/remove output becomes `Cons` in the current buffer while direct core returns `ChunkedCons`. | Reproduced Clojure 1.12.5 class/protocol probe. | High. |
| C4 | Fact | That output-shape delta changes downstream mapper call counts after `first`. | Reproduced 4-calls-versus-1 trace. | High. |
| C5 | Fact | Direct `take` is incremental and unchunked, while current `xf-seq` batches five or more outputs. | Core source plus class/protocol probe. | High. |
| C6 | Fact | Direct `take` checks `pos?` before source `seq`; the current wrapper does not. | Core source plus zero/invalid probes. | High. |
| C7 | Fact | Direct dechunked filter advances the tail before calling the predicate; current engine calls the predicate first. | Traced custom `ASeq` probe. | High. |
| C8 | Fact | Direct one-shot lazy nodes and Java `AFn` engine thunks differ after exceptions. | Direct mapper/predicate retry probes and `LazySeq` bytecode inspection. | High. |
| C9 | Assumption | One closed compatibility profile can express all known direct semantics without copied loops. | Implement against the oracle; review state/class/branch count and full diff before checkpoint. | Medium until Slice 2. |
| C10 | Failure mode | Value and direct call-count tests pass, but sparse output changes a downstream consumer. | Assert output `IChunkedSeq` shape, exact chunk sizes, and downstream traces at selectivity boundaries. | High confidence in mitigation. |
| C11 | Failure mode | Catching a step exception matches the first throw but leaves partial buffered values or replays source/user code later. | Force the same initial and later lazy nodes again after source/function/predicate throws; compare prefixes and complete traces to direct core. | High confidence in mitigation. |
| C12 | Failure mode | `take` stops on `Reduced` before direct core's final `rest`, hiding a source effect or exception. | Custom dechunked and chunked sources record `first`, `rest`, chunk access, and exceptions for n around 0/1/chunk boundaries. | High confidence in mitigation. |
| C13 | Unknown | The compatibility profile's branches materially slow successful hot paths. | Direct public and focused Java JMH rows plus representative inlining/JFR evidence. | Low until decision run. |
| C14 | Assumption | Map/filter/remove supply a safe structural no-reduced proof. | They delegate to the owned Clojure 1.12.5 transducer implementations and no arbitrary xform reaches this mode; verify bytecode/call sites and tests. | High if the boundary stays closed. |
| C15 | Unknown | Removing analyzer xforms from public transducer arities affects preserved ASM/primitive research tests. | Run the complete suite; keep analyzer/generator namespaces intact; adjust historical tests to call an explicitly preserved research helper only if needed, without restoring analyzer coupling to #1. | Medium. |
| C16 | Failure mode | Direct-core JMH rows include Var dispatch for one side or eager setup for one implementation. | AOT wrappers for every direct call, `javap` linkage gate, source and explicit control xforms in setup, and symmetric sink methods. Xform creation owned by the public candidate remains inside its timed call. | High confidence in mitigation. |
| C17 | Failure mode | A retained-head row measures an already realized cached head in later invocations. | Construct a fresh head per invocation, store it in a state field before reduction, keep it reachable through the sink, and clear it at invocation teardown. | High confidence in mitigation. |
| C18 | Failure mode | An unretained row is mislabeled because a JMH field or Blackhole retains the head. | Keep the head local, return/consume only checksum, inspect JMH generated code/JFR, and keep retained and unretained methods separate. | Medium until harness inspection. |
| C19 | Unknown | A full-reduction gap is caused by the result cursor rather than the transducer loop or buffer. | Compare core unary, public candidate, generic `xf-seq`, `sequence`, `eduction`, `transduce`, Java loop rows, allocation, and JIT/JFR before triggering a result type. | Low until measured. |
| C20 | Failure mode | A custom `IReduceInit` path is fast only by skipping memoization or consuming a one-shot source twice. | Reduce/seq ordering, repeated/early reduction, one-shot, exception, retained-head, and concurrency tests gate promotion. | High confidence in detection. |
| C21 | Failure mode | Weak references make tests appear memory-efficient while replay depends on GC. | Ban weak/soft caches and inspect the complete production graph/diff. | High confidence in prevention. |
| C22 | Unknown | One compatibility result can allow unretained prefixes to die while preserving retained replay without extra machinery. | Bench/test prototype with strong references only, deterministic trace tests, and heap/allocation evidence. Failure keeps `LazySeq`; success forces replanning before promotion. | Low; deliberately non-critical. |
| C23 | Failure mode | Phase 3 generalization breaks Phase 2 manifest identities or raw reproducibility. | Keep Phase 2 classes/tasks/manifests accepted by the registry and rerun Phase 2 registry/linkage/smoke validation after harness changes. | High confidence in mitigation. |
| C24 | Fact | The local direct-on lane is sufficient for this research decision, not for publication/upstream. | Parent runtime/direct-linking policy. | High. |

Every phase-critical unknown has a resolution before the dependent decision.
The semantic fallback is to stop/replan; the performance fallback is ordinary
`LazySeq` and a negative per-function recommendation.

## Decisive experiments

1. **Transducer delegation:** compare the four public transducer arities with
   core for init, completion, step values, `Reduced`, invalid arity, and map's
   multi-input reducing step. Confirm the #1 public path no longer uses
   `xfseq.analyze`.
2. **Construction boundary:** traced source, mapper, predicate, and guarded
   take counter stay untouched until first realization.
3. **Chunk protocol:** for chunked sources and pass counts 0, 1, 2, 3, 4, 5,
   31, and 32, compare result node protocol, chunk sizes, and next-batch timing
   for map/filter/remove.
4. **Downstream composition:** place direct core `map`, `filter`, `take`, and
   partial sinks after each candidate and compare exact calls after `seq`,
   `first`, `next`, prefix consumption, and full realization.
5. **Dechunked source order:** traced custom `ISeq` records `seq`, `first`,
   `next`/`more`; compare ordering with mapper/predicate calls and thrown source
   methods.
6. **Take matrix:** n values include invalid objects, nil, negative values,
   zero, 1, 4, 5, 31, 32, 33, and values beyond source length. Run empty,
   dechunked, chunked, infinite, and throwing sources. Assert the final `rest`
   advance without reading the following item.
7. **One-shot exception behavior:** throw in source `seq`, `first`, `rest`,
   chunk access, mapper, and predicate in initial and later lazy nodes. Re-force
   the same node and compare prefix, empty-tail result, and trace.
8. **Surface and cache:** run `seq`, `first`, `next`, `rest`, `nth`, `count`,
   `vec`, `into`, reduce with/without init, equality, hash, `meta`/`with-meta`,
   printing, Java iteration, repeated realization, and concurrent forcing on
   empty and non-empty results.
9. **One-shot sources:** use fresh Java iterator/iterable and custom seqable
   fixtures per oracle/candidate run; never consume one fixture twice for a
   differential assertion.
10. **Linkage:** disassemble every Phase 3 AOT wrapper. Direct core must call
    `clojure.core$map/filter/remove/take.invokeStatic`; the candidate must call
    `xfseq.core$...invokeStatic`; no timed boundary may read a Var.
11. **LazySeq baseline:** screen and decide direct-core versus candidate across
    operations, sources, sizes, selectivity/take counts, sinks, and retained
    modes. Save cell-local raw scores, uncertainty, forks, and bytes/op.
12. **No-reduced candidate:** for map/filter/remove, compare reduced-aware and
    operation-owned no-reduced compatibility paths using the same public sink.
    Do not extrapolate from the older sequence-contract candidate alone.
13. **Reduction diagnosis:** for every repeatable full-reduction regression over
    3%, compare all controls and inspect representative allocation/inlining or
    JFR evidence. Name the dominant extra calls/objects before opening the
    custom result experiment.
14. **Conditional result prototype:** if triggered, implement strong-reference
    memoization in benchmark/test scope and run reduce-then-seq, seq-then-reduce,
    repeated/early reduction, one-shot source, completion, exception,
    concurrency, retained/unretained, allocation, and heap-reachability checks.
15. **Phase preservation:** rerun the Phase 2 exact manifest/registry/linkage
    tests and at least one non-overwriting Phase 2 smoke after any shared harness
    generalization.

## Ordered implementation slices

Workers run sequentially. The parent inspects and accepts each complete diff,
runs the named checkpoint, commits it, and records the SHA before the next
worker begins. A worker is not alone in the repository: it must preserve prior
accepted edits and all unrelated user state.

### Slice 1: direct unary oracle and core transducer surface

Ownership:

- `src/xfseq/core.clj` transducer arities and minimal public call-site setup;
- new `test/xfseq/unary_oracle_test.clj` and narrowly related test helpers;
- the active Phase 3 plan evidence for this slice.

Work:

1. Encode direct Clojure 1.12.5 values, trace order, output chunks, downstream
   calls, take validation/order, and one-shot exception facts in reusable fresh
   fixtures.
2. Delegate all four transducer arities to `clojure.core`.
3. Keep collection arities unary and keep Phase 4 arities absent.
4. Preserve analyzer/generator sources as historical/later-work code, but remove
   their use from the four #1 transducer paths.
5. Prove map's delegated transducer accepts the multi-input reducing step even
   though no multi-collection public arity is added.
6. Run focused tests and the full local check. Do not change Java engine or
   benchmark code in this slice.

Checkpoint: the oracle fixture is deterministic, the transducer surface matches
core, the full check passes, and no collection-arity compatibility claim is
made yet.

### Slice 2: compatible ordinary LazySeq unary driver

Ownership:

- `src-java/xfseq/ObjectXFSeqInit.java`;
- `src-java/xfseq/XFSeqStepSimple.java` and at most one small shared internal
  operation/profile definition;
- `src-java/xfseq/buffer/ObjectBuffer.java` only for an explicit chunk-versus-
  cons flush operation needed by the contract;
- `src/xfseq/core.clj` unary collection call sites;
- `test/xfseq/unary_oracle_test.clj`, with small focused additions to existing
  object-engine tests only where generic behavior must be protected;
- active plan evidence for this slice.

Work:

1. Add one closed internal map-like/filter-like/take profile selected once by
   the public wrapper. Preserve the existing generic constructor and semantics.
2. Implement exact advance/batch order, output flush shape, take precheck, and
   one-shot failure termination.
3. Keep one initializer, state machine, accumulator/completion implementation,
   and object buffer. No function-specific loop copy or public policy argument.
4. Pass the complete direct oracle, including sparse downstream calls, later
   node failures, custom seqs, and concurrent realization.
5. Inspect the production diff for added states, branches, and retained
   references. If the four declared facilities do not suffice, stop and mark
   `Needs replanning` rather than adding another mechanism.
6. Run focused tests, the Phase 2 engine/candidate suites, and the full check.

Checkpoint: all four collection arities have no known direct semantic delta and
still return ordinary `LazySeq`; generic `xf-seq` and all Phase 2 candidates
retain their tested contracts.

### Slice 3: Phase 3 direct-unary JMH harness

Ownership:

- new Phase 3 Java JMH class/support code under `bench/java/xfseq/bench/`;
- direct-linked wrappers in `bench/clj/xfseq/bench/calls.clj` or a separate
  Phase 3 calls namespace;
- phase-aware extensions to `bench/clj/xfseq/bench/registry.clj`, its tests,
  `runner.clj`, and `build.clj`;
- `bench/manifests/phase3-{screen,decision}.edn`;
- Phase 3 benchmark documentation and README command additions;
- no production semantic changes.

Work:

1. Add direct static call sites for core unary and candidate unary functions.
2. Add generic `xf-seq`, `sequence`, `eduction`, and direct `transduce` controls
   without timing reflective lookup or setup construction.
3. Add separate fresh-head `reduce-unretained` and `reduce-retained` methods.
   The retained method stores the newly constructed head in state before
   reduction and clears it only in invocation teardown.
4. Add exact applicable-cell manifests and retain Phase 2 identities/tasks.
5. Add Phase 3 non-overwriting result/environment paths and source hashes.
6. Verify JMH output consumption, fresh one-shot fixtures, AOT linkage, and
   result exactness; run a tiny smoke only. Make no speed claim.

Checkpoint: semantic gates, isolated build, linkage, manifest applicability,
smoke, raw receipt validation, and Phase 2 preservation all pass.

### Slice 4: direct-on screen, decision, allocation, and diagnosis

Ownership:

- immutable Phase 3 raw results, environment receipts, JIT/JFR evidence, and
  report data under `results/phase-3/`;
- manifest follow-up additions needed to cover every screen reversal;
- plan evidence and decision tables;
- no production edit unless a later conditional slice is authorized by the
  recorded gate.

Work:

1. Run screen, then expand the decision manifest to include every apparent
   regression over 3% and adoption-critical winner.
2. Run fresh three-fork decision and separate GC-profiled decision lanes.
3. Compare direct core with the compatible LazySeq candidate cell by cell.
4. Compare retained/unretained full reductions and all context controls.
5. Run focused Java/no-reduced rows and JIT/JFR evidence for representative
   wins and regressions.
6. Record whether the no-reduced promotion gate or result-type investigation
   trigger is met. A wide interval or isolated point estimate is not enough.

Checkpoint: every manifest identity validates, every regression is followed
up, raw evidence is immutable, causes are named where evidence supports them,
and the next conditional action is unambiguous.

### Slice 5: conditional operation-owned no-reduced promotion

Trigger: at least one public map/filter/remove operation shows a repeatable 5%
or larger benefit in both a chunked and a dechunked important cell, with
supporting fork/uncertainty or allocation evidence, no repeatable supported
regression over 3%, and no extra source classification.

Ownership if triggered:

- the unary compatibility profile/initializer only;
- focused semantic and benchmark tests;
- refreshed non-overwriting decision/GC/JIT receipts for every affected cell;
- active plan evidence.

Work if triggered:

1. Select non-reducing behavior solely from the owned public operation.
2. Keep arbitrary `xf-seq` reduced-aware and reject metadata/caller escape
   hatches.
3. Keep one initialization-selected reduced-check policy in the shared
   compatibility state machine. If promotion requires a copied loop, replan.
4. Re-run the full semantic gate, all affected decision cells, and adjacent
   source/size controls.
5. Revert/remove the promotion if the fresh result fails the 5%/3% rule.

If the trigger is not met, skip this slice and record the reduced-aware baseline
decision. A skipped slice creates no empty commit.

### Slice 6: conditional memoizing `ISeq`/`IReduceInit` experiment

Trigger: the compatible LazySeq candidate has a repeatable full-reduction
regression over 3%, and the multi-control/JVM diagnosis identifies ordinary seq
cursor/realization allocation as a material cause that a direct reduction path
can plausibly remove.

Ownership if triggered:

- benchmark/test-only result prototype and tests;
- focused benchmark class/manifest additions and fresh receipts;
- active plan evidence;
- no production result class or public initializer integration.

Work if triggered:

1. Use strong references and one shared transformation/cache state. No weak
   cache, destructive path, duplicated transform engine, runtime generation, or
   GC reachability branch.
2. Pass every additional reduction-result contract case before a public
   benchmark comparison is accepted.
3. Test whether it can demonstrate at least 5% benefit in two important
   source/size cells, no repeatable supported regression over 3%, and correct
   retained/unretained memory behavior.
4. Compare with direct core and the fastest applicable repaired Java baseline.
5. If any semantic, structural, or performance gate fails, remove the prototype,
   keep ordinary `LazySeq`, and record a negative result.
6. If every gate passes, preserve the evidence, mark the active plan
   `Needs replanning`, and stop. A new reviewed Phase 3 plan must define the
   production class, cache ownership, concurrency model, and integration diff.

If the trigger is not met, skip this slice and record why the gap was not a
result-type problem. A skipped slice creates no empty commit. A positive
prototype ends the current run at the replanning gate rather than advancing to
Slice 7.

### Slice 7: consolidation and run-stage handoff

Ownership:

- final Phase 3 documentation, decision table, commands, versions, raw paths,
  hashes, checkpoint SHAs, and agent log;
- cleanup of dead benchmark/test prototypes;
- no new mechanism.

Work:

1. Delete unpromoted experimental machinery while retaining raw evidence and
   named benchmark references needed to explain the decision.
2. Rerun focused unary tests, full `check`, Phase 2 preservation tests/smoke,
   Phase 3 linkage/manifest validation, and diff hygiene.
3. Audit every exit criterion and set status to `Awaiting final review`.
4. Stop. Do not start Phase 4 or declare the phase complete.

## Semantic validation

### Direct differential matrix

Use a fresh source for core and candidate. Source families:

- nil and empty collections;
- persistent list and traced custom `ISeq`;
- vector, subvector, and custom chunked seq;
- range;
- hash and sorted set;
- hash-map and sorted-map entries;
- object array;
- Java `Iterable` and fresh `Iterator` adapter;
- dechunked lazy seq;
- finite prefixes of `repeat` and `iterate`;
- custom `Seqable` whose `seq` records or throws;
- mixed chunked/dechunked tails.

Boundary sizes:

```text
0, 1, 2, 3, 4, 5, 7, 8, 9, 31, 32, 33, 63, 64, 65, 1,000
```

Operation parameters:

- map identity, small arithmetic, nil/false values, and functions that throw;
- filter/remove pass counts/selectivity around 0, 1, 4, 5, 31, and 32 per
  input chunk;
- take invalid/nil, negative, zero, 1, 4, 5, 31, 32, 33, source length, and
  beyond source length.

### Trace checkpoints

Compare events after:

- construction;
- `seq`;
- `first`;
- `next`/`rest`;
- a chunk boundary;
- prefix consumption;
- full consumption;
- repeated consumption;
- forcing the same initial or later node after an exception;
- placing another chunk-sensitive core function downstream.

Trace source `seq`, `first`, `next`/`more`, `chunkedFirst`, `nth`, and
`chunkedMore`; mapper/predicate calls; reducing steps/completion; and thrown
points.

### Sequence and reduction surface

For empty and non-empty results cover:

- `seq?`, `sequential?`, and the expected `IChunkedSeq` status of realized
  nodes;
- `first`, `next`, `rest`, `nth`, `count`;
- `vec`, `into []`, Java iteration;
- `reduce` with and without init and early reduced results;
- equality and hash equality with list/vector;
- `meta`, `with-meta`, and their realization timing;
- printing;
- repeated realization and reduction;
- concurrent forcing of the same node;
- reduce/seq ordering and retained-head replay if a custom result is tested.

### Failure matrix

Compare exception class, point, trace, retained prefix, and the next attempt for:

- source `seq`, `first`, `next`/`more`, `chunkedFirst`, chunk `nth`, and
  `chunkedMore`;
- mapper and predicate;
- invalid mapper/predicate arity and non-seqable input;
- invalid take count on empty and non-empty input;
- result completion/flush failure where a controlled test buffer permits it;
- initial and later lazy nodes.

The direct oracle decides even where behavior after an exception is surprising.

## Performance and direct-linking methodology

### Exact Phase 3 runtime matrix

| Lane | Clojure | Java/OS | Linking | Purpose |
|---|---|---|---|---|
| `phase3-screen-on` | library 1.12.5; CLI 1.12.5.1664 | Homebrew OpenJDK 26.0.2.1, arm64; macOS 26.2 / Darwin 25.2.0 | released core jar on; candidate/caller AOT on | Find direct-unary reversals and choose decision cells |
| `phase3-decision-on` | same | same, fixed G1 heap | same symmetric on mode | Local throughput decision |
| `phase3-decision-gc-on` | same | same, fixed G1 heap | same symmetric on mode | Separate allocation evidence for decision identities |

There is no direct-linking-off lane. Results are local research evidence, not a
publication or upstream runtime matrix.

Record CPU identity when available, processor count, heap, GC, JVM flags,
commit, dirty state and diff hash, JMH/core/caller jar hashes, benchmark source
hashes, exact argv, manifest hash, and result hash.

### JMH profiles

- **Smoke:** one fork, two 100-ms warmups, two 100-ms measurements; identity,
  output, linkage, and receipt validation only.
- **Screen:** two forks, at least three 1-second warmups and measurements.
- **Decision:** three fresh forks, five 1-second warmups and measurements,
  `-Xms2g -Xmx2g -XX:+UseG1GC`.
- **Decision GC:** the exact decision identities and JVM settings in a separate
  run with `-prof gc`.

One thread is the default throughput measurement. Concurrency belongs to
semantic tests unless a later diagnosed contention issue justifies a focused
benchmark.

### Implementations

1. Direct `clojure.core/map`, `filter`, `remove`, or `take`.
2. The public `xfseq.core` unary candidate with ordinary `LazySeq`.
3. Generic `xf-seq` with the same core transducer, kept distinct from the
   compatibility profile.
4. `sequence` with the same core transducer.
5. `eduction` with the same transducer and sink.
6. Direct `transduce` to the equivalent checksum/vector sink, without an
   intermediate result built in setup.
7. Applicable repaired Java candidates and the compatible reduced/no-reduced
   loop forms in focused rows.
8. A custom result prototype only after its trigger, always labeled experiment
   under this run plan.

Direct core is the external baseline. The fastest semantically applicable
repaired hand-written Java form is the internal baseline. A sequence-contract
candidate that does not reproduce direct output protocol is labeled diagnostic,
not silently called a compatible winner.

### Sources, sizes, operations, and sinks

JMH source families:

- list/dechunked lazy seq;
- vector and subvector;
- range;
- hash set;
- hash-map entries;
- object array;
- Java iterable;
- `repeat`/`iterate` with an explicit terminating operation/sink.

Sizes:

```text
0, 1, 4, 8, 31, 32, 33, 64, 1,000, 10,000, 1,000,000
```

Workloads:

- map identity, small arithmetic, and heavier function control;
- filter and remove selectivity near 0%, 1%, 50%, 99%, and 100%;
- take counts 0, 1, 8, 31, 32, 33, source length, and a small prefix of a
  large/infinite source.

Sinks:

- construct only;
- `first`;
- prefix 8;
- full seq traversal/checksum;
- `into []`/vector;
- full reduction/checksum with an unretained fresh head;
- full reduction/checksum with a fresh head retained in benchmark state;
- reduce then replay for focused semantics/performance diagnosis, not pooled
  with one-sink rows.

The retained/unretained pair proves call shape and measures throughput and
allocation under explicit reachability. It is not, by itself, proof that old
prefixes became collectible. Any result-type experiment also needs structural
cache inspection and focused heap/reachability evidence; it may not infer
retention behavior from equal or unequal bytes/op alone.

The screen and decision manifests are explicit applicable subsets, not the
blind Cartesian product. At minimum, each operation has dechunked and chunked,
small/boundary and steady-state, partial and full, retained and unretained
coverage. Every screen reversal over 3% receives a fresh decision cell.

### Interpretation and selection

- Report raw score, units, score error/confidence interval, fork samples, and
  bytes/op. Never average unlike cells.
- Investigate every repeatable regression over 3%.
- A local public function receives `adopt` only with no repeatable material
  regression in its supported primary cells and at least four 5%-or-larger
  throughput and/or allocation wins spanning a chunked and dechunked source, a
  small/boundary and steady-state size, and a partial and full sink. Fewer wins
  yield at most `investigate`, even when the geometric story looks promising.
- A no-reduced mechanism needs at least a repeatable 5% benefit in both a
  chunked and dechunked important cell for the owned operation, supporting
  uncertainty/fork and allocation evidence, and no repeatable supported
  regression over 3%. A result prototype needs the same 5% benefit in at least
  two important source/size cells spanning chunked and dechunked input.
- Partial consumption, full traversal, retained reduction, and unretained
  reduction remain separate conclusions.
- A faster `eduction`/`transduce` row shows the cost of lazy materialization; it
  does not prove that a custom public result can preserve memoization.
- A negative result is valid. Do not add flags, source dispatch, loop copies,
  weak caches, or generated machinery to rescue one cell.

## Evidence layout

Expected durable artifacts:

```text
results/phase-3/
  semantic/
    unary-oracle.edn
  bench/
    smoke-<commit>-<run-id>.json
    screen-<commit>-<run-id>.json
    decision-<commit>-<run-id>.json
    decision-gc-<commit>-<run-id>.json
  environment-<profile>-<commit>-<run-id>.edn
  jit/
    <cell>-<commit>-<run-id>.log
  profiles/
    <cell>-<commit>-<run-id>.*
```

Every run ID is explicit for decision evidence. Durable writers use
create-new/non-overwriting semantics. The active plan records commands, exit
codes, row counts, hashes, raw paths, interpretation, and any interrupted or
rejected run.

## Exit criteria

1. The four transducer arities delegate to core and the #1 public path no longer
   depends on analyzer-generated xforms.
2. The four supported unary collection arities route through one closed object
   compatibility boundary and return ordinary `LazySeq`.
3. Construction is lazy and xform application/completion are exactly once on
   successful paths.
4. Direct differential values pass across the complete Phase 3 source, size,
   operation, and take-count matrices.
5. Source advance, mapper/predicate order, input batching, output chunk
   protocol, chunk size, and downstream calls match direct core.
6. Non-positive and invalid `take` behavior, including source and exception
   order, matches direct core.
7. Initial and later source/function/predicate exceptions match in class,
   point, prefix, trace, and same-node post-exception behavior.
8. Standard seq, metadata, equality/hash, iteration, reduction, caching,
   one-shot source, and concurrency tests pass for empty and non-empty results.
9. Generic `xf-seq` and every retained Phase 2 object candidate keep their
   existing tested contracts; primitive/ASM/fusion paths are not pulled in.
10. The full local `check` passes with no lint finding or unexpected compiler
    reflection warning.
11. Phase 3 JMH smoke, screen, decision, and separate allocation receipts pass
    semantic, linkage, applicability, exact-identity, metric, and
    non-overwriting validation.
12. Direct core, public candidate, generic `xf-seq`, `sequence`, `eduction`,
    `transduce`, and applicable Java baselines are represented where required.
13. Retained and unretained full-reduction rows use fresh heads and have
    reproducible throughput and allocation evidence.
14. Every repeatable regression over 3% is followed up and named; every added
    mechanism satisfies its 5%/3% gate or is absent from product code.
15. Any `IReduceInit` prototype stays outside product code. A negative result is
    recorded and removed; a positive result passes the complete additional
    contract and ends the run at `Needs replanning` before promotion.
16. Each function has an evidence-backed `adopt`, `investigate`, or `reject`
    recommendation under the predeclared four-cell coverage rule. No pooled
    speedup or general upstream claim is made.
17. Commands, runtime versions, raw paths/hashes, decisions, checkpoint SHAs,
    skipped conditional slices, and sequential agent runs are recorded here.
18. The normal run ends at `Awaiting final review`. A positive custom-result
    prototype ends instead at `Needs replanning`. No Phase 4 work begins in
    either case.

Failure to implement the compatibility profile with only the four declared
facilities is a replanning trigger, not permission to weaken an oracle. A
performance rejection for one or more functions is not a semantic phase
failure; it is the required adoption result.

## Decision log

| Date | Decision | Reason |
|---|---|---|
| 2026-09-01 | Treat Phase 3 as semantic feasibility before adoption timing. | Current wrappers already route through the engine but direct probes show observable incompatibilities. |
| 2026-09-01 | Use direct Clojure 1.12.5 functions as primary oracles; `sequence` remains context only. | Values alone hide chunk, source-order, downstream, and exception differences. |
| 2026-09-01 | Delegate transducer arities to core. | The analyzer path belongs to Implementation #2 and already prevented a symmetric Phase 2 control. |
| 2026-09-01 | Choose one closed compatibility profile as the simplest viable baseline. | Naïve generic routing is disproved; copied function loops would defeat the central simplicity hypothesis. |
| 2026-09-01 | Keep generic `xf-seq` behavior unchanged. | Phase 2 completed against its own generic oracle; direct unary exception/output rules are function-specific. |
| 2026-09-01 | Preserve direct `take` item-at-a-time behavior, including final `rest`. | The direct oracle contradicts generic chunk/reduced behavior. |
| 2026-09-01 | Treat take's pre-source count guard as an explicit compatibility facility. | Invalid and non-positive counts must be decided before source `seq`; hiding the guard in a wrapper would make the state boundary harder to audit. |
| 2026-09-01 | Require chunked sparse filter/remove output. | It changes both an explicit protocol and downstream mapper calls. |
| 2026-09-01 | Benchmark reduced-aware LazySeq first. | No-reduced and `IReduceInit` machinery need evidence from the exact semantic candidate. |
| 2026-09-01 | Permit no-reduced selection only from owned map/filter/remove operations. | This supplies a structural proof without reopening an arbitrary metadata escape hatch. |
| 2026-09-01 | Reject weak/soft caches and destructive reduction. | Retained-head replay and side effects cannot depend on GC or caller discipline. |
| 2026-09-01 | Require replanning before any custom result reaches production. | A new memoizing `ISeq` concurrency/cache design is a material redesign, not a conditional optimization detail. |
| 2026-09-01 | Reuse the Phase 2 5% benefit / 3% regression rule for added machinery. | It prevents noisy point estimates from buying permanent complexity. |
| 2026-09-01 | Keep the local performance lane direct-linking-on only. | It matches the released core jar; a symmetric off lane requires a separate core build and belongs later. |
| 2026-09-01 | Preserve Phase 2 benchmark identities and receipts while generalizing tooling. | Earlier evidence must remain reproducible and interpretable. |

## Planning validation evidence

| Check | Evidence |
|---|---|
| Required workflow | Read `xfseq-phase`, `approximate`, `confidence`, `prioritize`, `review-plan`, `xfseq-review`, and `plain-english` completely. |
| Parent source | Read the complete 967-line `docs/01-transducer-backed-lazy-seqs.md`, including Phase 3, semantic, benchmark, direct-linking, acceptance, risk, and decision sections. |
| Repository state | `git status --short` empty; HEAD `5e6e855...`; Phase 0, 1, and 2 plans all `Complete`. |
| Current build | `clojure -Srepro -M:test` through the test alias ran the complete discovered suite: 29 tests / 2,906 assertions / 0 failures / 0 errors. |
| Runtime | `java -version`: Homebrew OpenJDK 26.0.2.1; `clojure -Sdescribe`: CLI 1.12.5.1664; dependency path contains Clojure 1.12.5; `sw_vers`/`uname`: macOS 26.2, Darwin 25.2.0, arm64. |
| Core oracle source | Inspected installed Clojure 1.12.5 `core.clj` definitions of `sequence`, `map`, `filter`, `remove`, and `take`, plus `LazySeq` bytecode. |
| Production path | Inspected `src/xfseq/core.clj`, `ObjectXFSeqInit`, canonical and retained object steps, `ObjectBuffer`, analyzer/generator boundary, existing object tests, Phase 2 candidates, JMH callers/support/registry/build, manifests, raw-result interpretation, and Phase 2 final handoff. |
| Output protocol probe | Direct/core versus generic engine confirmed sparse filter/remove `ChunkedCons` versus `Cons`, and take 5+ `Cons` versus `ChunkedCons`. |
| Downstream trace | First of a core map over four sparse outputs invoked mapper four times for direct filter and once for current engine filter. |
| Take guard | Direct `take 0` left a traced source untouched while current engine called source `seq`; direct invalid n on empty threw `ClassCastException` while current engine returned empty. |
| Dechunked trace | Direct filter recorded `first,next,predicate`; current engine recorded `first,predicate,next`. Direct take 1 advanced `next`; current engine did not. |
| Exception probe | Direct core's failed initial chunk became empty on re-force; Java engine reinitialized and replayed. Existing Phase 2 tests separately show engine continuation retry behavior. |
| Result protocol | Direct map, current xfseq, and `sequence` return `LazySeq` without `IReduceInit`; `eduction` implements `IReduceInit` and is not an `ISeq`. |

## Plan review findings

### Review 1: semantics, conditional scope, and measurement

Verdict: revise, then review again.

#### Uncertain decisions

1. **Can a custom memoizing result be promoted inside this run plan?** The
   options were conditional promotion after benchmarks or a benchmark/test
   prototype followed by replanning. Conditional promotion would leave the
   cache graph, concurrency model, and public integration to a worker after the
   pre-implementation gate. Recommendation: prototype only; positive evidence
   marks Phase 3 `Needs replanning`. The plan now adopts that boundary.
2. **Do retained/unretained JMH bytes prove memory-release behavior?** A state
   field makes reachability explicit, but allocation rate is not retained-heap
   proof. Recommendation: keep both required rows, label their conclusion
   narrowly, and require structural cache plus focused heap/reachability
   evidence for a custom result. Added.
3. **Does setup symmetry accidentally remove candidate-owned xform cost?** Core
   direct functions do not construct a transducer, while the public candidate
   does. Recommendation: build explicit control xforms in setup, but leave the
   public candidate's own xform creation in the timed call. Clarified in the
   confidence ledger and harness slice.

#### Confident changes

A. Corrected the re-forcing typo in the known-delta table.

B. Kept the direct oracle strict even for surprising post-exception behavior;
the mission names direct Clojure behavior as the oracle.

C. Retained one closed compatibility profile rather than separate copied
function loops. The feasibility stop remains explicit if three declared
facilities are insufficient.

D. Kept operation-owned no-reduced work separate from arbitrary `xf-seq` and
from the larger result-type question.

E. Preserved the direct-linking-on-only local lane and Phase 2 identities; no
timing or compatibility evidence requires an off-built core in this phase.

F. Narrowed the custom-result exit criterion: negative experiments disappear;
positive experiments stop for replanning before product code changes.

Review 1 caused a material scope correction, so Review 2 is required.

### Review 2: post-revision execution check

Verdict: pass with no unresolved phase-critical finding.

#### Uncertain decisions

1. **What ends the run after a positive result prototype?** The revised Slice 6
   said `Needs replanning`, while the generic exit criterion still required
   `Awaiting final review`. Recommendation: make the two terminal paths
   explicit. Corrected.
2. **What counts as broad enough local adoption evidence?** “Enough source,
   size, and sink shapes” allowed post-hoc judgment. Recommendation: require at
   least four 5%-or-larger wins spanning chunked/dechunked, small/steady-state,
   and partial/full dimensions, with no repeatable regression over 3%. Added.
3. **Can a no-reduced win on one source justify a universal operation mode?**
   No; the mode applies across sources. Recommendation: require a 5%-or-larger
   win in both a chunked and dechunked important cell and forbid a copied loop.
   Added.

#### Confident changes

A. The exact direct-oracle differences are resolved before timing and cannot be
waived by a performance result.

B. Slice 1 and Slice 2 separate core transducer delegation from the Java
compatibility repair, giving the parent a clean semantic checkpoint.

C. The harness keeps public candidate-owned xform creation timed, uses direct
AOT call sites on both sides, and preserves Phase 2 receipts and identities.

D. Retained/unretained rows have a narrow interpretation and cannot substitute
for reachability evidence.

E. No-reduced optimization remains a small shared-state policy; a copied loop
or source classifier is a replanning trigger.

F. A custom result remains benchmark/test-only in this plan. Negative evidence
returns to `LazySeq`; positive evidence stops for a new reviewed design.

No findings conflict, and no user decision is required before implementation.

## Pre-implementation review

Date: 2026-09-01

The smallest accurate mental model is one ordinary `LazySeq`, one deferred
object engine, and one closed operation profile that changes only the four
behaviors direct core makes observably different: the take guard, source/batch
order, output flush shape, and one-shot failure termination. Performance work
starts only after that result matches direct core.

### Findings by severity

**Blockers:** none after the correction below.

**Medium impact, fixed before passing:** the reviewed plan limited the
compatibility profile to three facilities but separately required `take` to
check `pos?` before source `seq`. That guard needs the original count at the
deferred boundary and cannot be smuggled into generic batch behavior. The plan
now names four facilities explicitly and makes any fifth operation mechanism a
replanning trigger.

**Low impact, already bounded:** direct core's one-shot behavior after an
exception is surprising and differs from Phase 2 generic retry tests. The plan
does not generalize either behavior: direct unary profiles match core, generic
`xf-seq` retains its completed Phase 2 contract, and both paths have explicit
regression suites.

**Low impact, already bounded:** retained/unretained JMH allocation cannot prove
heap reachability. The plan labels those rows narrowly and requires structural
cache plus focused heap/reachability evidence before even a benchmark/test
custom result can pass. Production promotion is outside this reviewed plan.

### Gate assessment

| Gate | Result |
|---|---|
| Problem validity | Pass. Direct probes prove the current unary route is observably incompatible; repairing and measuring it is the necessary Phase 3 problem. |
| Semantic fidelity | Pass for planning. Direct core is the primary oracle, with explicit values, source order, input/output chunks, downstream calls, invalid take, exceptions and re-forcing, sequence surface, reduction, one-shot sources, metadata, and concurrency coverage. |
| Performance validity | Pass. The exact local lane is symmetric direct-on, AOT-linkage checked, forked, allocation-aware, manifest-exact, retained/unretained aware, cell-local, and governed by predeclared 3%/5%/four-cell rules. |
| Structural simplicity | Pass. The baseline is one existing engine plus four named compatibility facilities. Copied loops, source classifiers, arbitrary no-reduced claims, generated machinery, weak caches, and production custom results are rejected or force replanning. |
| Hot-path quality | Pass for planning. Reduced-aware LazySeq is measured first; applicable Java candidates, no-reduced forms, allocation, JIT/inlining, and JFR diagnose costs before any promotion. |
| Upstream fitness | Pass. The plan can produce a clean negative result per function, preserves Phase 2 evidence and later-phase code, and does not treat semantic sharing alone as an adoption case. |

Verdict: `ready for implementation`.

### What matters

- Direct core—not `sequence`—decides unary behavior.
- Four known compatibility differences are explicit; a fifth mechanism forces
  replanning.
- The first product result stays an ordinary `LazySeq`.
- Direct-on JMH compares direct core and the exact semantic candidate with
  retained and unretained full reductions.
- No-reduced behavior needs cross-source evidence; arbitrary callers cannot
  select it.
- A custom result is prototype-only; positive evidence stops for replanning.
- A slower function receives a negative adoption recommendation, not rescue
  machinery.

## Agent run log

| Date | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-09-01 | Initial planning | `/root` (`gpt-5.6-sol`, high) | Read the required skills and complete parent design; inspected the Phase 2 final handoff, production engine/buffer/public API, direct Clojure 1.12.5 source/bytecode, tests, benchmark harness/manifests/results; ran the clean suite and direct output/order/exception/result-protocol probes. | Draft created with the naïve route rejected, one compatibility-profile baseline, direct-core measurement, and evidence-gated no-reduced/result-type work. No production code changed. |
| 2026-09-01 | Plan review 1 | `/root` (`gpt-5.6-sol`, high) | Re-read the complete Phase 3 draft and relied-on production/test/JMH sources; reviewed semantic oracle strictness, compatibility-profile complexity, conditional optimization boundaries, retained/unretained interpretation, setup/linkage symmetry, and Phase 2 preservation. | Required result-type promotion to stop for replanning, narrowed retained-head claims, and clarified timed candidate-owned xform construction. Material revision requires Review 2. |
| 2026-09-01 | Plan review 2 | `/root` (`gpt-5.6-sol`, high) | Rechecked the revised phase boundary, terminal states, conditional triggers, worker ownership, direct-on symmetry, adoption thresholds, fallback paths, and every exit criterion. | Passed after making the positive-prototype terminal path explicit and predeclaring cross-source/four-cell performance gates. No unresolved phase-critical decision. |
| 2026-09-01 | Pre-implementation review | `/root` (`gpt-5.6-sol`, high) | Applied the strict problem, semantic, performance, simplicity, hot-path, and upstream gate to the fully revised plan and current repository evidence. | Fixed the omitted take-guard facility in the structural boundary; no remaining blocker. Verdict `ready for implementation`. No production code changed. |

## What matters

- The current wrappers produce correct basic values but are not direct-core
  compatible yet.
- Sparse filter/remove output, take batching, source order, and failure replay
  are observable blockers, not benchmark details.
- The first implementation target is still an ordinary `LazySeq` using one
  closed compatibility profile, not a custom result type.
- Core transducers replace the analyzer-generated public xforms; primitive
  research remains preserved but outside #1.
- Direct-core, retained/unretained, direct-on JMH evidence decides whether any
  function is worth proposing.
- No-reduced and `IReduceInit` paths are conditional and disappear if they do
  not clear both semantic and 5%/3% performance gates.
- Phase 3 ends with per-function evidence and stops before multi-source map.

## Slice 1 implementation evidence

Slice 1 was accepted by the parent and checkpointed at `03af0e7`. It changes
only the public Clojure transducer surface, test/oracle support, and this plan;
the Java engine and benchmark sources are unchanged.

| Check | Evidence |
|---|---|
| Parent preflight baseline | Before this slice, parent `/root` recorded HEAD `ab696eeab430659f2d9bcdc5bd6e5d18ac6148f8`, a clean worktree, and `clojure -Srepro -T:build check` passing with lint 0/0, clean reflection, 29 tests / 2,906 assertions / 0 failures / 0 errors. |
| Preserved performance baseline | The Phase 2 forked decision and separate GC receipts remain the preserved pre-change performance baseline. Phase 3 has no direct-unary harness until Slice 3, so this slice makes no performance claim. |
| Focused oracle | `clojure -Srepro -M:test -n xfseq.unary-oracle-test` passed: 10 tests / 796 assertions / 0 failures / 0 errors. The fixture covers fresh ordered sources, direct values, chunk sizes and node protocols, sparse filter downstream demand, dechunked source/mapper/predicate order, take validation and final `rest`, and one-shot source/function failures. |
| Focused regression pair | `clojure -Srepro -M:test -n xfseq.core-test -n xfseq.unary-oracle-test` passed: 11 tests / 842 assertions / 0 failures / 0 errors. This includes the preserved generated primitive-path checks alongside the new direct-core oracle. |
| Full local check | `clojure -Srepro -T:build check` passed: lint 0/0, reflection clean, 39 tests / 3,702 assertions / 0 failures / 0 errors. The ten added unary-oracle tests pass, and the historical generated-map helper keeps the existing primitive-path assertions valid. |
| Delegated transducer surface | `xfseq.core/map`, `filter`, `remove`, and `take` one-argument forms now return the corresponding `clojure.core` transducers. The oracle directly applies fresh transformed reducing functions, compares zero-arity init, ordinary steps, completion, reduced flags/unwrapped values, and invalid arity classes, and directly invokes map's multi-input reducing step for 2, 3, and 5 input values. |
| Historical primitive path | Analyzer/generator source namespaces remain intact. A test/benchmark-only `historical-map-xform` preserves the old analyzer-shaped metadata for `gen/xf-seq` regression tests; the four `xfseq.core` #1 transducer paths do not call it. |
| Scope audit | No `src-java` or benchmark file was edited. Unary collection arities remain the only public collection arities; multi-source map remains absent. |

### Slice 1 run log

| Date | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-09-01 | Parent preflight | `/root` | Captured the clean Phase 3 starting point and preserved the Phase 2 forked decision/GC receipts as the only pre-change performance baseline because the direct-unary harness is deferred to Slice 3. | HEAD `ab696eeab430659f2d9bcdc5bd6e5d18ac6148f8`; baseline check 29 / 2,906, lint 0/0, reflection clean. |
| 2026-09-01 | Implementation Slice 1 | `/root/phase3_slice1` | Replaced analyzer-backed public map/filter/remove/take transducer factories with direct core delegates; added fresh unary oracle fixtures/tests; preserved the analyzer-backed generated map behavior in a test-only historical helper so existing generator coverage stays valid. | Initial focused oracle passed 9 / 789. Initial full check exposed two expected historical generator primitive-path assumptions; the helper/test redirection resolved them without restoring analyzer coupling to the #1 path. |
| 2026-09-01 | Oracle coverage follow-up | `/root/phase3_slice1` | Replaced the explicit-init `transduce` snapshot with direct fresh transformed-reducing-function probes, covering zero-arity init, ordinary steps, completion, early `Reduced` behavior for `take`, and invalid xform/fixed-step arities; retained the explicit multi-input map proof. | Focused oracle passed 10 / 796; no production or benchmark scope change. |
| 2026-09-01 | Slice 1 verification | `/root/phase3_slice1` | Reran the complete local check and reviewed the diff for Java/benchmark scope, unary arities, direct-core delegation, and preserved analyzer/generator sources. | Focused core + oracle passed 11 / 842; `clojure -Srepro -T:build check` passed with lint 0/0, reflection clean, 39 tests / 3,702 assertions / 0 failures / 0 errors. Awaiting parent inspection and checkpoint commit; no performance claim. |
| 2026-09-01 | Slice 1 parent checkpoint | `/root` | Inspected the complete diff, returned one inaccurate init-coverage claim for correction, then independently reran the focused pair, full check, and diff hygiene. | Accepted at `03af0e7`; focused 11 / 842 and full 39 / 3,702 passed with lint 0/0, reflection clean, and `git diff --check` clean. |

## Slice 2 implementation evidence

Slice 2 was accepted by the parent and checkpointed at `e33173c`. It keeps one ordinary `LazySeq`, initializer, mixed state machine,
accumulator/completion path, and `ObjectBuffer`. The public unary collection
wrappers select one closed `UnaryProfile` (`MAP_LIKE`, `FILTER_LIKE`, or
`TAKE`) at construction. The profile supplies only the four facilities named
by this plan: take's deferred count guard, operation-specific source/batch
order, explicit chunk-versus-Cons flushing, and one-shot failure termination.
No source classifier, public policy flag, copied function loop, extra buffer,
or reduction protocol was added. Generic `xf-seq` retains its Phase 2
behavior and constructor shape.

The focused transducer oracle was also strengthened during this checkpoint.
Each comparison creates a fresh direct-core or `xfseq.core` transducer and
directly invokes its transformed reducing function's zero-arity initializer,
ordinary steps, completion, and invalid arity behavior. A separate fresh
`take` probe makes the sink return `Reduced` on its second step and compares
the unwrapped completion result and event trace. The existing fresh map
multi-input step proof remains in place; no multi-collection public arity was
added.

| Check | Evidence |
|---|---|
| Focused direct/candidate oracle | `clojure -Srepro -M:test -n xfseq.unary-oracle-test` passed: 24 tests / 1,699 assertions / 0 failures / 0 errors. It compares direct and candidate values over fresh nil/empty/list/vector/subvector/range/array/iterable/iterator/lazy sources, chunk/node shape including sparse output, downstream mapper demand, dechunked source order, final-take `rest` failure and one-shot behavior, remove predicate failure, invalid mapper/predicate arity, non-seqable input, initial/later failures and one-shot tails, custom dechunked `first`/`next` failures, initial/later custom chunkedFirst/chunk nth/chunkedMore failures, ordinary all-four-function sequence surface/cache/metadata/printing/iteration/reduction/early-reduced behavior, a fresh one-shot iterator, and concurrent forcing. Every lazy exception snapshot forces the same node four times; invalid `take` counts repeat their exact class four times without source access, while positive `take` source/first failures become empty after the first exception. |
| Transducer delegation oracle | The same focused run directly compares fresh transformed reducing functions for zero-arity init, ordinary steps, completion, `Reduced` propagation for `take`, invalid xform arity, invalid fixed-step arity/classes, and map's 2/3/5-input reducing steps. |
| Phase 2 regression suites | `clojure -Srepro -M:test -n xfseq.object-engine-test -n xfseq.object-candidate-test` passed: 28 tests / 2,860 assertions / 0 failures / 0 errors. Generic engine retry/completion/accumulator behavior and all retained Phase 2 candidate identities remain covered. |
| Full local check | `clojure -Srepro -T:build check` passed: lint 0/0, reflection clean, 53 tests / 4,605 assertions / 0 failures / 0 errors. |
| Scope and hygiene | Production changes are limited to `ObjectXFSeqInit.java`, `XFSeqStepSimple.java`, `ObjectBuffer.java`, `UnaryProfile.java`, and unary collection call sites in `src/xfseq/core.clj`; tests are confined to `unary_oracle_test.clj`. No Java benchmark or benchmark source changed. `git diff --check` passed. |
| Complexity audit | The diff adds one profile enum, profile selection at the public wrapper boundary, one failure bit at each existing lazy realization boundary, and one explicit `ObjectBuffer` chunk flush. Existing generic pending states, one buffer, and one completion path remain shared. Failed compatibility initializers/steps clear source, xform, buffer, accumulator, and pending references; reflection tests verify this and direct one-shot closures clear their captured source state. The invalid-count guard deliberately retains initializer callable state for repeated class-identical failures; a successful take step whose final `rest` fails clears source/xform state while preserving direct core's first exception followed by repeated `NullPointerException`. The four declared facilities sufficed; no replanning trigger was reached. |
| Performance baseline | No Phase 3 performance claim is made. The parent preflight baseline and preserved Phase 2 forked decision/GC receipts recorded above remain the only pre-Slice-3 performance evidence. |

### Slice 2 run log

| Date | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-09-01 | Implementation Slice 2 | `/root/phase3_slice1` | Added the closed unary compatibility profile, selected it from the four unary collection wrappers, repaired direct source/batch order and chunk/Cons flush shape, added the deferred `take` guard, and made only compatibility-profile lazy nodes one-shot after failure. Kept generic `xf-seq` and Phase 2 candidates on the existing path. | Candidate implementation passed the direct oracle and Phase 2 engine/candidate suites; no fifth compatibility mechanism was needed. |
| 2026-09-01 | Transducer oracle follow-up | `/root/phase3_slice1` | Added a fresh transformed-RF `take` probe with early `Reduced` and completion, expanded invalid step-arity coverage to include map's invalid mapper arity while retaining the multi-input proof, and kept all transducer comparisons direct against core. | Focused oracle passed 24 / 1,722 with no failures; no public multi-source collection arity was introduced. |
| 2026-09-01 | Semantic coverage follow-up | `/root/phase3_slice1` | Added fresh direct-vs-candidate cases for final-take rest failure, remove predicate failure, invalid mapper/predicate arity, non-seqable input, custom dechunked failures, all requested initial/later custom chunked failures, all-four-function surface/reduction/early-reduced behavior, and engine reference release. | Focused oracle passed 24 / 1,722; no additional production mechanism beyond the existing profile failure termination was needed. |
| 2026-09-01 | Slice 2 verification | `/root/phase3_slice1` | Ran the focused unary oracle, Phase 2 object engine/candidate suites, full build check, and diff hygiene; reviewed production state/branch/reference additions against the four-facility boundary. | Focused 24 / 1,722; Phase 2 pair 28 / 2,860; full 53 / 4,628; lint 0/0; reflection clean; `git diff --check` clean. Ready for parent inspection; no commit made. |
| 2026-09-01 | Take replay correction | `/root/phase3_slice1` | Changed every lazy exception snapshot to four same-node forces; moved invalid-count validation outside compatibility failure termination so count/type errors retain callable state; kept successful take `rest` failures retryable only for the deterministic repeated-NPE shape while clearing source/xform references; added positive take source-seq/first failure coverage. | Focused 24 / 1,699; Phase 2 pair 28 / 2,860; full 53 / 4,605; lint 0/0; reflection clean; `git diff --check` clean. No fifth compatibility mechanism was needed; uncommitted for parent review. |
| 2026-09-01 | Slice 2 parent checkpoint | `/root` | Inspected the full production/test diff, returned missing failure/surface/reference cases and then a two-force `take` replay blind spot, probed four-force direct behavior, and independently reran the semantic checkpoint. | Accepted at `e33173c`; combined unary plus Phase 2 suites passed 52 / 4,559, full check passed 53 / 4,605, and lint, reflection, and diff hygiene were clean. |

## Slice 3 implementation evidence

Slice 3 was accepted by the parent and checkpointed at `d020b2a`. It adds only
the isolated Phase 3 direct-unary benchmark harness and its
registry/manifests/receipts. It does not change production semantics or add a
performance/adoption claim. The Phase 2 benchmark identities, tasks, jar and
result namespace remain intact; the preservation smoke below uses a distinct
run ID and path. The timed benchmark stores one implementation/operation
construction plan during setup, and the non-timed trial validates complete
values/checksums on fresh fixtures before measured one-shot sources are used.

| Check | Evidence |
|---|---|
| Parent preflight baseline | The parent recorded the clean starting HEAD `ab696eeab430659f2d9bcdc5bd6e5d18ac6148f8`; `clojure -Srepro -T:build check` passed with lint 0/0, reflection clean, 29 tests / 2,906 assertions / 0 failures / 0 errors. The Phase 2 forked decision and separate GC receipts are preserved as the pre-change performance baseline because the Phase 3 direct-unary harness did not exist until this slice. |
| Phase 3 AOT/linkage | `clojure -Srepro -T:build phase3-bench-linkage '{}'` and `phase3-bench-jar '{}'` passed. The final post-check linkage report `target/bench/phase3-linkage-0ae5c77622d860912c57b14dab04c3aa3e7c6219.txt` (SHA-256 `a6c5da47734af5308d10032d867c8f15291718f7ceea038412396fee57ca32f4`) disassembles every direct construction wrapper, generic control wrapper, and timed `_firstValue`, `_vectorValue`, and `_reduceChecksum` sink helper; it rejects `Var` lookup and contains all eight required `clojure.core$...invokeStatic` / `xfseq/core$...invokeStatic` calls. The post-check isolated jar `target/bench/xfseq-phase3-jmh.jar` has SHA-256 `bffc0f7bce14f5dda1dd1676f081ddedf15de67d671b0e97f63f6c7f92372293`; `java -jar ... -l` listed all seven `Phase3UnaryBenchmark` methods. |
| Registry and manifests | The focused benchmark registry suite passed 7 tests / 46 assertions. The runner accepted explicit Phase 3 screen and decision manifests: 24 cells / 192 identities each; every cell names exactly `{implementation, operation, sourceKind, size}`, uses `Phase3UnaryBenchmark` and the six direct/control implementations, and each operation covers list/vector small/boundary/steady-state partial/full plus retained/unretained reductions. Negative tests reject omitted keys, malformed/missing params, missing iterator coverage, and missing required sinks. |
| Full semantic/build check | `clojure -Srepro -T:build check` passed: lint 0/0, reflection clean, 53 tests / 4,605 assertions / 0 failures / 0 errors. `git diff --check` passed. |
| Non-timed trial | `clojure -Srepro -T:build phase3-bench-trial '{}'` passed `Phase 3 trial validation passed: 72 fresh implementation/source cases`, covering all six implementations, all four operations, and fresh list/vector/iterator sources with complete values and checksums compared to direct core. The trial runs before JMH invocation setup and cannot consume measured one-shot fixtures. |
| Phase 3 smoke | `clojure -Srepro -T:build phase3-bench-smoke '{:run-id "slice3-review-20260901"}'` passed the semantic/build/linkage gates and the 72-case trial, ran one fork with two 100-ms warmups and measurements for all six implementations across all four list `first` operations plus fresh iterator map `reduceUnretained`/`reduceRetained`, and validated 36 rows. The immutable result is `results/phase-3/bench/smoke-0ae5c77622d860912c57b14dab04c3aa3e7c6219-slice3-review-20260901.json` (SHA-256 `a06475113283b1ce45234bdf9fed56a35ff83ac8b1159ab79f63e6b8ead721da`); `phase3-bench-validate` revalidated it without writing. |
| Phase 3 environment receipt | `results/phase-3/environment-smoke-0ae5c77622d860912c57b14dab04c3aa3e7c6219-slice3-review-20260901.edn` (SHA-256 `71c6c03354f210b4391a97d2a63f61229d98cd10d7904eebbbe29025ad0ac899`) records direct-linking, exact commands, commit/dirty diff, JMH jar/result hashes, and 14 benchmark source files with source-tree SHA-256 `8aad20b9ee421167287f6f15d44a6011a33570bfae2f8850a669aa574222c5c3`. Its smoke jar hash is `035e5197a706e8fad69c061b4e8c9abff12440cb91798cc442c0e418a4437e9e`. |
| Phase 2 preservation smoke | `clojure -Srepro -T:build bench-smoke '{:run-id "slice3-review-20260901"}'` completed the unchanged Phase 2 groups after the shared orchestration refactor; revalidation passed 19 rows / 10 benchmark identities with both required candidate IDs. The non-overwriting result is `results/phase-2/bench/smoke-0ae5c77622d860912c57b14dab04c3aa3e7c6219-slice3-review-20260901.json` (SHA-256 `8be7f506cbf26a11df3c61db298eabdbe861e2267b18861a5dc4f34d70f4f1cb`); its environment receipt is `results/phase-2/environment-0ae5c77622d860912c57b14dab04c3aa3e7c6219-slice3-review-20260901.edn` (SHA-256 `6965fa2e867df3d49ef8f0255dff06abca4f436f4e2b9f007665e1fab10cca06`). |
| Scope/performance decision | Changed files are benchmark harness/support, registry/runner/build/docs/manifests/README, and durable smoke receipts; no `src`/`src-java` production file changed. No screen, decision, GC, JIT, speed, allocation, or adoption claim is made; those belong to Slice 4. |

### Slice 3 run log

| Date | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-09-01 | Implementation Slice 3 | `/root/phase3_slice1` | Added direct-linked core/candidate unary wrappers, setup-selected monomorphic construction plans, generic `xf-seq`/`sequence`/`eduction`/`transduce` controls, fresh iterator source setup, and separate retained/unretained fresh-head reduction methods. Added the complete fresh-fixture trial, phase-aware registry/runner/build support, explicit screen/decision manifests, isolated jar/result paths, source hashes, and documentation. Factored shared Phase 2/3 manifest command/output/profile orchestration while preserving Phase 2 entry points and argv. | No production semantic changes; the harness builds and direct-linkage checks pass. |
| 2026-09-01 | Slice 3 validation | `/root/phase3_slice1` | Ran focused registry/manifests tests, 72-case trial, Phase 3 AOT/linkage and jar/listing checks, full local `check`, fresh Phase 3 smoke plus receipt revalidation, fresh Phase 2 preservation smoke plus revalidation, and `git diff --check`. | All checks passed; Phase 3 smoke has 36 validated rows, Phase 2 preservation has 19 rows / 10 identities, and no performance claim is made. Awaiting parent inspection and checkpoint; no commit made. |

### Slice 3 gate and focused-capability follow-up (pre-correction receipt)

The final gate follow-up keeps the compact primary manifests unchanged while
making the broader plan vocabulary executable for Slice 4. A private
`phase3-trial!` runs the already-built jar. Every Phase 3 timing profile calls
it immediately after `phase3-bench-jar`; the public trial builds once and then
calls the helper, and smoke builds once after `check` before calling it. The
focused benchmark class is a separate, setup-selected lane: its six explicit
parameters expose workloads, take counts, source families, and applicable
repaired Java reduced-aware/no-reduced candidates without adding those
dimensions to the primary four-key identity.

| Check | Evidence |
|---|---|
| Focused registry and manifest validation | `clojure -Srepro -M:bench -e "(require 'clojure.test 'xfseq.bench.registry-test) (clojure.test/run-tests 'xfseq.bench.registry-test)"` passed 9 tests / 60 assertions. Primary Phase 3 screen/decision remain 24 cells / 192 identities each. The focused screen has 10 cells / 237 identities and the focused decision has 7 cells / 241 identities, with exact `{implementation, operation, sourceKind, size, workload, takeCount}` keys. Registry negatives cover omitted implementation/iterator/sink/operation for smoke, focused missing/extra dimensions, invalid workload pairs, source-shape candidate mismatch, non-reducing `take`, and non-terminating full sinks over `repeat`/`iterate`. |
| Expanded trial | `java -cp target/bench/xfseq-phase3-jmh.jar xfseq.bench.Phase3BenchmarkSupport trial` passed the primary 72 fresh implementation/source cases and 291 focused workload/source cases. Focused coverage includes map identity/arithmetic/heavy, filter/remove 0/1/50/99/100% vocabulary, take 0/1/8/31/32/33/source-length/small-prefix/large-prefix, map-entry collections, terminating repeat/iterate, and all seven applicable repaired Java candidate IDs. |
| Focused harness smoke | Tiny one-fork JMH checks executed `Phase3FocusedBenchmark.first` on vector arithmetic/map-entry sources, `Phase3FocusedBenchmark.traverse` on terminating `iterate`, and a repaired no-reduced filter candidate. The rows completed without failures; these are harness correctness checks only and carry no performance claim. `java -jar target/bench/xfseq-phase3-jmh.jar -l` lists all seven methods for both `Phase3UnaryBenchmark` and `Phase3FocusedBenchmark`. |
| Final full check | `clojure -Srepro -T:build check '{}'` passed lint 0/0, reflection clean, 53 tests / 4,605 assertions / 0 failures / 0 errors. |
| Final AOT/linkage | `clojure -Srepro -T:build phase3-bench-linkage '{}'` passed after the final source changes. `target/bench/phase3-linkage-0ae5c77622d860912c57b14dab04c3aa3e7c6219.txt` SHA-256 is `b68a41c87092e385f7314e8a18e36af3ae3f0e195c5a058002ed97996bd06dcf`; it disassembles the focused direct wrappers and every timed sink helper, rejects Var reads, and contains all required core/candidate static invokes. |
| Final Phase 3 smoke | `clojure -Srepro -T:build phase3-bench-smoke '{:run-id "slice3-gate-20260901"}'` passed check, isolated build/linkage, both trial lanes, and one-fork/two-iteration smoke JMH. It validated 36 rows across all four primary operations and fresh iterator retained/unretained reductions. Result `results/phase-3/bench/smoke-0ae5c77622d860912c57b14dab04c3aa3e7c6219-slice3-gate-20260901.json` SHA-256 `8cb23fe44ec041839759356e0c87bb48f63631c6bc4c6d7d33034b7f355524e2`; `phase3-bench-validate` revalidated it without writing. |
| Final environment receipt | `results/phase-3/environment-smoke-0ae5c77622d860912c57b14dab04c3aa3e7c6219-slice3-gate-20260901.edn` SHA-256 `95e32534a1ec41806f6dadc13c7053d34ea023e21b6bf0828c3f6f21a7587c39`; source-evidence SHA-256 `a21c5b1fa8672bd9a1afa847ecd2ebde6256e7dcc099ebcf459c2a57ba96bc02`; jar SHA-256 `dffb7f8d0d5a23e56495fbad4c5906709134c9e35b61203934f17da631641c84`. |
| Phase 2 preservation | The previously recorded non-overwriting Phase 2 preservation smoke remains intact at `results/phase-2/bench/smoke-0ae5c77622d860912c57b14dab04c3aa3e7c6219-slice3-review-20260901.json` (19 rows / 10 identities) with its matching environment receipt; no Phase 2 path was overwritten by this follow-up. |
| Scope decision | The expanded dimensions are benchmark-only and isolated behind focused manifests. No production semantic file changed, no timing/GC/decision result was interpreted, and no performance/adoption claim was made. The gap did not require replanning. |

### Slice 3 focused-capability run log

| Date | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-09-01 | Final-gate helper | `/root/phase3_slice1` | Factored private `phase3-trial!`; invoked it after jar construction in every timing profile; changed public trial and smoke to build the jar once before invoking the helper; strengthened smoke operation exactness and added its negative registry case. | Primary smoke contract is now explicit and non-duplicating; registry suite passed 9 / 60. |
| 2026-09-01 | Focused capability | `/root/phase3_slice1` | Added setup-selected `Phase3FocusedBenchmark`/support, expanded source and workload constructors, focused screen/decision manifests, candidate source-shape/no-reduced applicability checks, and focused registry tests. | Main manifests remain compact (24 / 192 each); focused screen/decision expose 237 / 241 explicit identities for Slice 4 follow-up without harness edits. |
| 2026-09-01 | Focused correctness | `/root/phase3_slice1` | Extended jar trial with fresh direct-vs-primary and applicable repaired Java comparisons, then exercised focused map-entry, arithmetic, no-reduced filter, and terminating iterate JMH rows. | Primary trial 72 and focused trial 291 passed; all tiny focused rows completed without failures. |
| 2026-09-01 | Final Slice 3 verification | `/root/phase3_slice1` | Reran registry tests, full check, direct jar trial, AOT/linkage, JMH listing, and uniquely named primary smoke; recorded immutable result/environment hashes. | All requested gates passed; no blocker, no production change, no benchmark claim, and no commit made. |

### Slice 3 transduce/selectivity correction

Parent review identified that the original Phase 3 `transduce` control built a
vector before every sink and that `remove` selectivity labels described the
predicate rather than output. This correction keeps transduce only on
sink-specific direct terminal wrappers (first, prefix checksum, checksum,
vector, and unretained reduction). It rejects and removes `construct` and
`reduceRetained` transduce identities because a terminal transduction has no
lazy head to retain. `filter` and `remove` labels now both mean output
percentage; a focused size-100 trial assertion checks exact counts at 0, 1,
50, 99, and 100 for each operation.

| Check | Evidence |
|---|---|
| Registry and manifests | `clojure -Srepro -M:bench -e "(require 'clojure.test 'xfseq.bench.registry-test) (let [r (clojure.test/run-tests 'xfseq.bench.registry-test)] (when (pos? (+ (:fail r) (:error r))) (System/exit 1)))"` passed 9 tests / 64 assertions. Primary screen/decision remain 24 cells but now have 184 identities each (eight inapplicable transduce retained-head rows removed); focused manifests remain 237 / 241 identities. Registry negatives cover transduce construct and retained-head cells in both classes, plus smoke transduce retained rows. |
| Full local check | `clojure -Srepro -T:build check '{}'` passed lint 0/0, reflection clean, 53 tests / 4,605 assertions / 0 failures / 0 errors. |
| AOT/linkage | `clojure -Srepro -T:build phase3-bench-linkage '{}'` passed; the final linkage report disassembles `_firstValue`, `_prefixChecksum`, `_checksum`, `_vectorValue`, `_reduceChecksum`, all four direct transduce sink wrappers, and direct core/candidate construction callers, with no Var reads. Report SHA-256: `df4717b962a3ebfde1e90604af1345f38a13296f7b291da50fae3987670c3f0f`. |
| Sink-specific trial | `clojure -Srepro -T:build phase3-bench-trial '{}'` passed 72 fresh implementation/source cases and 420 applicable sink checks; focused trial passed 339 fresh workload/source cases, including exact output-selectivity assertions. The transduce trial has no retained-head check. |
| Fresh Phase 3 smoke | `clojure -Srepro -T:build phase3-bench-smoke '{:run-id "slice3-transduce-final-20260901"}'` passed semantic/build/linkage/trial gates and one-fork/two-iteration JMH. It validated 35 rows: all four first operations, six unretained iterator reductions, and five retained iterator reductions (transduce excluded). Result `results/phase-3/bench/smoke-0ae5c77622d860912c57b14dab04c3aa3e7c6219-slice3-transduce-final-20260901.json` SHA-256 `55d2867c04278c749db7c8e963ac1cf299b25e76bf92fb8ffd41c4a326c157ab`; environment SHA-256 `3573f1de4094c2250e69f8223e2a816cbca68b0b945d80b2450b9041ec8ef9d9`; jar SHA-256 `00de322bb8f8471dbb23e34717a45cf2e7851f65235c7634424335b0bc76f8a8`; source-evidence SHA-256 `ad0d6c2d5585c0265f798647af11cafc51b0aef8c07ddbfd3de6eb06ef398540`. |
| Scope decision | No production semantic changes and no Phase 2 benchmark changes. The existing non-overwriting Phase 2 preservation receipts remain intact. No performance/adoption claim is made. |

### Slice 3 transduce/selectivity correction run log

| Date | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-09-01 | Sink correction | `/root/phase3_slice1` | Replaced vector-producing Phase 3 transduce construction with setup-selected direct terminal sink plans and AOT wrappers; kept candidate-owned xform construction in the candidate direct plan. | First/prefix/checksum/vector/unretained transduce controls are direct and independently exact; no transduce retained-head scalar remains. |
| 2026-09-01 | Applicability correction | `/root/phase3_slice1` | Rejected transduce construct and retained-head identities in primary/focused registry validation; removed transduce from primary retained manifests and smoke. | Primary manifests validate at 184 identities each; smoke validates 35 rows and preserves all six implementations through applicable first/unretained controls. |
| 2026-09-01 | Selectivity correction | `/root/phase3_slice1` | Defined filter/remove focused labels as output percentages, complemented remove predicates, and added direct size-100 count assertions for 0/1/50/99/100. | Focused trial passed 339 cases with exact count assertions and all implementation/sink comparisons. |
| 2026-09-01 | Final correction verification | `/root/phase3_slice1` | Reran registry tests, full check, linkage, trial, and a fresh uniquely named Phase 3 smoke; checked diff hygiene. | All gates passed; no blocker, no commit made, and no performance claim. |
| 2026-09-01 | Final smoke receipt refresh | `/root/phase3_slice1` | Reran focused registry tests, the direct sink-specific trial, AOT/linkage, full local check, and a fresh non-overwriting smoke after excluding transduce retained-head identities. | Registry 9 / 64, trial 72 / 420 plus focused 339, full check 53 / 4,605, linkage passed; final smoke receipt has 35 rows and hashes recorded above. |
| 2026-09-01 | Slice 3 parent checkpoint | `/root` | Inspected the stable harness, focused lane, sink-specific transduce controls, retained-head applicability, output-selectivity assertions, manifests, registry, build orchestration, documentation, and raw receipts. Independently reran full `check`, the registry suite, isolated jar/linkage/trials, JMH listing, diff hygiene, and a new parent-owned smoke. | Accepted: full check 53 / 4,605; registry 9 / 64; trial 72 / 420 plus focused 339; both Phase 3 classes list all seven methods; parent smoke validated 35 rows. Parent receipt `results/phase-3/bench/smoke-0ae5c77622d860912c57b14dab04c3aa3e7c6219-slice3-parent-final-20260901.json` SHA-256 `be79719b743910c725d37518205e644cc28209ec5cb7ef140e3af24b86451ef0`; environment SHA-256 `4d3b0475381e7f83a62f38ae3ae2e6d60146408ad6dd718272563a320b4d5c7c`. No speed claim; ready for the Slice 3 checkpoint commit. |

## Slice 4 screen evidence and interruption checkpoint

Slice 4 began from the accepted harness checkpoint `afa1544` (harness
checkpoint `d020b2a`). The primary and focused screens completed with the
declared direct-linking/JMH environment. The focused decision lane was then
expanded only for the screen-backed map source-family regressions and the
applicable repaired Java filter/remove rows. Before a merged decision receipt
existed, the agent/parent canceled that first run after a mistaken inference
about unrelated processes; its partial groups are retained only as
non-decision diagnostics and must not be used for a decision. The user's
`18:21:54 CEST` FYI was not an interruption of xfseq.

| Check | Evidence |
|---|---|
| Primary screen | `clojure -Srepro -T:build phase3-bench-screen '{:run-id "slice4-20260901-primary-screen"}'` passed the semantic gate (53 tests / 4,605 assertions, lint 0/0, reflection clean), AOT/linkage, jar, primary/focused trial setup, and the 2-fork screen. Result `results/phase-3/bench/screen-afa15445a6a88d997a2db61bc9008a68f609ee25-slice4-20260901-primary-screen.json` has 184 rows / 96 unique parameter sets, SHA-256 `a1e09a98d31c19a8730f650123ec206761d74e4642e7a40594be220f8c00c5bd`. Environment `results/phase-3/environment-screen-afa15445a6a88d997a2db61bc9008a68f609ee25-slice4-20260901-primary-screen.edn` SHA-256 `e7e61a90c8af1e625acdcb63c6ef8f95f6bbc10653f5d613117e0ddf37ba0ec0`. |
| Focused screen | The separate focused screen completed all 10 cells / 237 applicable identities with 2 forks and no failures or interruptions. Result `results/phase-3/focused/bench/screen-afa15445a6a88d997a2db61bc9008a68f609ee25-slice4-20260901-focused-screen.json` SHA-256 `2aee0128190f7aabc59d997fa4f0ac1fc7468328fc56bf30d8a3ead2e74d6c13`; environment `results/phase-3/focused/environment-screen-afa15445a6a88d997a2db61bc9008a68f609ee25-slice4-20260901-focused-screen.edn` SHA-256 `02054e9cf31742c9ae8ed3222a8f139c677f0d888eb54b3fad6f62810398a08e`. The screen showed map source-family direct regressions from roughly −7.7% to −34.4%, and the existing focused decision cells already covered the map workloads, filter/remove selectivities, take counts, and terminating sources. |
| Focused decision follow-up | Added three manifest-only cells to `bench/manifests/phase3-focused-decision.edn`: map source families (54 identities), repaired Java list filter/remove (10), and repaired Java vector filter/remove (4). The manifest validates at 10 cells / 309 identities; manifest SHA-256 is `bdab4f90ea8c6382d8dc1d86be6f50fb3384d958619c0e62827c1b66f5d66779`. |
| Interruption/cancellation correction | Command `clojure -Srepro -T:build phase3-bench-focused '{:profile "decision" :manifest-file "bench/manifests/phase3-focused-decision.edn" :run-id "slice4-20260901-focused-decision"}'` began with the semantic/AOT/linkage/trial gates passing. The user's parent-reported `2026-09-01 18:21:54 CEST` FYI concerned unrelated processes in another project and did not interrupt xfseq. The agent/parent later canceled this xfseq process after that mistaken inference; the exact Ctrl-C/command-end time was not captured, but it preceded the preserved-copy timestamps at 18:22:56/18:22:59 CEST. Complete temporary groups were `focused-decision-map-source-families` (54 rows) and `focused-decision-map-workloads` (36 rows). `focused-decision-filter-selectivities` had started but its temporary JSON was 0 bytes; all later groups had not begun. No durable merged decision result or environment receipt was produced. `ps` was permission-denied, so no PID is claimed and no user process kill is attributed. |
| Preserved partial diagnostics | Complete temporary groups were copied with benchmark data unchanged, then normalized only to remove the generated extra blank line at EOF: `results/phase-3/focused/partial/decision-slice4-20260901-interrupted/000-focused-decision-map-source-families.json` (54 rows, SHA-256 `f3e034213f8d48f323c064b302b4364e3292873fd9b4de9cb3b420264f8a53ef`) and `001-focused-decision-map-workloads.json` (36 rows, SHA-256 `e921c5e284e741b547cddcf749ea67bc27d43f3bcad4dd9c8a505e5a9c77fd7f`). These are explicitly non-decision artifacts; no partial group is merged into a decision receipt. |
| Slice 4 state | No primary/focused decision, decision-GC, JIT/inlining, JFR, or allocation lane completed. Consequently no cell-local decision, causal claim, per-function adopt/investigate/reject outcome, Slice 5 no-reduced trigger, Slice 6 result-type trigger, or performance claim is made. The earlier focused run was agent/parent-canceled after the mistaken unrelated-process inference; Slice 5 and Slice 6 remain unstarted. |

### Slice 4 run log (interrupted)

| Date/time | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-09-01 | Screen preflight | `/root/phase3_slice1` | Validated the primary and focused manifests, ran the primary screen, and retained its exact environment/result receipts. | Primary screen completed: 184 rows / 96 unique parameter sets, no failures. |
| 2026-09-01 | Focused screen | `/root/phase3_slice1` | Ran the separate 10-cell focused screen covering map workloads/source families, filter/remove selectivities, take counts/terminating sources, and applicable repaired Java rows. | Focused screen completed: 237 applicable identities, no failures; result/environment hashes recorded above. |
| 2026-09-01 | Manifest follow-up | `/root/phase3_slice1` | Added only the map source-family and repaired Java filter/remove decision cells justified by the focused screen; validated the manifest. | Focused decision manifest: 10 cells / 309 identities; no production or harness edit. |
| 2026-09-01 ~18:22 CEST | Focused decision cancellation | `/root/phase3_slice1` | Continued the required 3-fork focused decision lane with 5 warmups/measurements and G1 settings until agent/parent cancellation after the mistaken unrelated-process inference. | Canceled while filter-selectivities was running; exact stop time was not captured. Two complete temporary groups were preserved as non-decision diagnostics; no merged decision receipt exists. The `18:21:54 CEST` user FYI was unrelated to xfseq. |
| 2026-09-01 18:35:50 CEST | Resume authorization | `/root/phase3_slice1` | Parent relayed the user's explicit direction to continue Slice 4. Kept the interrupted groups diagnostic-only and authorized a fresh complete focused decision rerun. | Resume authorized; the new run ID is `slice4-20260901-focused-decision-resume1`. |

### Slice 4 approved evidence-scope correction and targeted lanes

The complete resume run below is the durable full focused decision receipt; the
earlier temporary groups remain diagnostic-only. At `2026-09-01 21:20:39 CEST`,
the user-approved narrowing was recorded: the supported screen regressions,
plus representative fresh three-fork confirmation, mathematically rule out
adoption under the existing no-regression gate for the affected public
functions. Therefore exhaustive primary/focused decision and GC cells that
cannot change a per-function outcome are intentionally skipped. This is an
evidence-scope correction, not a performance claim or a claim that skipped
cells are equivalent.

Before any new timing, the following compact manifests were predeclared. The
targeted decision manifest has 72 identities: every operation appears in a
chunked partial rejection row (`vector`/`prefix8`), a dechunked full
best/closest row (`list`/`traverse`), a chunked full causal-control row with
`xfseq-generic`, `sequence`, `eduction`, and sink-specific `transduce`, and
explicit list/vector `reduceUnretained` and `reduceRetained` pairs. The
separate targeted GC manifest has 74 focused identities: the exact
diagnostic reduction pairs for map/filter/remove/take, direct core/candidate
and sink-specific transduce where applicable, generic/sequence/eduction
controls for the representative map rows, plus applicable
dechunked/chunked reduced-aware/no-reduced Java pairs. It deliberately does not pretend to be
the full 184-identity primary GC matrix or the full 309-identity focused GC
matrix.

| Check | Evidence |
|---|---|
| Full focused decision resume | `clojure -Srepro -T:build phase3-bench-focused '{:profile "decision" :manifest-file "bench/manifests/phase3-focused-decision.edn" :run-id "slice4-20260901-focused-decision-resume1"}'` completed all 10 cells / 309 identities with 3 forks, 5 one-second warmups/measurements, and fixed G1 heap; no interruption or merge failure. Result `results/phase-3/focused/bench/decision-afa15445a6a88d997a2db61bc9008a68f609ee25-slice4-20260901-focused-decision-resume1.json` SHA-256 `2c9cbfa5cbdde800cf6259baf30c505ef9af4aaa3e9fafa15169d6c4434f8d43`; environment `results/phase-3/focused/environment-decision-afa15445a6a88d997a2db61bc9008a68f609ee25-slice4-20260901-focused-decision-resume1.edn` SHA-256 `568e11bdc57e4963f72e0222bda0fb500f18f248ee657c3b201463a55026a755`. |
| Scope rationale | The primary screen result (`a1e09a98d31c19a8730f650123ec206761d74e4642e7a40594be220f8c00c5bd`) and focused screen result (`2aee0128190f7aabc59d997fa4f0ac1fc7468328fc56bf30d8a3ead2e74d6c13`) contain repeatable supported direct regressions for map/filter/remove/take; the fresh 3-fork resume receipt confirms the focused operation/workload/source reversals. Since adoption requires no repeatable supported regression over 3%, a targeted confirmation cannot convert these functions to `adopt`; skipped cells are recorded as confidence limitations and not silently treated as measured. |
| Predeclared targeted manifests | `bench/manifests/phase3-slice4-targeted-decision.edn` (72 identities) and `bench/manifests/phase3-slice4-targeted-gc.edn` (74 identities; focused class, profile `:decision` for the existing `:decision-gc` validator) were written and registry-validated before timing. The GC manifest includes map generic/sequence/eduction controls and take list/vector retained/unretained pairs requested in pre-run review. Decision manifest SHA-256 `733f4355ed6e1b9bac8c1ca95bf7900277e6a1029e6760e2b043a1ace5634cb3`; updated GC manifest SHA-256 `b825bc44f2d828f3f51ce553d49a071f8779273c8a00d98b324dbeddc7fc09b4`. |
| Targeted three-fork decision | `clojure -Srepro -T:build phase3-bench-decision '{:manifest-file "bench/manifests/phase3-slice4-targeted-decision.edn" :run-id "slice4-20260901-targeted-decision1"}'` completed 5 cells / 72 rows with 3 forks, 5 one-second warmups/measurements, fixed 2-GiB G1 heap, direct linking, JMH 1.37, Clojure 1.12.5, and no failures. Result `results/phase-3/bench/decision-afa15445a6a88d997a2db61bc9008a68f609ee25-slice4-20260901-targeted-decision1.json` SHA-256 `519b0c23ecadbe201658a6c8a33b5ad682f85dd675bc99afba2378f3e0370208`; environment `results/phase-3/environment-decision-afa15445a6a88d997a2db61bc9008a68f609ee25-slice4-20260901-targeted-decision1.edn` SHA-256 `e7357f002abb44832b3f0b46ab66958485d7546f3a116ea9b12d1f1ae116bb16`; source-evidence SHA-256 `ed99da52bc73f67a67a3da61c386af04f902d033b2f327fc669f1797ee850c9d`, JMH jar SHA-256 `e6c0d01205a018dedc57037d604d1b5659243de2b1240add1bdbb47c982c783d`. |
| Targeted decision observations | Direct candidate/core throughput ratios were `map −30.6%` (chunked prefix), `filter −37.8%`, `remove −41.0%`, `take −17.9%`; dechunked full rows were map −8.6%, filter +25.5%, remove +27.2%, take −18.2%. Across the explicit retained/unretained pairs, map and take remained slower on both source shapes; filter/remove wins were confined to list reductions while vector reductions remained slower. The chunked full control row was retained for causal comparison only: generic `xf-seq`, `sequence`, `eduction`, and direct `transduce` are not equivalent public result contracts and do not change adoption eligibility. No cause, no-reduced trigger, result-type trigger, or adoption recommendation is assigned before GC/JIT/JFR follow-up. |

### Slice 4 targeted run log

| Date/time | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-09-01 21:20:39 CEST | Scope correction/predeclaration | `/root/phase3_slice1` | Recorded the user-approved mathematical narrowing after the complete focused resume receipt; wrote the targeted decision/GC manifests before launching new timing. | Full matrices that cannot change the no-regression outcome are skipped; targeted 72/74 identity lanes are the declared follow-up, with skipped-cell confidence limits explicit. |
| 2026-09-01 | GC pre-run review correction | `/root/phase3_slice1` | Added focused map list/vector generic/sequence/eduction controls to both reduction methods and focused take list/vector direct retained/unretained pairs; revalidated before GC timing. | Targeted GC manifest is now 12 cells / 74 identities; SHA-256 `b825bc44f2d828f3f51ce553d49a071f8779273c8a00d98b324dbeddc7fc09b4`. |
| 2026-09-01 | Targeted decision | `/root/phase3_slice1` | Ran the predeclared targeted primary decision through the normal semantic, AOT/linkage, trial, manifest, JMH, merge, validation, and environment-receipt path. | 5 groups / 72 rows completed with 3 forks and no failures; durable result/environment hashes recorded above. |

### Slice 4 targeted diagnosis, decisions, and final gates

Slice 4 was accepted by the parent and checkpointed at `0652f05`. It records
the complete focused decision, the user-approved targeted decision and GC
lanes, representative JIT/JFR diagnostics, and the resulting per-function
rejections without changing production or benchmark implementation code.

The targeted GC lane was run only after the 12-cell/74-identity manifest was
validated. The four compiler/inlining and four JFR probes were representative
diagnostics, not a replacement for the three-fork decision receipts. They use
the same direct-linked isolated Phase 3 jar and fixed 2-GiB G1 settings. JFR
event counts are process-wide/sample-based observations; they are retained as
raw evidence and are not treated as a causal allocation proof.

| Check | Evidence |
|---|---|
| Targeted GC decision | `clojure -Srepro -T:build phase3-bench-decision-gc '{:manifest-file "bench/manifests/phase3-slice4-targeted-gc.edn" :run-id "slice4-20260901-targeted-gc1"}'` completed all 12 groups / 74 rows with 3 forks, 5 one-second warmups/measurements, `-Xms2g -Xmx2g -XX:+UseG1GC`, and `-prof gc`; the merged receipt has no failures. Result `results/phase-3/focused/bench/decision-gc-afa15445a6a88d997a2db61bc9008a68f609ee25-slice4-20260901-targeted-gc1.json` SHA-256 `2fe8baedd336619d704e1fe771889d6c0654e5405bbcb7c12c416a78d0c7e84f`; environment `results/phase-3/focused/environment-decision-gc-afa15445a6a88d997a2db61bc9008a68f609ee25-slice4-20260901-targeted-gc1.edn` SHA-256 `a61590885d4287ca75b707f3ef6f7591070086b89339681e46a686234959940f`. `validate-manifest!` revalidated 74 rows, the 12-cell manifest, and manifest SHA-256 `b825bc44f2d828f3f51ce553d49a071f8779273c8a00d98b324dbeddc7fc09b4`. |
| GC observations | Candidate/core `gc.alloc.rate.norm` ratios were map list `-11.1%` unretained / `-6.9%` retained and map vector `+23.3%` / `-0.6%`; take list `-0.4%` / `-0.3%` and vector `-3.0%` / `-1.9%`; filter/remove list about `-21.6%` for both operations, while vector behavior differed by operation and retention mode. These mixed allocation results do not erase the direct throughput regressions or establish a product no-reduced/result-type cause. |
| JIT/inlining probes | Four one-fork direct-on probes used 3 one-second warmups/measurements, fixed G1/2-GiB settings, `-XX:+UnlockDiagnosticVMOptions -XX:+PrintCompilation -XX:+PrintInlining`, and `-prof comp`: map vector `reduceUnretained` (rerun after an initial malformed-regex diagnostic), map list `reduceRetained`, filter list `reduceRetained`, and take vector `prefix8`. Each valid JSON has two core/candidate rows. JSON hashes after trailing-blank-line normalization: map-vector `bd06737db9ac8848f198c2287f498514709aa4568a3897e6ce348afe15d8ecd8`, map-list `61cd77d52c28b56053ad311b1d5dbac2177dc077f78a1ad95e2215932e297eea`, filter-list `b6f77d45e98d42447fde97553e7a9929c585cd0c4285d2001919abf6d8a1719d`, and take-vector `0571515255961025a207116a86bc12605f0ad4d7fe8b00f69f178d8cb814ce67`; corresponding log hashes are map-vector `00f2ae19dec30d56ff2c863c416db4a47c58085d04c761f0688229f1088f9a01`, map-list `e7a4a5585b19413139ee1e9f2bc4b2bb3bf7f695a250eaf28ac9c04cbe99abc8`, filter-list `067de7e1b08fe4d017243bd0addd2112a89a64535b460254589348e1359412de`, and take-vector `a376f89954ea3bf56699be5e8577eabee2207818fffc9ff1defcc981c3cc2203`. The initial malformed-regex map-vector diagnostic is retained but excluded: empty JSON SHA-256 `e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`, log SHA-256 `7c96971e0e1cb94d69a3e1257f5a6087b4cc74f853545af7ebb66a782f026fe2`. The valid probes show candidate/core throughput ratios of map vector `-64.0%`, map list `-2.8%`, filter list `+29.2%`, and take vector `-7.3%`; compiler output alone does not identify a unique cause. |
| JFR probes | Four one-fork direct-on probes used 3 one-second warmups/measurements, fixed G1/2-GiB settings, and `-prof jfr`, covering the same representative map/filter/take cells. JSON hashes after trailing-blank-line normalization: filter list `53d9207bcf2c8fd89d366c1a0babed1f29a61ff5bc913e85ba3fc4f631573f55`, map list `2cb70626a3fc37d5862928d07678aabd46e7d60e7bd2085af6c87ce96b790164`, map vector `25cfdb52e803dd9f37b1ffc93dfda3e1ac87e58ce32687a71c9fa8e177e06056`, and take vector `567377b075147883f47414aee294a746f0e2a3bf554cd2a61e56e186d9d3a9f0`; corresponding log hashes are filter list `055ddb73bd0fb5bc21965fdbbc3474496aa935601bb558060b84e077dfb2592e`, map list `27f62f67ba3ff92ac86a7d680abc10c225178f04b75e5504414fb277b96e9074`, map vector `6fbbe1adacd7bf283dc6b1093c7184520abf8800e7141f854f6fdb8b5e891b41`, and take vector `2cdf46bc2a0fceb76a549c34a48cb20f3519f2143a15bef3b4dbdf664541da19`. The eight raw `profile.jfr` files are under `results/phase-3/profiles/jfr-slice4-20260901-{filter-list-reduce-retained,map-list-reduce-retained,map-vector-reduce-unretained,take-vector-prefix8}/`; candidate/core SHA-256 pairs in that order are filter `157b7fd29a2ad9336361c304193cfbe7fb23c435401022e16ccfd696d0ce3e6c` / `180503e430ffe5f1cf248a17eecb1cea0c32e5e60e92073503b74aac2da85c0f`, map-list `ccbb8422042b34fec455df74f9aff180f337cb2841a05c8855213e53a1e4d2c2` / `c85f6a21c53afc4a32150c2afd9c957433dfa882b64f37597b37d129315f39fc`, map-vector `7c09e995accaa665f67273ed995e71716a0d1ae764265e92a6f192bebf0a067d` / `cc4cdb81d3b21de03daa763729194a5f6df189fe018daa35d13e9a4ca6bdf93d`, and take `92d6042b297cc2238da65b7ca48a6633373a6995e18bed0c122d3089d3f1d698` / `3454bd1ed935dc112c4f893f11f756334f8878eeccab800b9835759312c40d9d`. `jfr summary` reports `jdk.ObjectAllocationSample`, `jdk.GCPhaseParallel`, and related GC events in every file. Candidate/core throughput ratios were filter list `+23.9%`, map list `-4.9%`, map vector `-42.5%`, and take vector `-13.3%`; these are diagnostic probes, not pooled claims. |
| Final semantic/registry/linkage gates | `clojure -Srepro -M:bench -e "(require 'clojure.test 'xfseq.bench.registry-test) (let [r (clojure.test/run-tests 'xfseq.bench.registry-test)] (when (pos? (+ (:fail r) (:error r))) (System/exit 1)))"` passed 9 tests / 64 assertions. The one-line registry fixture refresh from 241 to the accepted 309-identity focused decision manifest was required for this gate; no production or benchmark implementation changed. `clojure -Srepro -T:build check '{}'` passed lint 0/0, reflection clean, 53 tests / 4,605 assertions / 0 failures / 0 errors. `clojure -Srepro -T:build phase3-bench-linkage '{}'` passed; report `target/bench/phase3-linkage-afa15445a6a88d997a2db61bc9008a68f609ee25.txt` SHA-256 `df4717b962a3ebfde1e90604af1345f38a13296f7b291da50fae3987670c3f0f` disassembles direct construction and every timed sink helper (`_firstValue`, `_prefixChecksum`, `_checksum`, `_vectorValue`, `_reduceChecksum`, and four transduce sinks), contains no Var lookup, and contains the required core/candidate static invokes. |
| Phase 2 preservation gate | The first `bench-smoke` attempt reached all semantic/AOT gates but hit a stale JMH lock before producing a receipt. The same non-overwriting command was rerun with `JAVA_TOOL_OPTIONS=-Djmh.ignoreLock=true`: `clojure -Srepro -T:build bench-smoke '{:run-id "slice4-20260901-phase2-preservation2"}'`, then `clojure -Srepro -T:build bench-validate '{:run-id "slice4-20260901-phase2-preservation2"}'` passed 19 rows / 10 benchmark identities with both required candidate IDs. Result `results/phase-2/bench/smoke-afa15445a6a88d997a2db61bc9008a68f609ee25-slice4-20260901-phase2-preservation2.json` SHA-256 `856c1d642619cd3c951151a93e02e88396ea989e235e526b3434041fcd93d4f8`; environment `results/phase-2/environment-afa15445a6a88d997a2db61bc9008a68f609ee25-slice4-20260901-phase2-preservation2.edn` SHA-256 `4c17a50236a2c3c49b8eab5208a639122e1adf98649cb9c047744a51d85b6a6e`. Existing Phase 2 receipts were not overwritten. |
| Decision basis | In the fresh full focused decision, direct candidate/core pairs over matched cells had regressions beyond 3% in map `14/15`, filter `6/10`, remove `6/10`, and take `10/13`; the targeted three-fork lane independently confirmed map `7/7` regressions, take `7/7` regressions, and mixed filter/remove behavior with vector regressions. The targeted controls are not equivalent public result contracts and are causal context only. |
| Per-function recommendation | `map: reject` the current public compatible replacement because regressions are broad and include both source shapes and both reduction reachability modes. `take: reject` because every targeted direct pair regressed despite small allocation differences. `filter: reject` and `remove: reject` for adoption because list wins are offset by repeatable chunked/vector regressions over 3%; the wins remain diagnostic, not a pooled claim. The existing direct core behavior remains the required fallback. |
| Conditional triggers | Slice 5 no-reduced promotion: **not triggered**—no public map/filter/remove operation has a repeatable >=5% benefit in both chunked and dechunked important cells with no supported regression over 3%; repaired Java no-reduced rows are diagnostic only. Slice 6 result-type experiment: **not triggered**—although full-reduction regressions exist, GC/JFR/control evidence is mixed and process-wide/sample-based, so it does not identify ordinary cursor/realization allocation as a material cause that a custom result can plausibly remove. No production result prototype or no-reduced promotion was started. |
| Scope and confidence | The user-approved narrowing intentionally skips exhaustive primary/focused decision and GC cells that cannot alter these no-regression outcomes. The targeted receipts cover representative supported regressions, closest/best rows, chunked/dechunked controls, partial/full sinks, retained/unretained pairs, Java reduced-aware/no-reduced diagnostics, and take allocation pairs; skipped cells are not treated as measured. No cause is named beyond the observed mixed source-shape/allocation relationship, no pooled speedup/allocation claim is made, and Slice 5/6 remain unstarted. |

### Slice 4 final run log

| Date/time | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-09-01 22:52 CEST | Targeted GC completion | `/root/phase3_slice1` | Ran the predeclared 12-cell targeted GC manifest after adding map controls and take retained/unretained pairs; validated the merged receipt and environment. | 12/12 groups, 74 rows, 3 forks, no failures; result SHA `2fe8baedd336619d704e1fe771889d6c0654e5405bbcb7c12c416a78d0c7e84f`, environment SHA `a61590885d4287ca75b707f3ef6f7591070086b89339681e46a686234959940f`. |
| 2026-09-01 22:52 CEST | JIT/JFR diagnosis | `/root/phase3_slice1` | Ran four representative compiler/inlining probes and four representative JFR probes against the isolated Phase 3 jar; preserved raw logs, JSON, and eight binary JFR files. | All valid probes completed with two rows each and no active profiling process remains. One malformed-regex JIT probe is retained as excluded diagnostics; no helper-specific causal claim is made. |
| 2026-09-01 22:52 CEST | Final gates | `/root/phase3_slice1` | Refreshed the stale focused registry identity assertion, reran registry tests, full semantic check, linkage, and a unique Phase 2 preservation smoke with JMH lock override; validated the Phase 2 receipt. | Registry 9/64, full check 53/4,605, lint 0/0, reflection clean, linkage no Var lookups, Phase 2 19 rows/10 identities; all passed. |
| 2026-09-01 22:52 CEST | Slice 4 checkpoint | `/root/phase3_slice1` | Recorded per-function adoption decisions, no-reduced/result-type trigger outcomes, skipped-cell confidence limits, raw evidence hashes, and preserved baseline/receipt paths. | `map`, `filter`, `remove`, and `take` all reject the current public compatible replacement under the declared no-regression gate. Slice 5/6 were not started; no commit made. |
| 2026-09-01 | Slice 4 parent checkpoint | `/root` | Inspected the manifests, receipts, environment metadata, profile artifacts, registry fixture, decision arithmetic, trigger conclusions, and interruption history; corrected one ambiguous sentence so the first focused decision cancellation is attributed only to the agent/parent. Independently reran full `check`, registry tests, linkage, exact validation of the 309-row focused decision, 72-row targeted decision, and 74-row targeted GC receipt, hash/row checks, and diff hygiene. | Accepted: full check 53 / 4,605, lint 0/0, reflection clean; registry 9/64; linkage passed; all receipt hashes and row counts matched; recomputed direct regressions matched the recorded 14/15, 6/10, 6/10, 10/13 focused counts and 7/7 targeted map/take counts. Ready for the Slice 4 checkpoint commit; Slice 5/6 remain skipped because their triggers were not met. |
| 2026-09-01 | Slice 4 checkpoint SHA record | `/root` | Recorded the accepted Slice 4 checkpoint before starting any later slice. | Checkpoint `0652f05`; Slice 5 and Slice 6 remain skipped, so the next executable work is Slice 7 cleanup and final handoff only. |

## Slice 7 consolidation and final handoff

Slice 7 starts from accepted repository checkpoint `819e114` and the accepted
Slice 4 content checkpoint `0652f05`. Slice 5's operation-owned no-`Reduced`
promotion and Slice 6's custom-result experiment remain skipped because their
recorded triggers were not met. The final audit found no unpromoted
`IReduceInit`/custom-result prototype, no Phase 3 no-reduced production
machinery, and no dead benchmark/test support that can be removed without
breaking a named receipt or validation path. The retained focused harness,
repaired Java controls, manifests, and diagnostic profiles are all referenced
by the recorded evidence; the malformed-regex JIT output remains as an
explicitly excluded raw diagnostic. No new mechanism or production change was
made.

| Check | Evidence |
|---|---|
| Focused unary semantics | `clojure -Srepro -M:test -n xfseq.unary-oracle-test` passed 24 tests / 1,699 assertions / 0 failures / 0 errors. |
| Phase 2 semantic preservation | `clojure -Srepro -M:test -n xfseq.object-engine-test -n xfseq.object-candidate-test` passed 28 tests / 2,860 assertions / 0 failures / 0 errors. |
| Full local check | `clojure -Srepro -T:build check '{}'` passed lint 0/0, compiler reflection clean, 53 tests / 4,605 assertions / 0 failures / 0 errors. |
| Phase 2 preservation smoke | With the stale JMH lock bypassed explicitly via `JAVA_TOOL_OPTIONS=-Djmh.ignoreLock=true`, `clojure -Srepro -T:build bench-smoke '{:run-id "slice7-20260901-phase2-preservation1"}'` followed by `clojure -Srepro -T:build bench-validate '{:run-id "slice7-20260901-phase2-preservation1"}'` passed 19 rows / 10 benchmark identities and both required Java candidate IDs. Result `results/phase-2/bench/smoke-819e1144a48dfc7ac6e21e9709bbdcfc75bee156-slice7-20260901-phase2-preservation1.json` SHA-256 `3a7f20e0db994e904466cdde72ad53b4e3d63951b04791c8d3e531dec4ddac51`; environment `results/phase-2/environment-819e1144a48dfc7ac6e21e9709bbdcfc75bee156-slice7-20260901-phase2-preservation1.edn` SHA-256 `3d81266d5c265d48f69eb124e0e3c7791c76a808c3c90d17ab57fe0035892dba`. Existing Phase 2 receipts were not overwritten. |
| Phase 3 linkage | `clojure -Srepro -T:build phase3-bench-linkage '{}'` passed on checkpoint `819e1144a48dfc7ac6e21e9709bbdcfc75bee156`; report `target/bench/phase3-linkage-819e1144a48dfc7ac6e21e9709bbdcfc75bee156.txt` SHA-256 `df4717b962a3ebfde1e90604af1345f38a13296f7b291da50fae3987670c3f0f` contains no `Var` lookups and includes the required direct core/candidate static calls and timed sink wrappers. |
| Phase 3 manifest/receipt validation | `validate-manifest!` revalidated every durable Slice 4 result: primary screen 184 rows with manifest SHA-256 `505418c010699e3d93fc32486de311517584e51ae98d03c9852ee521c3fb4bb4` and result SHA-256 `a1e09a98d31c19a8730f650123ec206761d74e4642e7a40594be220f8c00c5bd`; focused screen 237 / `b4b55f7c271945dcd9ea81cc8bc081da4a690df4c22cc0806bd9205067683b36` / `2aee0128190f7aabc59d997fa4f0ac1fc7468328fc56bf30d8a3ead2e74d6c13`; focused decision 309 / `bdab4f90ea8c6382d8dc1d86be6f50fb3384d958619c0e62827c1b66f5d66779` / `2c9cbfa5cbdde800cf6259baf30c505ef9af4aaa3e9fafa15169d6c4434f8d43`; targeted decision 72 / `733f4355ed6e1b9bac8c1ca95bf7900277e6a1029e6760e2b043a1ace5634cb3` / `519b0c23ecadbe201658a6c8a33b5ad682f85dd675bc99afba2378f3e0370208`; targeted GC 74 / `b825bc44f2d828f3f51ce553d49a071f8779273c8a00d98b324dbeddc7fc09b4` / `2fe8baedd336619d704e1fe771889d6c0654e5405bbcb7c12c416a78d0c7e84f`. |
| Diff hygiene and scope | `git diff --check` passed. No Slice 5/6 code, Phase 4 work, production semantic change, benchmark implementation change, or performance claim was added. |

### Exit-criterion audit

| # | Criterion | Result |
|---:|---|---|
| 1 | Core transducer arities delegate directly and public paths no longer use analyzer-generated xforms. | Pass — Slice 1 oracle, source diff, and accepted direct-linkage evidence. |
| 2 | Unary collection arities route through one closed compatibility boundary and ordinary `LazySeq`. | Pass — Slice 2 profile implementation and accepted focused oracle. |
| 3 | Construction is lazy; successful xform application/completion occur exactly once. | Pass — direct oracle construction/trace coverage. |
| 4 | Direct differential values pass the Phase 3 source/size/operation/take matrix. | Pass — accepted semantic matrix and primary/focused fresh-fixture trials. |
| 5 | Source advance, mapper/predicate order, batching, output chunks, and downstream calls match core. | Pass — accepted trace/chunk/downstream oracle cases. |
| 6 | Invalid and non-positive `take`, source order, and exception order match core. | Pass — accepted four-force and take guard oracle cases. |
| 7 | Initial/later failures match class, point, prefix, trace, and same-node behavior. | Pass — accepted four-force failure matrix. |
| 8 | Empty/non-empty seq surface, metadata, equality/hash, iteration, reduction, cache, one-shot, and concurrency pass. | Pass — focused unary oracle 24 / 1,699. |
| 9 | Generic `xf-seq` and retained Phase 2 candidates keep tested contracts. | Pass — Phase 2 suites 28 / 2,860 and preservation smoke. |
| 10 | Full local check has no lint or unexpected reflection finding. | Pass — 53 / 4,605, lint 0/0, reflection clean. |
| 11 | Phase 3 smoke, screen, decision, GC, applicability, identity, metric, and non-overwrite gates pass. | Pass — accepted receipts plus all-receipt manifest validation. |
| 12 | Direct core, public candidate, generic controls, `sequence`, `eduction`, `transduce`, and applicable Java baselines are represented. | Pass — accepted manifests/trials; transduce retained-head inapplicability is explicitly rejected. |
| 13 | Retained/unretained reductions use fresh heads with throughput/allocation evidence. | Pass — targeted decision and 74-row GC lane. |
| 14 | Every repeatable >3% regression is followed up; no added mechanism lacks its gate. | Pass — focused counts, targeted confirmation, GC/JIT/JFR diagnostics, and no unearned promotion. |
| 15 | Any custom result stays outside product code; positive evidence would stop for replanning. | Pass — Slice 6 trigger not met; no custom result prototype exists. |
| 16 | Each function receives an evidence-backed adoption recommendation. | Pass — `map`, `filter`, `remove`, and `take` all reject the compatible replacement under the no-regression gate. |
| 17 | Commands, versions, raw paths/hashes, checkpoints, skipped slices, and sequential run log are recorded. | Pass — plan sections through Slice 7 and receipts above. |
| 18 | Normal run ends at `Awaiting final review`; no Phase 4 work begins. | Pass — this plan's status is now `Awaiting final review`; Phase 4 was not started. |

### Slice 7 run log

| Date/time | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-09-01 | Slice 7 start | `/root/phase3_slice1` | Started from accepted checkpoint `819e114` / Slice 4 content checkpoint `0652f05`; audited Slice 5/6 trigger state and all Phase 3 benchmark/test files for dead or unpromoted machinery. | Slice 5 and Slice 6 remain skipped; no dead prototype was found that could be removed without losing a named receipt or validation path. |
| 2026-09-01 | Semantic gates | `/root/phase3_slice1` | Ran focused unary semantics, Phase 2 engine/candidate suites, and full local check. | Focused 24 / 1,699; Phase 2 28 / 2,860; full 53 / 4,605; lint 0/0; reflection clean; all passed. |
| 2026-09-01 | Phase 2 preservation | `/root/phase3_slice1` | Ran a fresh non-overwriting Phase 2 smoke with explicit JMH stale-lock override and validated its receipt. | 19 rows / 10 identities; result/environment hashes recorded above; prior receipts preserved. |
| 2026-09-01 | Phase 3 linkage/receipts | `/root/phase3_slice1` | Rebuilt AOT callers, reran Phase 3 linkage, and validated primary/focused/targeted screen, decision, and GC receipts against exact manifests. | Linkage has no Var lookup; all five durable Phase 3 result/manifest pairs validated with exact row counts and hashes. |
| 2026-09-01 | Consolidation | `/root/phase3_slice1` | Recorded the no-cleanup audit, exit-criterion results, checkpoint SHAs, versions/paths/hashes, and terminal status. | No production/harness implementation edits; no commit; status `Awaiting final review`. |
| 2026-09-01 | Slice 7 parent checkpoint | `/root` | Inspected the final documentation-only handoff and fresh Phase 2 receipts; independently reran the focused unary oracle, validated the fresh Phase 2 smoke, revalidated all five durable Phase 3 manifest/result pairs, checked receipt hashes/rows, exit-criterion consistency, scope, and diff hygiene. | Accepted: unary oracle 24 / 1,699; Phase 2 smoke 19 rows / 10 identities; Phase 3 receipts 184 / 237 / 309 / 72 / 74 rows with exact recorded hashes; no code change, no Phase 4 work, and status remains `Awaiting final review`. Ready for the final run-stage checkpoint commit. |
