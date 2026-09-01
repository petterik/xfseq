# Implementation #1, Phase 3: unary core-function compatibility

Status: Ready for implementation

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

Slice 1 is implemented and awaiting the parent checkpoint review.  It changes
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
