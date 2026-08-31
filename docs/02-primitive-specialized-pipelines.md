# Implementation #2: primitive-specialized collection pipelines

Status: draft implementation plan
Last updated: 2026-08-31

## Summary

This document describes how to complete the primitive-specialization experiment
after the object-only lazy-sequence engine in Implementation #1 is correct,
stable, and benchmarked.

The intended user experience is ordinary collection-oriented Clojure code with
type-hinted functions:

```clojure
(def inc-long
  (fn ^long [^long x]
    (unchecked-inc x)))

(def add-long
  (fn ^long [^long acc ^long x]
    (unchecked-add acc x)))

(->> (long-array [1 2 3 4])
     (xfseq.core/map inc-long)
     (xfseq.core/filter odd?)
     (xfseq.core/reduce add-long 0))
```

The implementation should discover the primitive `IFn` interfaces emitted by
the Clojure compiler, preserve primitive values through supported transforms,
and invoke primitive reducing functions without per-element boxing.

The central requirement is an end-to-end primitive path:

```text
primitive source
  -> primitive source read
  -> primitive transform invocation
  -> primitive intermediate storage or fused handoff
  -> primitive reducing invocation
  -> one boxed public result
```

Optimizing only `map` is insufficient. If `reduce`, filtering, composition, or
the lazy-seq boundary boxes every element, the feature does not achieve its
goal.

## Dependency on Implementation #1

Implementation #2 begins only after the following #1 properties are stable:

- `xf-seq` has a correct object fallback.
- Construction, step, reduction, and completion semantics are tested.
- The public collection functions have complete supported arities.
- The build compiles Java from a clean checkout.
- JMH baselines exist for object pipelines.

Primitive specialization is an optimization of that semantic baseline. It must
never become the only implementation of an operation.

The design and acceptance criteria for #1 are in
[`01-transducer-backed-lazy-seqs.md`](01-transducer-backed-lazy-seqs.md).

## Current development environment

The local tools reported on 2026-08-31 are:

```text
OpenJDK 26.0.2.1, Homebrew build 26.0.2.1
Clojure CLI 1.12.5.1664
```

`clojure --version` reports the CLI/tool version, not the
`org.clojure/clojure` library selected by a project's `deps.edn`. The repository
currently still declares Clojure 1.10.1. Build modernization should separately
set the project runtime to Clojure 1.12.5.

Java 26 is useful for local development and forward-looking benchmarks. Final
compatibility and publication results should still include supported LTS JDKs,
especially Java 17, 21, and 25.

## Goals

1. Detect reliable primitive function signatures for `long`, `double`, and
   object arguments and return values.
2. Preserve primitive values through supported lazy `map`, `filter`, and
   `remove` operations.
3. Implement primitive reductions with and without an explicit initial value.
4. Support a fused transducer/reduction path that avoids primitive intermediate
   sequences when explicitly requested.
5. Provide exact object fallback whenever a specialization is unavailable or
   ambiguous.
6. Preserve the behavior of generic Clojure invocation, including conversion,
   exception, reduction, and realization semantics.
7. Bound generated/precompiled code size, class count, caches, and startup cost.
8. Demonstrate the absence of per-element boxing with allocation measurements,
   not merely elapsed time.
9. Keep the primitive subsystem separable from the #1 object engine and
   suitable for publication as an independent experiment.

## Non-goals

- Supporting every JVM primitive in the first implementation. Clojure's
  optimized function interfaces make `long` and `double` the natural initial
  types.
- Inferring numeric intent from collection contents.
- Treating a vector of boxed numbers as a primitive source without an explicit
  contract.
- Silently changing checked arithmetic into unchecked arithmetic.
- Automatically vectorizing with the Java Vector API.
- Supporting primitive early termination through a reducer whose declared
  return type cannot return `Reduced`.
- Generating an unbounded class per function, pipeline, or call site.
- Making runtime ASM generation a prerequisite for correctness.
- Requiring users to annotate collections. Function hints and known primitive
  source types should be sufficient for the supported path.

## What the existing prototype proves

The repository already demonstrates several important facts.

### Primitive signature discovery

[`xfseq.analyze`](../src/xfseq/analyze.clj) inspects Clojure's specialized `IFn`
interfaces and converts names such as `IFn$OLO` into argument and return type
descriptors.

This successfully recognizes the interfaces emitted for functions such as:

```clojure
(fn ^long [^long x] ...)
(fn ^double [^double x] ...)
(fn ^long [^long acc ^long x] ...)
```

### Type propagation through transducers

The analyzer rewrites transducer templates so mapping and predicate functions
can be called with `.invokePrim`, and so the generated reducing function exposes
the corresponding primitive input signature.

[`map:type-analyzer`](../src/xfseq/analyze.clj) propagates:

- the mapper's input type to the transducer step input;
- the mapper's return type to the downstream reducing-function input.

This is the essential insight needed for specialization across a transform.

### Primitive sources and outputs

The prototype includes:

- `ILongSeq` and `IDoubleSeq`;
- `ILongChunk` and `IDoubleChunk`;
- primitive array chunks;
- primitive cons and chunked-cons implementations;
- `LongBuffer` and `DoubleBuffer`;
- protocol extensions that expose `long[]` and `double[]` as primitive seqs.

This permits a primitive array to enter a specialized mapping path and a
primitive mapping function to produce a primitive lazy result.

### Specialized source drivers

The repository has two generations of driver specialization:

- nine Clojure-generated `deftype` combinations in
  [`xfseq.core`](../src/xfseq/core.clj);
- 54 ASM-generated combinations in [`xfseq.gen`](../src/xfseq/gen.clj), covering
  element type, transducer input type, reduced checking, and source chunk mode.

The checked-in tests show that long and double mapping works across object,
range, vector, set, object-array, long-array, double-array, chunked, and
dechunked inputs for the tested values.

## What remains incomplete

The current implementation should be treated as a proof of concept because:

1. Primitive reduction is explicitly unimplemented.
2. A lazy pipeline may avoid boxing inside `map` but box again at `reduce`.
3. Transducer composition loses the metadata used to select a primitive output
   buffer.
4. The analyzer examines only directly declared interfaces and records only one
   signature per arity.
5. Signature selection can depend on interface enumeration order when more than
   one candidate exists.
6. Transducer bodies are rewritten as forms and compiled with runtime `eval`.
7. The ASM implementation eagerly defines 54 classes and invokes constructors
   reflectively.
8. The Clojure-generated and ASM-generated implementations duplicate the same
   specialization problem.
9. The transducer is applied twice: once for analysis and once for execution.
10. Object-to-primitive coercion is not behaviorally uniform between the two
    engines and ordinary Clojure invocation.
11. `DoubleBuffer`'s completion arity returns `nil` rather than its accumulator.
12. Primitive chunk and seq contracts have only narrow equality coverage.

A confirmed coercion example is a type-hinted `^long` function applied to a
`Character`: ordinary generic function invocation throws `ClassCastException`,
while the Clojure-generated xfseq path currently uses `RT.longCast` and produces
the character code. Specialization must not silently introduce such behavior
changes.

## Type model

### Kinds

Use three internal value kinds:

| Kind | Symbol | JVM representation |
|---|---|---|
| object | `O` | `java.lang.Object` |
| long | `L` | primitive `long` |
| double | `D` | primitive `double` |

Do not use bare Clojure symbols as the canonical internal representation.
Introduce a small closed enum or equivalent Java/Clojure data type so unknown,
unsupported, and object are not accidentally conflated.

Suggested states:

```text
OBJECT
LONG
DOUBLE
UNKNOWN
AMBIGUOUS
```

`UNKNOWN` and `AMBIGUOUS` always select a safe fallback unless the user invokes
an explicit experimental override API.

### Function signatures

Represent a function signature as:

```clojure
{:arity  2
 :args   [:long :double]
 :return :long
 :class  clojure.lang.IFn$LDL}
```

Use compact notation in diagnostics and benchmarks:

```text
L -> L
D -> D
(L,L) -> L
(L,D) -> L
(O,L) -> O
```

Keep all signatures for an arity. If a class exposes more than one plausible
primitive signature for the same arity, the planner must either select one from
proven operand kinds or fall back. It must not overwrite one candidate in a
map.

### Signature discovery

Replace the current global memoized string parser with a bounded class-based
analyzer:

1. Walk directly declared interfaces, inherited interfaces, and relevant
   superclasses.
2. Match known nested interfaces of `clojure.lang.IFn`.
3. Parse the binary interface name after `$`, or preferably build a lookup table
   from known `IFn` interface `Class` values.
4. Validate arity and method descriptor.
5. Return a set of signatures grouped by arity.
6. Cache the immutable result in `ClassValue` so unloaded function classes and
   classloaders are not retained forever.

Add compatibility tests against both Clojure 1.12 and the active 1.13
development line.

### Proven type versus requested type

The planner must distinguish:

- **source kind**: how an element can be read without boxing;
- **function argument kind**: how a function can be invoked;
- **function return kind**: what the compiler guarantees it returns;
- **downstream input kind**: what the next step accepts;
- **accumulator kind**: how a reduction accumulator is carried.

A function accepting `long` does not prove that an object collection contains
only valid long values. Conversely, a primitive `long[]` proves the source kind
even when the mapping function accepts `Object`.

## Conversion and fallback semantics

### Safe specialization matrix

The initial planner should support these transitions:

| Source | Function argument | Action |
|---|---|---|
| `L` | `L` | direct primitive invocation |
| `L` | `D` | JVM long-to-double conversion, then primitive invocation |
| `L` | `O` | box once at the function boundary |
| `D` | `D` | direct primitive invocation |
| `D` | `L` | JVM/`Number.longValue`-equivalent narrowing, after semantic tests |
| `D` | `O` | box once at the function boundary |
| `O` | `O` | ordinary object invocation |
| `O` | `L` or `D` | generic invocation or exact checked bridge; not an assumed primitive source |

Object input with a primitive-returning function can still produce a primitive
output, but it is a partially boxed path:

```text
object source -> generic/checked function input -> primitive return -> primitive output
```

This path may be useful for vectors of numbers, but it must be benchmarked and
reported separately from a fully unboxed path.

### Generic invocation is the semantic oracle

For every specialized call shape, compare against invoking the same compiled
function through ordinary `IFn.invoke`.

Test at least:

- `nil`;
- `Character` and `Boolean`;
- `Byte`, `Short`, `Integer`, `Long`, `Float`, and `Double`;
- `BigInt`, `BigInteger`, `BigDecimal`, and ratios;
- `Long/MIN_VALUE` and `Long/MAX_VALUE`;
- positive and negative zero;
- `NaN` and infinities;
- fractional double-to-long conversion;
- values outside exact integer ranges.

If matching generic conversion would require fragile duplication of Clojure
compiler bridge behavior, call the generic bridge and accept boxing for that
boundary.

### Fallback rule

Specialization is optional. The planner must choose the #1 object engine when:

- the source kind is unknown;
- the needed function signature is absent or ambiguous;
- a pipeline changes to an unsupported element kind;
- a reducer's accumulator kind is unstable;
- early reduction requires returning an object;
- a multi-source combination has no tested specialization;
- class generation or catalog lookup fails;
- any invariant required for the primitive path cannot be proven.

Planner failure must never turn valid generic code into an exception.

## Typed pipeline model

### Stage descriptors

Each supported stage should expose an internal descriptor independent of
metadata on an anonymous function:

```clojure
{:operation    :map
 :function     f
 :input-kind   :long
 :output-kind  :double
 :cardinality  :one-to-one
 :may-reduce?  false}
```

Useful descriptor properties include:

- accepted input kinds;
- output kind;
- cardinality: one-to-one, zero-or-one, expanding, or object-producing;
- whether it preserves the original element value;
- whether it can return `Reduced`;
- whether it has state or completion output;
- the required specialized reducing-function factory.

Do not make public function metadata the sole source of this information.
Metadata is easily lost through `comp`, wrapping, and ordinary higher-order
operations.

### Operation type rules

Initial rules:

| Operation | Element result kind |
|---|---|
| `map` | mapper return kind when uniquely known |
| `filter` | input element kind |
| `remove` | input element kind |
| `take` | input element kind |
| `drop` | input element kind |
| `take-while` | input element kind |
| `drop-while` | input element kind |
| `dedupe` | input element kind if equality path is supported |
| `distinct` | input element kind, but set membership boxes in the first version |
| `keep` | object unless a non-nil primitive contract is introduced |
| `mapcat`/`cat` | object unless the nested source kind is explicit |
| `partition-*` | object |
| `interpose` | common kind only when separator and input share a proven primitive kind |

Begin with `map`, `filter`, `remove`, and `take`. Broaden the rules only after
the end-to-end reduce path is measured.

### Composition

There are two distinct composition cases.

1. **Ordinary nested lazy calls**

   ```clojure
   (reduce rf init (filter p (map f primitive-source)))
   ```

   Primitive seq/chunk interfaces can carry the kind from one realized lazy
   stage into the next. This does not require recovering metadata from
   `clojure.core/comp`, but it still allocates intermediate lazy/chunk objects.

2. **Fused transducer composition**

   A fused planner needs a structured stage list or a typed transducer object.
   An opaque function returned by `clojure.core/comp` does not reliably expose
   the final output kind.

For the experimental fused API, introduce an explicit composition operation or
typed eduction/pipeline value. It may implement `IFn` for transducer
compatibility while also implementing an internal protocol that exposes the
stage plan.

Do not change `clojure.core/comp` or infer a plan by inspecting closure fields.

## Primitive source model

### Required sources

The first fully unboxed sources are:

- `long[]`;
- `double[]`;
- `ILongSeq`/`ILongChunk` produced by xfseq;
- `IDoubleSeq`/`IDoubleChunk` produced by xfseq.

These sources have a stable element representation that can be proven without
examining values.

### Deferred sources

- Standard persistent vectors contain objects and may be heterogeneous.
- Standard ranges store primitive state internally but expose Clojure's
  existing seq/chunk interfaces, whose public element access is boxed.
- Sets and maps are object collections.
- Java streams and primitive iterators need separate lifecycle and protocol
  decisions.

They may still benefit from primitive function return paths, but they are not
fully unboxed sources in the initial claim.

### Primitive seq and chunk contracts

Retain the small interfaces:

```java
interface ILongSeq extends ISeq {
    long firstLong();
}

interface ILongChunk extends IChunk {
    long nthLong(int i);
}
```

with double equivalents.

The implementations must also satisfy ordinary Clojure behavior through their
boxed `first` and `nth` methods. Add and test:

- logical bounds checks;
- `nth(i, notFound)` behavior;
- empty chunk behavior;
- metadata preservation;
- `next`, `more`, `chunkedNext`, and `chunkedMore`;
- equality, hashing, counting, printing, and reduction;
- serialization only if it is intentionally supported.

Primitive methods are internal acceleration points. Generic consumers must see
ordinary Clojure values.

## Primitive output buffers

`LongBuffer` and `DoubleBuffer` should follow the object buffer's corrected
contract from #1:

- the reducing step returns its accumulator;
- completion returns its accumulator;
- small batches produce primitive cons cells;
- larger batches produce primitive chunks;
- oversized batches split into ordered chunks without recursive stack growth;
- reset behavior does not retain oversized storage indefinitely without
  benchmark justification.

The lazy-seq driver still uses an object accumulator token because a transducer
may return `Reduced`. Primitive element storage and primitive accumulator
reduction are separate concerns.

## Primitive reducing functions

### Explicit-initial-value reduce

Implement this path first because it has the clearest type contract:

```clojure
(xfseq.core/reduce rf init primitive-coll)
```

A fully primitive loop is eligible when:

1. The source element kind is `L` or `D`.
2. The reducing function has one unique arity-two primitive signature.
3. The initial value can enter the reducer's accumulator kind using ordinary
   Clojure conversion semantics.
4. The reducer return kind equals its first argument/accumulator kind.
5. The reducer does not need to return `Reduced` as an object.

Examples:

```text
(L,L) -> L   long accumulator, long elements
(D,D) -> D   double accumulator, double elements
(D,L) -> D   double accumulator, long elements
(L,D) -> L   long accumulator, double elements, with tested narrowing
```

The initial value is boxed at the public function boundary. Cast or unbox it
once before entering the loop, then box the final result once when returning to
Clojure.

### Reduce without an initial value

For a non-empty primitive source, the first primitive element becomes the
accumulator. The fast path initially supports reducers where:

```text
element kind == accumulator kind == return kind
```

For an empty source, invoke the reducer's zero arity generically, matching
`clojure.core/reduce`.

More complex mixed-kind inference can be added after the explicit-init path is
correct and useful.

### Early termination

A reducer declared to return primitive `long` or `double` cannot also return a
`Reduced` wrapper. Therefore:

- pure primitive reducers use a no-reduced primitive loop;
- reducers with object return use the generic reduction path and may terminate
  through `Reduced`;
- a future explicit primitive-reducer protocol may provide a side channel, but
  it is not part of the first design.

Do not use object-identity comparison to infer reduction.

### Direct array reduction

Reduce `long[]` and `double[]` directly by index. Do not first wrap them in lazy
seqs or chunks.

For xfseq-produced primitive lazy sequences:

- use primitive chunk access when present;
- fall back to primitive `firstLong`/`firstDouble` for dechunked nodes;
- advance without boxing;
- use the generic object path as soon as the primitive interface invariant is
  absent.

## Specialized transform implementation

### Baseline recommendation: bounded precompiled catalog

Replace runtime `eval` and eager ASM generation with a finite specialization
catalog generated at build time from a human-readable generator.

The catalog should contain only combinations used by supported operations. For
example, map wrappers need combinations of:

- source/input kind;
- mapper invocation signature;
- mapper return kind;
- downstream input kind;
- accumulator kind required by the downstream reducing function.

Filter wrappers additionally specialize predicate input while preserving the
original element kind.

Generate Java or Clojure `deftype` source during development/build preparation,
compile it normally, and load no new classes at a call site. Check in the
generator and either check in or reproducibly generate its output according to
the eventual contribution target's conventions.

Advantages:

- bounded class and metaspace cost;
- AOT compatibility;
- no runtime compiler dependency;
- no classloader-specific memoized `eval` results;
- ordinary direct constructor/factory calls;
- easier bytecode inspection and profiling.

### Runtime generation as an experiment

Keep the current ASM work on a research branch for comparison. Evaluate runtime
generation only if the precompiled matrix is demonstrably too large or cannot
provide important combinations.

Any runtime generator must then provide:

- a bounded cache key;
- classloader-safe unloading;
- race-free single definition;
- no reflective constructor call in the hot path;
- verified stack-map frames;
- compatible classfile versions;
- diagnostics and deterministic fallback;
- tests for reload, AOT, and repeated classloader creation.

Do not generate a class per function instance or pipeline value.

### Planner and factories

Use a small planner that returns an executable strategy:

```clojure
{:source-kind       :long
 :element-kind      :long
 :accumulator-kind  :long
 :strategy          :long-array-map-filter-reduce
 :factory           ...
 :fallback-reason   nil}
```

In development, expose planner diagnostics without printing during namespace
loading. A user should be able to ask why a pipeline did or did not specialize.

Suggested reasons include:

```text
:unknown-source
:ambiguous-function-signature
:unsupported-conversion
:object-producing-stage
:unstable-accumulator
:reduced-capable-reducer
:unsupported-arity
```

## Fused reduction

Lazy primitive sequences are useful, but the strongest performance result will
come from eliminating intermediate seqs and buffers when the sink is a reduce.

Introduce an explicit typed reducible value, conceptually:

```clojure
(->> source
     (xfseq.core/eduction
       (xfseq.core/map inc-long)
       (xfseq.core/filter even-long?))
     (xfseq.core/reduce add-long 0))
```

The reducible stores:

- the original source;
- a structured list of typed stages;
- object fallback transducers;
- no realized values.

At reduction time, the planner sees source, stage, and reducer signatures
together and selects the end-to-end strategy. This avoids guessing the final
buffer type before applying an opaque composed transducer.

An optional `consume` spelling can delegate to the same planner, but it should
not destructively deconstruct ordinary lazy sequences. The broken current
`drain` implementation should not be extended into this subsystem.

## Public API strategy

The initial experimental API may shadow core names in `xfseq.core`:

```clojure
xfseq.core/map
xfseq.core/filter
xfseq.core/remove
xfseq.core/take
xfseq.core/reduce
xfseq.core/eduction
xfseq.core/explain
```

Rules:

- A type-hinted function is sufficient to request specialization; no separate
  `map-long` name is required.
- Generic functions and collections continue to work through #1 fallback.
- Return values remain ordinary boxed Clojure values at the public boundary.
- Primitive seq/chunk interfaces are implementation details unless later use
  cases justify documenting them.
- `explain` is diagnostic and has no effect on execution.
- Avoid dynamic global flags that change numeric semantics.

An explicit `unchecked` API is unnecessary: checked versus unchecked
arithmetic is already chosen by the user's function body.

## Implementation phases

### Phase 0: freeze the semantic baseline

- Complete Implementation #1.
- Preserve the 2020 primitive implementation as a comparison branch/tag.
- Record current primitive value results and known failures.
- Separate object, primitive, generated, and benchmark namespaces.

Exit criterion: disabling specialization changes performance only, not supported
behavior.

### Phase 1: signature model and analyzer

- Introduce the closed kind and signature model.
- Discover all primitive interfaces across interface inheritance.
- Store multiple signatures per arity.
- Replace global `memoize` with `ClassValue` or an equally unloadable cache.
- Add deterministic ambiguity and fallback rules.
- Add `explain` output.
- Test against compiler-generated functions on Clojure 1.12 and 1.13.

Exit criterion: signature analysis has exhaustive deterministic tests and does
not generate or evaluate code.

### Phase 2: primitive source and sequence contracts

- Correct and fully test primitive array seqs, chunks, conses, and buffers.
- Fix completion return values and logical bounds checks.
- Add direct source-kind detection.
- Verify ordinary boxed sequence behavior.
- Add memory-retention and chunk-boundary tests.

Exit criterion: primitive collections are semantically valid even when consumed
only through ordinary Clojure interfaces.

### Phase 3: primitive reduce

- Implement direct `long[]` and `double[]` reduction with explicit init.
- Add same-kind no-init reduction.
- Add primitive chunk and dechunked-seq reduction.
- Support stable mixed accumulator/element signatures where semantics are
  proven.
- Fall back for `Reduced`-capable/object-return reducers.
- Measure allocation per element.

Exit criterion: supported primitive reductions perform no per-element boxing
and match `clojure.core/reduce` for values and failures.

### Phase 4: bounded specialized transform catalog

- Write the catalog generator.
- Generate map specializations first.
- Add filter/remove specializations that preserve primitive elements.
- Add take as a kind-preserving stateful transform.
- Use direct factories rather than reflection.
- Retire runtime `eval` from the primary path.

Exit criterion: primitive array -> lazy primitive map/filter -> primitive reduce
works without per-element boxing in supported combinations.

### Phase 5: typed composition and fused reduction

- Introduce typed stage descriptors.
- Implement typed `eduction` or pipeline composition.
- Plan source, stages, and reducer together.
- Add fused map, filter, remove, and take combinations.
- Retain object transducers for fallback.
- Compare fused execution with `transduce` and hand-written loops.

Exit criterion: representative multi-stage pipelines allocate independently of
element count, apart from source/result storage required by the workload.

### Phase 6: broaden supported operations

Evaluate, in order:

1. `drop` and `take-while`/`drop-while`;
2. `map-indexed`, with a primitive long index;
3. `dedupe`;
4. primitive-compatible `interpose`;
5. object-producing operations such as partitioning, primarily for fused
   primitive input rather than primitive output.

Each operation needs explicit type, state, completion, and cardinality rules.

Exit criterion: every new operation has independent semantic and allocation
evidence.

### Phase 7: code-generation decision

- Compare precompiled catalog size and performance with the preserved ASM path.
- Measure startup, class loading, metaspace, steady-state throughput, and native
  image/AOT compatibility.
- Keep runtime generation only if it wins materially and its lifecycle is
  bounded.
- Delete the losing primary implementation rather than maintaining two equal
  production paths.

Exit criterion: one generation strategy is selected and documented.

### Phase 8: publication and upstream research

- Publish #1 independently first.
- Present #2 as a separate optimization study.
- Identify whether the useful upstream unit is compiler metadata, primitive
  collection interfaces, reducing loops, or higher-order function changes.
- Discuss interface and compiler implications before proposing a core patch.
- Keep any first proposal much smaller than the whole experimental subsystem.

Exit criterion: claims clearly separate lazy-seq improvements, primitive
dispatch, primitive storage, and fused reduction.

## Correctness test plan

### Signature fixtures

Compile functions covering all supported unary and binary combinations:

```text
O -> O, O -> L, O -> D
L -> O, L -> L, L -> D
D -> O, D -> L, D -> D

(O,O) -> O/L/D
(O,L) -> O/L/D
(O,D) -> O/L/D
(L,O) -> O/L/D
(L,L) -> O/L/D
(L,D) -> O/L/D
(D,O) -> O/L/D
(D,L) -> O/L/D
(D,D) -> O/L/D
```

Not every signature needs a production specialization, but the analyzer must
classify each one accurately.

Also cover:

- ordinary object-only functions;
- multi-arity functions;
- inherited interfaces;
- deliberately ambiguous implementations;
- reified/custom `IFn` implementations;
- reloaded namespaces and disposable classloaders.

### Source matrix

- empty, one-element, and boundary-sized `long[]` and `double[]`;
- primitive xfseq chunks of sizes 1, 4, 8, 31, 32, 33, 64, and 65;
- dechunked primitive seqs;
- object arrays containing numeric and invalid values;
- vectors with homogeneous, heterogeneous, `nil`, and invalid values;
- ranges, sets, and maps through fallback;
- infinite primitive seqs terminated by take/reduction.

### Numeric edge matrix

- long minimum, maximum, zero, and values around overflow;
- double positive/negative zero;
- subnormal doubles;
- `NaN` payload/behavior where observable;
- positive and negative infinity;
- long-to-double values around exactness boundaries;
- double-to-long fractional, out-of-range, `NaN`, and infinite values;
- boxed numeric subclasses;
- `nil`, character, boolean, keyword, and arbitrary objects.

Compare value or exception behavior with ordinary invocation of the exact same
compiled function.

### Pipeline matrix

- long -> long map;
- long -> double map;
- double -> long map;
- double -> double map;
- primitive -> object map and object -> primitive-return map;
- primitive-input predicate filter/remove;
- map then filter, filter then map, five maps, and take in every position;
- lazy stage-by-stage and fused typed-eduction forms;
- reduction with explicit init, without init, empty input, and one element;
- reducer return kind matching and not matching accumulator kind;
- object-return reducer using `Reduced`;
- stateful/completing transforms through fallback.

### Differential and property tests

For generated primitive arrays and numeric functions:

1. Run the generic core/object implementation.
2. Run the planned specialized implementation.
3. Compare values, result types at the public boundary, exceptions, function
   invocation counts, and source consumption.

Property generation must be constrained enough to avoid declaring undefined or
intentionally different unchecked-overflow behavior a failure.

### Allocation assertions

JMH allocation profiling is the primary proof. Supplement it with focused
tests or profiles showing that:

- array reduction does not allocate per element;
- a fused supported pipeline does not allocate per element;
- lazy primitive pipelines allocate seq/chunk structure but not boxed numeric
  objects per element;
- fallback allocation resembles the #1 object implementation;
- caches and generated classes remain bounded across repeated distinct
  function instances and classloaders.

## Benchmark plan

### Baselines

Compare each relevant workload against:

1. A hand-written primitive Java loop.
2. Clojure `areduce` or an equivalent type-hinted loop.
3. `clojure.core/map` plus `reduce`.
4. `sequence` plus `reduce`.
5. `transduce` with type-hinted functions.
6. The #1 object-only xfseq engine.
7. Lazy primitive xfseq.
8. Fused primitive xfseq.
9. The preserved 2020 implementation where it remains runnable.

The hand-written loop is a performance ceiling/reference, not a semantic
replacement.

### Workloads

- sum a primitive array;
- map increment then sum;
- long-to-double map then sum;
- double-to-long map then sum;
- filter at 0%, 1%, 50%, 99%, and 100% selectivity then reduce;
- five-stage arithmetic map pipeline;
- alternating map and filter stages;
- early take from a large array/primitive seq;
- lazy `first`, small prefix, and full traversal;
- conversion to vector, intentionally showing the cost of boxing at an object
  sink;
- reducers doing cheap arithmetic and more substantial computation.

### Sizes

Include sizes that expose fixed overhead and steady state:

```text
0, 1, 8, 32, 1,000, 10,000, 1,000,000
```

### Metrics

- nanoseconds per operation;
- nanoseconds per element;
- throughput;
- bytes per operation and per element;
- allocation count where available;
- GC count/time;
- class-loading and initialization time;
- loaded class count and metaspace for generator comparisons;
- generated code size;
- branch/instruction profiles for representative cases when useful.

Use async-profiler/JFR allocation profiles to confirm the classes of remaining
allocations.

### Runtime matrix

Primary stable publication runs:

- Clojure 1.12.5;
- Java 17, 21, and 25.

Forward-looking runs:

- the installed Java 26.0.2.1;
- Clojure 1.13.0-alpha6, the active development release as of 2026-08-31.

Record Clojure library version separately from the Clojure CLI version.

### Reporting

Separate results into four categories:

1. source access specialization;
2. primitive function invocation;
3. primitive intermediate storage;
4. primitive fused reduction.

This prevents a large fused-pipeline win from being incorrectly attributed to
the lazy-seq engine or mapper invocation alone.

## Acceptance criteria

### Semantic milestone

- Every specialization has a tested object fallback.
- Planner ambiguity always falls back deterministically.
- Generic invocation is the conversion/error oracle.
- Public results match core values and public boxed types.
- Primitive seq/chunk implementations satisfy ordinary Clojure contracts.
- Unsupported `Reduced`, completion, state, and arity cases remain correct
  through fallback.
- No transducer is initialized twice.
- No specialization depends on interface iteration order.

### End-to-end primitive milestone

- Explicit-init long-array and double-array reductions have no per-element
  boxing.
- Supported lazy map/filter pipelines do not allocate boxed numeric objects per
  element.
- Supported fused map/filter/reduce pipelines allocate independently of element
  count, excluding intentional source/result allocation.
- Final public results are boxed once as ordinary Clojure values.
- A diagnostic explains the chosen specialization or fallback reason.

### Engineering milestone

- Primary execution does not require runtime `eval`.
- Class count and cache growth are bounded and tested.
- AOT and namespace reload behavior are documented.
- The specialization catalog is generated reproducibly.
- Java 17, 21, 25, and local Java 26 tests pass for the supported Clojure
  versions.
- No unexpected reflection occurs in hot paths.

### Performance milestone

- Final claims come from forked JMH runs with GC/allocation profiling.
- Primitive reduction is materially faster than object fallback for large
  primitive arrays.
- Fused pipelines approach the hand-written primitive-loop baseline closely
  enough to justify their complexity.
- Fixed planning and initialization costs are reported for small collections.
- Any specialized case slower than fallback is either corrected or removed
  from automatic selection.

Set numeric thresholds after the first stable harness run. A useful initial
goal is zero per-element numeric boxing and a clear large-input win, rather than
prematurely committing to one throughput ratio across JVMs.

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| Signature inference changes behavior | Treat generic invocation as oracle; specialize only proven transitions. |
| Specialization matrix explodes | Support a small operation/type set; use bounded generated catalog and fallback. |
| `comp` loses type information | Use structured typed pipeline values for fused execution; let ordinary composition fall back. |
| Primitive work is lost at `reduce` | Implement primitive reduction before claiming pipeline success. |
| Object collections are assumed homogeneous | Never infer primitive source kind from sampled values. |
| Runtime code generation leaks classes | Keep it off the primary path; use bounded caches and compare against precompiled catalog. |
| Custom primitive seqs break Clojure APIs | Test their complete boxed seq/chunk surface independently. |
| Mixed conversion semantics diverge | Use generic bridges for uncertain object-to-primitive boundaries. |
| Cheap benchmarks exaggerate dispatch wins | Include substantial functions and report per-element allocation. |
| New JDK optimization changes conclusions | Run the LTS matrix plus local Java 26 and publish exact versions. |
| Complexity blocks upstream adoption | Publish as a separable experiment and propose smaller compiler/runtime pieces. |

## Likely publication structure

Implementation #2 is best presented as a sequel to #1:

1. What Clojure's primitive `IFn` interfaces already encode.
2. Recovering function signatures at runtime.
3. Why a fast primitive mapper is not enough.
4. Primitive sources, intermediate chunks, and reduction.
5. The conversion and fallback traps.
6. Precompiled specialization catalog versus runtime ASM.
7. Allocation profiles proving where boxing disappeared.
8. Comparisons with hand-written loops, `areduce`, core seqs, and transducers.
9. What would need compiler or core support to make this transparent.

Report negative results as carefully as wins. Cases that fall back or become
slower define the useful boundary of the technique.

## Upstream considerations

The complete #2 system is unlikely to be the right first Clojure core patch. It
crosses compiler interfaces, sequence/chunk representation, higher-order
function dispatch, reduction, and code generation.

Potential smaller upstream units include:

- a supported API for obtaining a function's primitive signatures;
- primitive seq/chunk interfaces owned by Clojure;
- optimized primitive-array reduction paths;
- compiler propagation of primitive higher-order function information;
- internal typed transducer factories for a small number of operations.

The library should first establish which layer produces most of the benefit.
That evidence can determine whether the idea belongs in `clojure.core`, the
compiler, a contrib library, or an independent performance library.

Any core contribution must follow the current Clojure contribution agreement
and patch-development rules. The existing human-authored prototype can inform
that work; future core-bound code must also satisfy the project's authorship
requirements.

## Decision log

1. **#1 is the fallback.** Primitive code never defines semantics by itself.
2. **Long and double first.** Do not generalize before proving the main path.
3. **End-to-end or no claim.** Source, transforms, storage, and reduce are
   measured separately and together.
4. **Generic invocation is the oracle.** Type hints authorize optimized calling
   conventions, not different coercion behavior.
5. **Unknown object collections stay object collections.** No sampling-based
   specialization.
6. **Primitive reduce precedes broad transform coverage.** It closes the most
   important boxing boundary.
7. **Bounded precompiled catalog is the baseline.** ASM remains comparative
   research until it proves necessary.
8. **Multiple signatures are preserved.** Ambiguity cannot depend on interface
   order.
9. **Structured plans for fusion.** Opaque `comp` closures are not introspected.
10. **Ordinary lazy nesting remains supported.** Fusion is an explicit faster
    path, not a requirement for correctness.
11. **No per-element boxing is an allocation claim.** It must be demonstrated
    with profiling.
12. **CLI and runtime versions are recorded separately.** Tool installation
    does not silently define the project Clojure version.

## External references

- [Clojure primitive support reference](https://clojure.org/reference/java_interop#primitives)
- [Clojure 1.12.5 source](https://github.com/clojure/clojure/tree/clojure-1.12.5)
- [Clojure stable and development releases](https://clojure.org/releases/downloads)
- [OpenJDK Java Microbenchmark Harness](https://github.com/openjdk/jmh)
- [Criterium](https://github.com/hugoduncan/criterium)
- [async-profiler](https://github.com/async-profiler/async-profiler)
- [Clojure development workflow](https://clojure.org/dev/workflow)
- [Clojure patch-development guidance](https://clojure.org/dev/developing_patches)
