# 2020 research state

This note freezes the research implementation that precedes Implementation #1.
It is an historical manifest, not a compatibility promise. The parent design
is [`Implementation #1: transducer-backed lazy sequences`](../01-transducer-backed-lazy-seqs.md).

## Immutable reference

The preservation point is the last 2020 research commit:

| Item | Value |
|---|---|
| Commit | `168ce02f2dcb796045990fe1647205f4da20c1f5` |
| Commit date | 2020-05-10 14:08:31 -0700 |
| Subject | Extracts processing of chunked seq to its own static method, keeping stacks small |
| Annotated tag | `research-2020-05-10` |
| Tag target | `168ce02f2dcb796045990fe1647205f4da20c1f5` |
| Local tag object | `555c01620cce3b1eeb59384008a7d30786e4a427` |
| Publication | Local only; `git ls-remote origin refs/tags/research-2020-05-10 refs/tags/research-2020-05-10^{}` returned no rows. Nothing was pushed. |

The full commit SHA is authoritative if the annotated tag is not transferred.
The tag was created only after checking that no tag of this name existed. It
must never be moved silently if a future checkout contains a conflicting tag.

The checksums below cover the research source, Java source, legacy tests and
the pinned dependency declaration (34 tracked files):

| Check | Command | SHA-256 |
|---|---|---|
| Git tree listing | `git ls-tree -r --full-tree research-2020-05-10 -- src src-java test deps.edn \| shasum -a 256` | `d9db3f7db3c8c62b786e2241c648038daa7ba5ac4f7c52e9431f25eb67da882b` |
| Tar archive | `git archive --format=tar research-2020-05-10 -- src src-java test deps.edn \| shasum -a 256` | `effaac4ee627ceb17581dc2e6c9c40b12b9e72a46c282d0239e041fdb5befc67` |

To inspect an object that is present locally, use the tag or the explicit
commit. The checkout changes `HEAD`, so run it only from a clean/disposable
worktree; the listing and archive commands themselves are read-only:

```sh
git checkout --detach research-2020-05-10
git rev-parse HEAD
git ls-tree -r --full-tree research-2020-05-10 -- src src-java test deps.edn
git archive --format=tar research-2020-05-10 -- src src-java test deps.edn | shasum -a 256
```

The expected `HEAD` value after the checkout is
`168ce02f2dcb796045990fe1647205f4da20c1f5`. A clone without the tag can use
the full SHA when the commit object is available. Tag publication is a
separate repository operation and is intentionally outside this phase.

## What is preserved

The preservation set contains four Clojure source files, 27 Java source files,
two legacy test/benchmark files, and `deps.edn`:

```text
src/xfseq/analyze.clj
src/xfseq/core.clj
src/xfseq/gen.clj
src/xfseq/protocols.clj
src-java/xfseq/AChunkedCons.java
src-java/xfseq/ACons.java
src-java/xfseq/DoubleArrayChunk.java
src-java/xfseq/DoubleChunkedCons.java
src-java/xfseq/DoubleCons.java
src-java/xfseq/IDoubleChunk.java
src-java/xfseq/IDoubleSeq.java
src-java/xfseq/ILongChunk.java
src-java/xfseq/ILongSeq.java
src-java/xfseq/LongArrayChunk.java
src-java/xfseq/LongChunkedCons.java
src-java/xfseq/LongCons.java
src-java/xfseq/XFSeqStep.java
src-java/xfseq/XFSeqStepChunkedOnly.java
src-java/xfseq/XFSeqStepChunkedOnlyNoReduced.java
src-java/xfseq/XFSeqStepSimple.java
src-java/xfseq/XFSeqStepSimpleDoubleLong.java
src-java/xfseq/XFSeqStepSimpleLong.java
src-java/xfseq/XFSeqStepSimpleLongLong.java
src-java/xfseq/XFSeqStepSimpleNoReduced.java
src-java/xfseq/XFSeqStepSimpleObjectLong.java
src-java/xfseq/XFSeqStepSingleOnly.java
src-java/xfseq/XFSeqStepSingleOnlyNoReduced.java
src-java/xfseq/buffer/DoubleBuffer.java
src-java/xfseq/buffer/IXFSeqBuffer.java
src-java/xfseq/buffer/LongBuffer.java
src-java/xfseq/buffer/ObjectBuffer.java
test/xfseq/bench.clj
test/xfseq/core_test.clj
deps.edn
```

The two Java sequence families and their buffers are research infrastructure,
not an independent public API contract. No generated classes or IDE output are
tracked. `deps.edn` declares Clojure 1.10.1, an IDE output path
(`classes/production/xfseq`), and Criterium 0.4.5 only under `:bench`.

The source paths above are unchanged from the preservation commit in the
working tree used for this phase. This check is intentionally path-limited so
the 2026 design and agent documentation do not become part of the old state:

```sh
git diff --quiet \
  168ce02f2dcb796045990fe1647205f4da20c1f5..HEAD -- \
  src src-java test deps.edn README.md CHANGELOG.md doc .gitignore
```

It exits `0` with no output. The current `HEAD` adds planning/agent documents,
but no production source, Java source, legacy test/benchmark, dependency, or
legacy documentation file in the checked path set.

## Architecture and history

The implementation has three historical paths:

1. `xfseq.core/xf-seq` defers setup behind `XFSeqHead` and `InitXFSeq`,
   selects an object/long/double buffer, analyzes the reducing function, and
   invokes a generated Clojure `deftype` step. The core namespace also exposes
   the prototype `map`, `filter`, `remove`, and `take` functions plus the
   experimental `consume` and `drain` operations.
2. `xfseq.gen/xf-seq` selects one of 54 runtime ASM constructors. Its key is
   three argument types (`Object`, `long`, `double`), two identity-stop modes,
   and three source modes (`mixed`, `chunked`, `dechunked`). It is a historical
   comparator and is not the product path in Implementation #1.
3. The hand-written Java loops under
   [`src-java/xfseq`](../../src-java/xfseq) drive the mutable buffers and
   preserve mixed, known-dechunked, known-chunked, and primitive-signature
   experiments. [`ObjectBuffer`](../../src-java/xfseq/buffer/ObjectBuffer.java)
   and its long/double siblings retain the output as Clojure seq nodes.

The source history shows the progression: Java implementations began in
January 2020; `7bd0126` (2020-05-08) added the Java loop variants used as ASM
inspiration; `c222309` (2020-05-08) made the ASM path callable;
`d1205a2` (2020-05-09) added the no-identity-stop mode;
`b542f19` (2020-05-09) added mixed/chunked/dechunked modes; and
`168ce02` (2020-05-10) extracted chunk processing to keep generated stacks
small. The parent design retains these ideas for comparison while requiring a
normal lazy-sequence surface and a real `Reduced` contract before adoption.

## Namespace surface

The following is the source-level public surface at the tag. Public helper
vars generated while developing the prototype are listed separately so they
are not mistaken for a supported API.

| Namespace | Intended or experimental operations | Classification |
|---|---|---|
| `xfseq.core` | `xf-seq [xf coll]`; `map` `[f]`/`[f coll]`; `filter`, `remove`, `take` with transducer and one-collection arities | Prototype sequence entry points; incomplete relative to `clojure.core` |
| `xfseq.core` | `consume [rf init coll]`; `drain [coll]` | Experimental fusion/deconstruction operations; known completion/composition defects |
| `xfseq.core` | `long-chunk`, `double-chunk`; `long-add`, `long-inc`, `long-even?`; `double-add`, `double-inc`, `double-even?` | Primitive research helpers, not #1 API |
| `xfseq.gen` | `xf-seq [xf coll]`; `generate-xfseq-simple`; `gen-xf-seq-class`; `xf-seq-ctors` | Runtime ASM comparator and generation support |
| `xfseq.protocols` | `IDeconstruct`, `ILongSeqable`, `IDoubleSeqable` | Internal experiment protocols; primitive/fusion work is later scope |
| `xfseq.analyze` | interface/type-hint analyzers and `xf-factory` helpers | Incidental support for generated paths |

`xfseq.core` also publishes `InitXFSeq`, `XFSeqHead`, `buffer-map`,
`xfseq-classes`, `map:xf-factory`, `filter:xf-factory`,
`remove:xf-factory`, `take:xf-factory`, `gen-deftype`,
`gen-xfseq-step`, `gen-xfseq-classes`, `class->sym`, and `gen-xfseq-name`.
These generated types, factories, and registries are implementation support;
their visibility through `ns-publics` is incidental and creates no compatibility
decision for #1. The public `xfseq.gen` helpers are `aload-local`,
`iload-local`, `type->letter`, `generate-xfseq-simple`, `gen-xf-seq-class`,
`xf-seq-ctors`, and `xf-seq`; `invoke-interface` is private. The public
`xfseq.analyze` helpers are `interfaces`, `analyze-primitive-interfaces*`,
`analyze-primitive-interfaces`, `type-hint->letter`, `hint-map->interface`,
`is-primitive?`, `apply-type-hints`, `rf-type-analyzer`,
`rf+f-type-analyzer`, `xf-factory*`, `xf-factory`, and `map:type-analyzer`.
These code-generation and analyzer names are incidental support, not an API
compatibility decision for #1.

## Stable candidate registry

These IDs are historical benchmark identities. `identity-stop` describes the
old accumulator-identity comparison; it does not claim correct Clojure
`Reduced` behavior. An ID must not be silently reused for another loop.

| Stable ID | Source/class or generated key | Intended shape |
|---|---|---|
| `core-direct` | `clojure.core` function | External semantic/performance baseline |
| `core-sequence` | `clojure.core/sequence` | Generic transducer-engine baseline |
| `legacy-clj-generated` | `xfseq.core` generated `XFSeqStep_<rf><input>` deftypes (9 type pairs) | Clojure-generated prototype path |
| `legacy-asm-<arg>-<input>-<identity-stop\|no-stop>-<mode>` | `xfseq.gen/XFSeqStep_<A><I><T\|F><M\|C\|D>`; 3 × 3 × 2 × 3 = 54 keys | ASM matrix over argument type, input type, identity-stop mode, and source mode |
| `java-polymorphic-object-identity-stop` | `XFSeqStep.ObjectStep` | Shared Java base, object input/output |
| `java-polymorphic-long-identity-stop` | `XFSeqStep.LongStep` | Shared Java base, primitive-long input |
| `java-polymorphic-double-identity-stop` | `XFSeqStep.DoubleStep` | Shared Java base, primitive-double input |
| `java-mixed-object-identity-stop` | `XFSeqStepSimple` | Mixed chunked/dechunked object loop |
| `java-mixed-object-no-stop` | `XFSeqStepSimpleNoReduced` | Mixed object loop without identity stop |
| `java-dechunked-object-identity-stop` | `XFSeqStepSingleOnly` | Known dechunked object input |
| `java-dechunked-object-no-stop` | `XFSeqStepSingleOnlyNoReduced` | Known dechunked object input without identity stop |
| `java-chunked-object-identity-stop` | `XFSeqStepChunkedOnly` | Known chunked object input |
| `java-chunked-object-no-stop` | `XFSeqStepChunkedOnlyNoReduced` | Known chunked object input without identity stop |
| `java-mixed-long-from-object` | `XFSeqStepSimpleLong` | Primitive-long reducing call over object input |
| `java-mixed-long-from-long` | `XFSeqStepSimpleLongLong` | Primitive-long reducing call over long input |
| `java-mixed-object-from-long` | `XFSeqStepSimpleObjectLong` | Object reducing call over long input |
| `java-mixed-double-from-long` | `XFSeqStepSimpleDoubleLong` | Primitive-double reducing call over long input |

The 27 Java files remain reachable at the tag, including every class in this
registry and the supporting seq/chunk/buffer classes. Phase 0 constructor-smoked
every registered Java and ASM candidate under its declared shape.
Semantic repair and comparable timing of the hand-written variants remain
Phase 2 work; this manifest does not adapt or repair them.

## Known limits of the preserved state

The unchanged `test/xfseq/core_test.clj` is one value-oriented test with 46
assertions. It is useful as a historical smoke test but does not establish
ordinary sequence behavior, realization timing, completion, reduced handling,
or fusion correctness. Planning probes recorded that the tag's implementation
has an incomplete `XFSeqHead` surface, applies a transducer twice on first
realization, loses empty completion output, omits ordinary reducing-function
completion in `consume`, loses one map in a two-stage `drain`, touches its
source during ASM construction, and omits multi-collection `map` arities.
Those observations are archaeology labels, not tests of a future repaired
implementation.

`test/xfseq/bench.clj` uses Criterium 0.4.5, size 10,000, eight source shapes,
and a value-discarding `reduce`; only the ASM block is active in the checked-in
file. Its informal timing comment has no raw output, allocation evidence,
fork metadata, or symmetric direct-linking setup. Any Phase 0 timing snapshot
therefore remains historical context only and cannot support a release or
upstream performance claim.
