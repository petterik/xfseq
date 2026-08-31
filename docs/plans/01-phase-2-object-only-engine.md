# Implementation #1, Phase 2: object-only engine

Status: Implementation in progress (Slices 1–3 checkpointed; Slice 4 not
started)

Stage: plan complete; pre-implementation review passed; Slice 1 implemented,
accepted, and checkpointed; Slice 2 implemented and accepted by parent
validation and checkpointed; Slice 3 implemented, accepted, and checkpointed;
Slice 4 not started

Last updated: 2026-09-01

Parent design: [`docs/01-transducer-backed-lazy-seqs.md`](../01-transducer-backed-lazy-seqs.md)

## Plain-English problem

Replace the prototype's generated, primitive-aware sequence path with the
smallest correct object-only lazy-sequence driver, then measure every repaired
hand-written object loop so later refactors are compared with the fastest
correct implementation rather than the easiest baseline.

The desired outcome is one ordinary `LazySeq` with correct transducer state,
completion, reduction, chunking, and retention behavior. `ObjectXFSeqInit`, a
mutable buffer, and Java loop classes are candidate mechanisms, not goals: if a
single mixed loop wins, Phase 2 should not add dispatch; if specialization wins
materially, it must have a structural and reviewable selection rule.

## Phase goal

1. Make `xfseq.core/xf-seq` return `clojure.lang.LazySeq` directly and defer all
   source and transducer work until that lazy node is forced.
2. Introduce a correct object-only initializer and use repaired
   `XFSeqStepSimple` as the provisional mixed-source Java step before
   considering specialization.
3. Apply the transducer once, preserve every returned accumulator, detect
   `Reduced` with `RT.isReduced`, unwrap it once, and complete exactly once.
4. Complete empty input and retain completion-emitted values in order.
5. Process one input chunk at a time for chunked sources and incrementally seek
   the next output for dechunked sources, without an empty lazy node per rejected
   input.
6. Repair the retained object Java variants to an explicitly stated contract,
   give repaired candidates new stable benchmark IDs, and preserve the 2020 IDs
   as historical identities.
7. Repair or replace `ObjectBuffer` so output batches are ordered, bounded,
   cleared, and split into chunks of at most 32 without recursive stack growth.
8. Remove primitive analysis and runtime-generated Clojure step selection from
   the `xf-seq` execution path. The historical tag and Phase 0 evidence remain
   the immutable record; the ASM namespace remains outside the product path.
9. Add direct JMH 1.37 throughput and allocation benchmarks, run the local
   direct-linking-on decision matrix, and select the simplest production loop
   strategy supported by the data.
10. Record whether the engine is promising enough to justify Phase 3. Phase 2
    can establish a correct fastest baseline without claiming that
    Implementation #1 has yet earned an upstream proposal.

## Non-goals

- Do not redesign or claim direct-core compatibility for unary `map`, `filter`,
  `remove`, or `take`. Their current collection arities already call `xf-seq`
  and therefore inherit the repaired engine, but Phase 3 owns their transducer
  cleanup, arities, direct-function realization traces, and adoption decision.
- Do not implement multi-source `map`; Phase 4 owns it.
- Do not add other core functions; Phase 5 owns candidate expansion.
- Do not repair or redesign `consume`, `drain`, or deconstruction. Returning a
  real `LazySeq` intentionally removes `XFSeqHead` from the public path; Phase 6
  owns any explicit fusion contract.
- Do not add long/double buffers, primitive seq protocols, type inference,
  runtime `eval`, generated `deftype`, or ASM to the #1 execution path.
- Do not delete the hand-written primitive or ASM research sources. They remain
  reachable history and belong to Implementation #2 or historical comparison.
- Do not add source-class whitelists, sampled dispatch, flags, cache-generated
  classes, or a second public engine to hide a losing design.
- Do not add CI, alternate JDKs, a Clojure prerelease, or a direct-linking-off
  lane. Those are later compatibility or diagnostic costs if this local
  candidate is promising.
- Do not claim direct `map`/`filter` replacement speed from a generic
  `sequence` comparison. Applicable direct-core rows are context until Phase 3
  establishes function-specific semantic equivalence.

## Why this matters upstream

- **Clojure core maintainer:** the proposed boundary is one lazy initializer,
  one stateful loop, and one buffer; every additional loop or dispatch branch
  must buy a repeatable material result.
- **Library user:** the result is an ordinary cached lazy sequence, and custom
  transducers do not lose accumulators, completion output, or early termination.
- **Performance/JVM engineer:** repaired Java candidates are compared in forked,
  warmed, allocation-profiled, direct-linked runs with source shape and sink
  separated instead of pooled.
- **Future contributor:** historical IDs, repaired IDs, decision rules, raw
  results, and rejected alternatives explain why each retained class exists.

## Current repository facts and baseline

### Phase handoff and local environment

- Planning starts at `8e13124de85237556316db78ba8841f52b46d8f3` on
  `master`; the worktree was clean before this plan was created.
- Phase 0 preserves the 2020 tree at
  `168ce02f2dcb796045990fe1647205f4da20c1f5` and the local annotated tag
  `research-2020-05-10`. Its old timing rows are explicitly non-equivalent and
  non-decisional.
- Phase 1 is complete. `clojure -Srepro -T:build check` passes from the current
  checkout with clj-kondo clean, no compiler reflection warning, and 1 test / 46
  assertions / 0 failures / 0 errors.
- The build compiles 27 Java sources to 30 Java 8-compatible class files under
  `target/classes`.
- The exact Phase 2 research runtime is Clojure library 1.12.5, Clojure CLI
  1.12.5.1664, Homebrew OpenJDK 26.0.2.1, macOS 26.2 / Darwin kernel 25.2.0,
  and arm64.

### Current public and execution behavior

- `xfseq.core/xf-seq` returns `XFSeqHead`, which is `Seqable` and `Sequential`
  but not a normal `ISeq`. The committed Phase 0 report shows `count` and `vec`
  failures.
- Construction of `xfseq.core/xf-seq` is source-lazy, but first realization
  applies a custom transducer twice: once for analysis and once for execution.
- `InitXFSeq` chooses primitive/object buffers, analyzes reducing-function
  interfaces, and selects one of nine generated Clojure `deftype` steps.
- `gen-xfseq-classes` runs while `xfseq.core` loads. The generic object path is
  therefore still coupled to generated and primitive research even before the
  user selects a primitive operation.
- Empty input bypasses transducer application and completion, losing completion
  output.
- The current loops compare the returned accumulator with the buffer by object
  identity. They neither call `RT.isReduced` nor retain an ordinary changed
  accumulator.
- The current `ObjectBuffer` starts at capacity 8, grows by multiplication,
  uses `Cons` for one to four values, and uses `ArrayChunk` thereafter. It lacks
  overflow checks; terminal output can expose one chunk larger than 32; its
  small-result and reset choices have no modern allocation evidence.
- The existing normal suite is a fully realized value smoke. It does not cover
  construction, accumulator changes, empty completion, completion ordering,
  chunk call counts, exceptions, concurrency, or retention.

### Retained hand-written object candidates

The historical IDs continue to name the old semantics and must not be silently
reused for repaired code.

| Historical ID | Class | Shape | Current defect |
|---|---|---|---|
| `java-polymorphic-object-identity-stop` | `XFSeqStep.ObjectStep` | shared-base mixed | identity stop, discarded accumulator |
| `java-mixed-object-identity-stop` | `XFSeqStepSimple` | self-contained mixed | identity stop, discarded accumulator |
| `java-mixed-object-no-stop` | `XFSeqStepSimpleNoReduced` | mixed, no stop check | only correct under an unproved non-reducing precondition |
| `java-dechunked-object-identity-stop` | `XFSeqStepSingleOnly` | known dechunked | identity stop, discarded accumulator |
| `java-dechunked-object-no-stop` | `XFSeqStepSingleOnlyNoReduced` | known dechunked, no stop check | restricted precondition only |
| `java-chunked-object-identity-stop` | `XFSeqStepChunkedOnly` | known chunked | identity stop, discarded accumulator |
| `java-chunked-object-no-stop` | `XFSeqStepChunkedOnlyNoReduced` | known chunked, no stop check | restricted precondition only |

Repaired IDs use a `v2` suffix and say what is proved, for example
`java-mixed-object-reduced-aware-v2` and
`java-chunked-object-nonreducing-v2`. The implementation must add an explicit
old-to-new mapping in the benchmark registry. A no-reduced variant is “correct”
only in matrix cells whose exact operation structurally cannot return
`Reduced`; metadata supplied by an arbitrary caller is not proof.

### Exact oracle observations from Clojure 1.12.5

Local planning probes and the Clojure 1.12.5 jar establish that oracle choice
must be per observable:

- `clojure.core/sequence` calls `RT/iter` and
  `TransformerIterator/create` during construction. With a traceable `Seqable`,
  construction called source `seq`, applied the transducer, and stepped the
  first item before returning its `LazySeq`.
- On a 64-element vector, first realization caused 33 mapping calls through
  `sequence` and 64 predicate calls through a 50%-selective filtering
  `sequence`. Direct unary `map` and `filter` each processed exactly the first
  32-element input chunk.
- `TransformerIterator` passes `nil` as its accumulator and ignores ordinary
  accumulator changes. It is not the oracle for Phase 2's stronger returned-
  accumulator requirement.
- Direct unary core functions return `LazySeq`, defer construction work, and
  process one input chunk at a time. The parent #1 design deliberately chooses
  those properties for the future shared engine.
- Clojure 1.12.5 `LazySeq` uses a lock and clears its thunk after forcing. It is
  the concurrency/caching mechanism around the mutable step; the step must not
  introduce a second realization mechanism.

Therefore:

| Observable | Primary oracle |
|---|---|
| Fully realized values and completion output | Clojure 1.12.5 `sequence` plus `transduce` for full output |
| Construction laziness and input-chunk behavior | Parent #1 contract and corresponding direct unary core function |
| Returned accumulator and `Reduced` protocol | Clojure reduction/transducer contract (`transduce`, `RT.isReduced`, one `unreduced`) |
| Ordinary sequence surface, equality, hashing, printing, caching, concurrency | `clojure.lang.LazySeq` and direct core lazy results |
| Candidate-specific source preconditions | Deterministic custom chunked/dechunked sources, not collection-name guesses |

This split is intentional and testable. Phase 2 does not claim to replace
`clojure.core/sequence`, whose current construction and iterator realization
timing differ.

## Semantic contract for the object engine

### Initialization order

`xf-seq` itself only constructs `LazySeq(ObjectXFSeqInit)`. When forced, the
initializer must:

1. call `RT.seq` on the source;
2. create the object buffer;
3. apply the transducer to that buffer exactly once;
4. use the buffer as the initial accumulator;
5. if the source is empty, call completion with that accumulator and return the
   flushed completion output;
6. otherwise construct and invoke the selected object step.

Source `seq` deliberately precedes transducer application, so a source-seq
exception occurs without transducer-initialization effects. Neither happens at
construction.

The transformed reducing function's zero-arity is not called. This matches the
explicit initializer design and the initialized form of `transduce`; the buffer
itself is the supplied initial accumulator.

### Step and termination state

The state carried by a reduced-aware step is:

```text
ObjectBuffer buffer
IFn reducingFn
Object accumulator
ISeq source
boolean completed
```

For every input, assign the exact return from `reducingFn(accumulator, input)`
back to `accumulator`. If `RT.isReduced` is true, dereference it once, stop
before another source item, invoke completion once with the unwrapped value,
flush step output followed by completion output, clear terminal references, and
return no continuation.

Natural exhaustion follows the same completion path. Do not probe a lazy tail
solely to discover exhaustion earlier: completion happens when exhaustion is
observed by the normal next step. This avoids extra source access during partial
consumption.

For a chunked node, process the complete current input chunk unless `Reduced`
terminates it. For a dechunked node, continue across rejected inputs and return
as soon as at least one output exists. All outputs from one processed input
batch precede one `LazySeq` continuation.

`completed` is a defensive state invariant, not an alternate concurrency
mechanism. `LazySeq` owns exactly-once forcing. A repeated direct candidate
invocation after terminal completion must not rerun completion or the source.

### Buffer contract

The object buffer is both the downstream reducing function and private mutable
storage. It must:

- return the accumulator argument from its two-arity step after appending the
  value;
- make its completion arity return the accumulator without adding output;
- grow with an explicit maximum/overflow check;
- keep output order for zero, one, 32, 33, and much larger batches;
- expose chunks no larger than 32;
- avoid recursive construction proportional to output count;
- clear or replace every mutable slot transferred out of the buffer;
- reset an oversized working array to a bounded capacity after flush unless a
  measured reuse policy is selected;
- never mutate an array already visible through an `ArrayChunk`;
- produce `nil` for an empty flush and an ordinary `ISeq` tail otherwise.

The current one-to-four `Cons` fast paths and initial capacity 8 are candidates,
not contract. Retain them in the first correct version, then remove or change
them only from allocation and throughput evidence.

## Options and trade-offs

### Production loop boundary

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| One final mixed reduced-aware Java step | Smallest product path; safe for arbitrary `xf-seq` input | Per-node chunk test and reduced check may cost throughput | **Provisional simplest option and default unless measurements justify more** |
| Shared state/completion base with mixed/chunked/dechunked subclasses | One semantic state machine; variants differ only in input loop | Virtual calls or abstraction can remain in the hot loop | Repair and benchmark as a candidate |
| Self-contained specialized classes | Maximum JIT visibility; preserves historical shapes | Duplicated state/completion code is costly to audit | Keep only if it materially beats the common form |
| One-time source/operation dispatcher | Can avoid per-element checks | Needs a structural proof that the full source/tail keeps the selected shape; adds branches and policy | Add only after decision data and a safe proof |
| Runtime-generated or primitive-selected loop | Large specialization space | Violates #1 scope and hides complexity in generation | Reject |

No-reduced loops cannot serve arbitrary `xf-seq`. They remain restricted
benchmark candidates in Phase 2. Phase 3 may use one only when the public
operation itself, not caller metadata, proves it cannot return `Reduced`.

### Initialization and public surface

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| `LazySeq(ObjectXFSeqInit)` directly | Standard sequence protocols and locking; no custom head | Fusion cannot deconstruct ordinary results | **Choose** |
| Repair `XFSeqHead` into a complete seq type | Could retain deconstruction | Reimplements a large protocol surface and couples Phase 6 | Reject |
| Eagerly create source/rf then wrap only the step | Fewer objects at first force | Violates chosen construction behavior and exception order | Reject |

Remove obsolete generated Clojure step initialization from the active
`xf-seq` path. Do not move it into another production namespace merely to keep
dead machinery alive; the preservation tag is the authoritative historical
copy. Keep `xfseq.gen` isolated as a historical comparator, not required by
normal tests or public engine benchmarks.

### Buffer shape

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| Repair current array buffer and retain initial small fast paths | Small diff; preserves strongest old hypothesis | More branches; benefits unproven on JDK 26 | **Start here, measure each branch policy** |
| Always emit 32-sized `ArrayChunk` batches | Very simple and regular | One-value dechunked outputs may allocate more | Benchmark alternative |
| Persistent/transient collection as buffer | Simpler ownership semantics | Extra nodes and generic collection overhead likely dominate | Reference experiment only if array ownership remains unclear |
| Reuse arbitrarily large arrays | Reduces reallocations for repeated expansion | Retains memory after one large batch | Reject unless retained-size and throughput data reverse the decision |

### Benchmark scope

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| Criterium only | Existing dependency and quick REPL feedback | No direct hand-written-loop isolation or publishable allocation evidence | Keep only for exploration |
| Direct JMH 1.37 Java harness plus AOT Clojure wrappers | Forked, allocation-aware, stable params; can isolate loop and public call | Build work and a deliberate matrix are required | **Choose** |
| Full final upstream on/off and multi-JDK matrix now | Strong publication evidence | Premature before a candidate survives local Phase 2 | Defer |

## Selected approach

1. Add semantic tests first alongside `ObjectXFSeqInit`, repaired
   `ObjectBuffer`, and repaired `XFSeqStepSimple` as the provisional mixed
   reduced-aware step. Route generic `xf-seq` through it and return `LazySeq`
   directly; do not add a second canonical mixed loop.
2. Remove the old generated/primitive step selection from that execution path.
   Preserve unrelated primitive/ASM sources and do not repair fusion.
3. Repair and preserve the named polymorphic, mixed, known-dechunked,
   known-chunked, and restricted no-reduced object classes against shared
   deterministic fixtures. Prefer a shared state/completion implementation;
   retain duplicated loop bodies only if JMH demonstrates a material advantage.
4. Add JMH 1.37 with direct-linked AOT wrappers for public paths and Java-level
   benchmarks for the loop/buffer candidates. Verify linkage and benchmark
   identity before accepting scores.
5. Run correctness gates before every timing run. Screen variants, then run the
   exact decision subset with three forks and GC profiling. Inspect JIT/inlining
   for the selected winner and every material reversal.
6. Select one mixed production loop by default. Add one-time specialization
   only when the targeted rows improve by at least 5%, fork samples and reported
   uncertainty support the direction, the improvement repeats after rerun, no
   supported primary row regresses by more than 3%, allocation does not reverse
   the result, and the source/operation precondition is structural.
7. Keep the fastest correct hand-written result for each applicable matrix cell
   as a named later baseline even if a simpler universal loop becomes the
   production choice. Do not pool rows into an overall winner.

## Impact / Effort / Value priorities

| Item | Impact | Effort | Value | Dependency/evidence | Decision |
|---|---|---|---|---|---|
| Correct accumulator, `Reduced`, and completion state machine | High | High | High | Direct reduction contract; current loops fail it | Now, first |
| Real `LazySeq` and deferred initializer | High | Medium | High | Current `XFSeqHead` surface fails | Now |
| Differential values and realization/exception traces | High | High | High | Correctness gates every benchmark | Now |
| Buffer ownership, chunk bounds, and slot clearing | High | Medium | High | Current terminal chunk can exceed 32 | Now |
| Repair/name every object Java candidate | High | High | High | Phase 0 stable inventory | Now |
| Direct JMH throughput/allocation harness | High | High | High | Needed to select rather than guess | Now |
| Mixed-vs-specialized production selection | High | Medium | High | Depends on correct candidate JMH rows | Decision gate |
| No-reduced public dispatch | Medium later | Medium | Medium | Generic xform cannot prove precondition | Experiment only; no Phase 2 public use |
| Direct-linking-off diagnostics | Low now | Medium | Low now | No release claim; symmetric off core build absent | Later if promising |
| Other JDKs/CI | Low now | High | Low now | Candidate value unknown | Later if promising |
| Unary core wrappers | High later | Medium | High later | Correct object engine first | Phase 3 |
| Fusion and primitive machinery | Low for #1 Phase 2 | High | Negative now | Separate contracts/design | Reject/defer |

## Confidence ledger

| ID | Kind | Statement / failure mode | Resolution | Confidence |
|---|---|---|---|---|
| C1 | Fact | Current `xf-seq` returns incomplete `XFSeqHead`. | Replace the public return with `LazySeq`; run full surface tests. | High; committed Phase 0 evidence. |
| C2 | Fact | Current first realization applies the transducer twice and empty input never completes. | Count init/step/completion events for empty, non-empty, natural exhaustion, and reduction. | High; committed report and source inspection. |
| C3 | Fact | Every retained reduced-aware object loop uses accumulator identity rather than `RT.isReduced` and discards accumulator changes. | Candidate contract tests use a changing token accumulator and a non-buffer `Reduced`. | High; all candidate sources inspected. |
| C4 | Fact | Clojure `sequence` is not the construction/timing oracle selected by the parent design. | Record the oracle split and compare traces with direct unary core behavior. | High; local 1.12.5 probe and bytecode inspection. |
| C5 | Fact | `LazySeq` provides synchronized once-only forcing and clears the thunk. | Concurrently force the same initial and continuation node; require one event sequence. | High on Clojure 1.12.5. |
| C6 | Assumption | One mixed reduced-aware loop is fast enough to be the only generic product loop. | Compare it with repaired shared-base and self-contained candidates across source/sink reversals. | Medium; simplest but unmeasured. |
| C7 | Unknown | A shared state/completion base changes inlining or interface dispatch enough to lose materially. | JMH Java-level rows plus `PrintInlining`/JFR evidence for representative cases. | Low until measured. |
| C8 | Unknown | Current `Cons` cases 1–4 and initial capacity 8 remain allocation/throughput wins on JDK 26. | Parameterized buffer microbench and public dechunked/expanding rows; choose one fixed policy. | Low; only a Java 8-era comment supports it. |
| C9 | Unknown | A safe one-time source-shape dispatch exists for arbitrary seq tails. | Require a proof from concrete source API/class semantics and adversarial mixed-tail tests before dispatch. Otherwise use mixed. | Low; `IChunkedSeq` describes the current node, not every tail. |
| C10 | Assumption | Exactly one `unreduced` before completion matches ordinary transducer reduction. | Differential custom transducers including nested/preserve-reduced composition against `transduce`. | High, but must be tested. |
| C11 | Failure mode | A no-reduced candidate looks fastest because it silently ignores early termination. | Exclude it from arbitrary-xform rows; test only structurally non-reducing operations and label the precondition in every result. | High confidence in mitigation. |
| C12 | Failure mode | A chunk-only candidate is fed a source with a dechunked later tail. | Deterministic mixed-tail negative tests and no production dispatch without whole-source proof. | High confidence in mitigation. |
| C13 | Failure mode | Output arrays remain reachable or are mutated after exposure. | Reflection-assisted buffer ownership tests, weak-reference retention stress, and allocation profiling. | Medium; GC checks require repeated bounded trials. |
| C14 | Failure mode | Candidate/core timing is asymmetric through Var calls or compilation mode. | AOT both benchmark wrappers and candidate namespaces direct-on; inspect bytecode/property and record released core identity. | High confidence in mitigation. |
| C15 | Failure mode | JMH measures source construction, dead-code elimination, or benchmark dispatch. | Build sources in `@Setup`, construct only the lazy result inside measured construction rows, consume checksums/Blackholes, and avoid reflective candidate lookup in timed methods. | High confidence in mitigation. |
| C16 | Unknown | The correct object engine is locally promising versus `sequence`, direct unary core context, and the fastest Java candidate. | Run the Phase 2 matrix and record a `promising` or `stop` handoff without hiding regressions. | Low until forked results exist. |
| C17 | Failure mode | Completion is forced early by probing the next lazy tail. | Trace a source whose tail `seq` has effects; require no tail probe until its continuation is forced. | High confidence in mitigation. |
| C18 | Failure mode | Fixing `xf-seq` silently pulls `consume`/`drain` semantics into the phase. | Keep fusion tests as Phase 0 history, document that ordinary `LazySeq` is not deconstructable, and make no Phase 6 repair. | High confidence in mitigation. |

Every phase-critical unknown has a resolution before production selection. If
the JMH harness cannot prove symmetric direct-on linkage, or any retained
candidate cannot be made semantically correct within its declared precondition,
selection stops and the plan returns to Draft rather than timing invalid rows.

## Decisive experiments

1. **Initializer order:** a traceable source and transducer must show no events
   at construction, then `:source-seq` before `:xform-apply` at first force.
2. **Accumulator:** a custom xform changes the accumulator object on every step
   and verifies the next step/completion receives it. Run over empty,
   dechunked, and chunked sources.
3. **Completion:** purpose-built xforms emit during completion. Require exactly
   one completion and step-output-before-completion-output for natural
   exhaustion, initially empty input, and `Reduced` within a chunk.
4. **Reduction:** terminate at each position around 1, 31, 32, and 33; require
   no later source event and no continuation that can restart processing.
5. **Input batches:** first force of a 64-item vector invokes map/predicate on 32
   inputs; first force of a dechunked source stops after the first emitted value
   while crossing rejected items only as needed.
6. **Mixed tail:** a custom source begins chunked and later becomes dechunked,
   and the reverse fixture begins dechunked then becomes chunked. The mixed loop
   must work; specialized candidate adapters must reject unsupported fixtures.
7. **Expansion:** emit 0, 1, 2, 4, 5, 31, 32, 33, 64, and at least 1,000 values
   from one input. Require order, maximum output chunk size 32, bounded stack,
   and correct continuation placement.
8. **Buffer ownership:** after each flush policy, verify the buffer no longer
   owns exposed arrays/values, small slots are cleared, and oversized capacity
   returns to the selected bound.
9. **Lazy-node concurrency:** use a barrier to force the same initial node and
   same later continuation from multiple threads; require one source/xform event
   sequence after successful forcing. Exception tests must reproduce
   `LazySeq`'s retry behavior rather than assuming a failed thunk is cached.
10. **Exceptions:** compare source `seq`/`first`/`next`, xform initialization,
    step, and completion exception type and event point with the selected oracle
    contract; rerunning a failed node must follow `LazySeq`'s actual behavior.
11. **Candidate equivalence:** run the complete deterministic contract suite
    against every reduced-aware candidate and the restricted subset against
    every no-reduced candidate before it becomes JMH-reachable.
12. **Linkage:** disassemble representative AOT wrappers and record
    `:direct-linking true`; reject results containing reflective or per-operation
    Var/candidate lookup in the timed path.
13. **Buffer policy:** compare current 1–4 `Cons`/capacity-8 policy with the
    simplest all-chunk alternative in dechunked one-output and expanding rows.
14. **Loop selection:** rerun every apparent >3% reversal in a fresh three-fork
    decision run with GC profiler. Select specialization only under the stated
    5% benefit / 3% regression rule.

## Ordered implementation slices

Run workers sequentially. The parent reviews, validates, commits, and records
each accepted SHA before starting the next slice. A worker is not alone in the
repository and must preserve prior/user edits rather than resetting them.

### Slice 1: canonical semantic engine

Ownership:

- `src-java/xfseq/ObjectXFSeqInit.java`
- repaired `src-java/xfseq/XFSeqStepSimple.java` as the provisional mixed
  reduced-aware step, plus at most one shared state/completion support class
  that is not a second engine
- `src-java/xfseq/buffer/ObjectBuffer.java`; keep `IXFSeqBuffer` unchanged unless
  a demonstrated blocker requires a reviewed minimal compatibility edit
- the `xfseq.core/xf-seq` execution boundary and removal of obsolete active
  generated-step initialization
- deterministic object-engine tests, excluding alternate-candidate adapters
- Slice 1 evidence and decision entries in this plan

Work:

1. Implement the initialization/state/buffer contracts above with one mixed
   reduced-aware step.
2. Return `LazySeq` directly and remove `XFSeqHead` from the public path.
3. Keep primitive and ASM research sources present but unreachable from generic
   object execution.
4. Add differential values, traces, accumulator, completion, reduced,
   expansion, exception, surface, concurrency, and deterministic buffer tests.
5. Keep public unary wrapper definitions and fusion code unchanged. The existing
   unary collection arities necessarily inherit the repaired `xf-seq`, but no
   Phase 3 semantic/adoption claim is made.

Parent check: inspect all mutable-state transitions, independently run the full
check and focused tests, verify no runtime class generation on the object path,
and confirm no Phase 3+ API or primitive implementation was added.

### Slice 2: repaired candidate set

Ownership:

- the remaining six retained object candidate classes listed above; preserve
  their class identities while repairing their internals or delegating to
  shared correct state/completion code, and re-audit Slice 1's
  `XFSeqStepSimple` through the common candidate suite
- candidate factories/adapters used only by tests and benchmarks
- candidate contract tests and stable v2 registry
- Slice 2 evidence and decision entries in this plan

Work:

1. Repair reduced-aware mixed, dechunked, and chunked shapes to the same state,
   completion, and buffer contract.
2. Repair no-reduced loops only under an explicit structural precondition; do
   not expose them through arbitrary `xf-seq`. They still retain every ordinary
   returned accumulator; only the proved-unnecessary `Reduced` check is absent.
3. Prefer shared completion/state code. If self-contained duplication is kept
   for measurement, label it provisional and keep both paths behavior-identical.
4. Add mixed-tail negative tests and reject invalid specialized adapter inputs
   before they cast.
5. Repair `XFSeqStep.ObjectStep` without changing the behavior of the preserved
   primitive `LongStep` and `DoubleStep`; override or delegate the object member
   if changing the shared base would cross that boundary.
6. Add v2 stable IDs without rewriting Phase 0 history.

Parent check: run the same contract suite across every candidate, inspect that
restricted candidates are unreachable for reducing xforms, and verify the
historical source/tag and primitive/ASM files remain available.

### Slice 3: symmetric JMH harness

Ownership:

- JMH-only Java/Clojure benchmark source directories
- `deps.edn` benchmark dependencies and the minimal `build.clj` JMH/AOT tasks
- benchmark parameter registry, environment capture, and result validation
- benchmark documentation and Slice 3 plan entries

Work:

1. Pin JMH 1.37 and build a standalone benchmark jar without changing the
   normal library classpath or `check` semantics.
2. AOT-compile core/candidate caller wrappers with direct linking on and verify
   their bytecode/linking identity.
3. Add public end-to-end and Java-level loop/buffer groups with static candidate
   selection outside timed methods.
4. Keep collections in `@Setup`; use checksums or `Blackhole`; separate
   construction, first, prefix, traversal, vector materialization, and reduce.
5. Emit JMH JSON and environment metadata; make a tiny smoke profile separate
   from the decision profile.

Parent check: inspect generated benchmark identities, run the smoke after the
semantic suite, verify the reported operation really differs by candidate, and
reject hidden Var/reflection/lookup costs or result overwrites.

### Slice 4: decision runs and production consolidation

Ownership:

- raw Phase 2 benchmark and JIT/allocation evidence under `results/phase-2/`
- only production edits required by the measured loop/buffer selection
- README benchmark command and final run-stage entries in this plan

Work:

1. Run screening and exact decision matrices only after all correctness gates.
2. Investigate allocation and JIT/inlining for winners and every repeatable
   material regression.
3. Select the simplest qualifying production loop/buffer policy. Delete
   provisional duplicate product paths that did not earn their complexity;
   retain benchmark references needed for the fastest-correct baseline.
4. Record cell-level results, rejected alternatives, and a `promising` or
   `stop` recommendation for Phase 3.
5. Rerun the full local check, focused semantic suite, decision subset, result
   validator, and diff hygiene. Mark the plan `Awaiting final review`, never
   `Complete`.

Parent check: audit every selection rule against raw JSON, independently rerun
representative winning/regressing cells with GC profiling, inspect the final
product path for dead branches/duplicate engines, and audit every exit criterion.

## Semantic validation

### Differential value matrix

Use Clojure 1.12.5. Compare fully realized outputs with `sequence` and, for
complete eager output, `transduce` into a vector.

Sources:

- `nil` and empty collections;
- persistent list and explicitly dechunked lazy seq;
- vector, subvector, and `range`;
- hash/sorted set and hash/sorted map entries;
- object array;
- Java `Iterable` and `Iterator` adapter;
- `repeat`/`iterate` with an explicit terminating transform;
- custom `Seqable`;
- deterministic chunked, dechunked, and mixed-tail sources.

Construct a fresh instance for each oracle/candidate observation when a source
is consumable or stateful, especially Java iterators and custom lazy sources.

Sizes:

```text
0, 1, 2, 3, 4, 5, 7, 8, 9, 31, 32, 33, 63, 64, 65, 1,000
```

Transforms:

- one-to-one `map`;
- zero-or-one `filter` and `keep` at edge selectivities;
- early `take` and a purpose-built reducer;
- stateful `distinct`, `dedupe`, and `partition-by`;
- expanding `mapcat`, `cat`, and `interpose`;
- completion-emitting `partition-all` and a purpose-built completion xform;
- composed map/filter/take/stateful/expanding pipelines;
- custom accumulator-changing and throwing xforms.

### Trace and surface matrix

At construction, `seq`, `first`, `next`, one chunk, partial prefix, full
consumption, and repeated consumption, record:

```text
[:source-seq]
[:source-first n]
[:source-next n]
[:xform-apply]
[:step n accumulator-id]
[:rf-output value]
[:complete accumulator-id]
```

Cover `seq?`, `sequential?`, `first`, `next`, `rest`, `nth`, `count`, `vec`,
`into []`, `reduce`, sequential equality, hash equality, printing, Java
iteration, metadata behavior inherited from `LazySeq`, repeated realization,
same-node concurrency, and abandoned tails.

### Failure and retention rules

- Compare exception class and event point, not repository-specific message text
  unless Clojure itself specifies it.
- Never turn `OutOfMemoryError`, `StackOverflowError`, or a source over-read into
  an accepted expected difference.
- Weak-reference tests run repeated bounded GC attempts and report inconclusive
  separately; deterministic array ownership/slot clearing tests remain the
  hard gate.
- Property-based generation may supplement the deterministic matrix after the
  fixed oracle cases pass; it may not replace them.

## Performance and direct-linking methodology

### Exact Phase 2 runtime matrix

| Lane | Clojure | Java | Linking | Purpose |
|---|---|---|---|---|
| `phase2-decision-on` | library 1.12.5; CLI 1.12.5.1664 | Homebrew OpenJDK 26.0.2.1, arm64 | released core jar on; AOT caller and candidate namespaces on; Java candidates direct | Primary local production-loop decision |

Record macOS/Darwin version, CPU identity when available, processors, heap,
GC, JVM flags, commit, dirty state, JMH version, classpath/jar hashes, and exact
commands with every result group.

There is no off-linked lane in Phase 2. No off result is required to choose
among direct Java loops, and an off core comparison would require a symmetric
off-built core jar. If the candidate is promising, later release/upstream work
must add separate same-revision on/off Clojure builds and broader JDK coverage.

### JMH execution profiles

- **Smoke:** one fork, two short warmups, two short measurements, one tiny
  parameter subset; validates identity and output only.
- **Screen:** two forks, at least three warmup and three measurement iterations;
  finds plausible reversals without making a selection.
- **Decision:** three fresh forks, five 1-second warmup iterations, five
  1-second measurement iterations, fixed `-Xms2g -Xmx2g`, G1 GC, JSON output,
  and a separate `-prof gc` run for allocation.

Use JMH defaults only when the raw metadata records them. Do not combine a GC
profiled score with an unprofiled throughput score as if they were one run.

### Implementations

Applicable groups compare:

1. `core-sequence` with the same xform;
2. direct unary core function where the operation is truly equivalent, labeled
   Phase 3 context rather than Phase 2 adoption evidence;
3. `eduction` plus the same sink;
4. `transduce` for fully eager reduction context;
5. the canonical public object engine;
6. every repaired reduced-aware hand-written Java loop valid for the source;
7. restricted no-reduced loops only for structurally non-reducing operations;
8. buffer policy alternatives;
9. preserved Phase 0 numbers only in a separate historical appendix, never in
   a v2 ranking.

### Phase 2 benchmark matrix

Sources:

- persistent list/dechunked lazy seq;
- vector and subvector;
- range;
- hash set and hash-map entries;
- object array;
- Java iterable;
- custom mixed-tail source for semantic smoke only.

Sizes:

```text
0, 1, 4, 8, 31, 32, 33, 64, 1,000, 10,000, 1,000,000
```

Workloads:

- identity and small arithmetic map;
- filter near 0%, 1%, 50%, 99%, and 100%;
- map then filter and a five-stage map pipeline;
- early `take` from large finite input;
- expansion producing 0, 1, 2, 32, and more than 32 outputs per input batch;
- one stateful transform and completion output;
- a heavier function-call control that makes framework overhead secondary.

Sinks:

- construct only;
- `first`;
- consume prefix 8;
- full checksum traversal;
- `into []`;
- reducing checksum.

The harness may generate the Cartesian matrix, but the decision run uses a
checked-in manifest of applicable cells. Chunk-only/dechunked-only candidates
must never appear in invalid cells. Empty/construct-only and million-element
rows use only workloads for which they answer a real question.

### Interpretation and selection

- Keep raw scores, error/intervals, fork samples, and bytes/op. Do not publish
  only ratios.
- Do not average different sources, sizes, workloads, or sinks.
- Investigate every repeatable regression over 3%.
- Product specialization needs at least a repeatable 5% throughput or allocation
  benefit in its targeted primary rows, support from fork samples and reported
  uncertainty, no repeatable regression over 3% in supported rows, and a
  structural dispatch proof.
- If throughput and allocation disagree, inspect GC/JIT evidence and prefer no
  added complexity until a user-relevant outcome is clear.
- The fastest correct candidate in each applicable cell remains the internal
  baseline. A later refactor must not compare only with the selected universal
  loop when a specialized repaired loop is faster.
- Phase 2 records whether performance is promising; broad core-function adoption
  still requires Phase 3 function-specific direct-core evidence.

## Evidence layout

Expected durable artifacts:

```text
results/phase-2/environment.edn
results/phase-2/bench/smoke-<commit>.json
results/phase-2/bench/screen-<commit>.json
results/phase-2/bench/decision-<commit>.json
results/phase-2/bench/decision-gc-<commit>.json
results/phase-2/jit/<representative-case>.log
```

The plan records commands, exit codes, test summaries, result hashes, selected
rows, rejected rows, candidate IDs, slice SHAs, and agent runs. Result writers
must refuse to overwrite an existing raw path. Machine-local temporary files
remain under `/private/tmp` and are not cited as durable completion evidence.

## Exit criteria

Phase 2 may move to final review only when all are true:

1. `xfseq.core/xf-seq` returns `LazySeq` directly and construction produces no
   source, transducer, step, or completion event.
2. The source is seq'd before the transducer is applied at first force; the
   transducer is applied once and completion occurs once for empty, exhausted,
   and reduced paths.
3. Every returned accumulator is preserved, `Reduced` is detected explicitly,
   no input is consumed after reduction, and completion output follows step
   output.
4. The complete deterministic value suite passes against its stated Clojure
   1.12.5 oracles across all required sources, sizes, and transform shapes.
5. Chunked input processes one input chunk per step unless reduced; dechunked
   input stops after producing output; mixed-tail input works without unsafe
   source classification.
6. Standard sequence surface, cached realization, exception, concurrency, and
   repeated-realization tests pass for empty and non-empty results.
7. Output batches preserve order, never expose a chunk above 32, do not grow the
   Java stack with expansion, clear transferred references, and apply the
   selected bounded-capacity policy.
8. Primitive analysis and runtime-generated step selection are absent from the
   generic object execution path. No new generated, reflective, primitive, or
   fusion mechanism appears.
9. Every retained object candidate has a new unambiguous v2 ID, passes its full
   declared contract, and maps back to (without overwriting) its Phase 0 ID.
10. No-reduced and source-specialized candidates are unreachable outside their
    proved preconditions.
11. JMH 1.37 smoke, screen, direct-on decision, and allocation runs complete on
    the exact local runtime with validated linkage and raw non-overwritten JSON.
12. The selected product path follows the simplicity/performance decision rule;
    every apparent >3% reversal is resolved or named as a blocker, and the
    fastest correct cell-level Java baseline remains reproducible.
13. The plan records a truthful `promising` or `stop` recommendation. It makes
    no Phase 3, cross-JDK, direct-linking-off, publication, or upstream claim.
14. `clojure -Srepro -T:build check`, focused semantic tests, benchmark result
    validation, compiler reflection checks, and `git diff --check` all pass.
15. Commands, versions, raw paths/hashes, decisions, slice SHAs, and sequential
    agent runs are recorded here; the run ends at `Awaiting final review`.

If semantic fidelity requires a second public engine, source whitelist, caller-
trusted metadata, generated class, or function-specific special case, mark the
plan `Needs replanning`. If performance does not justify specialization, choose
the single mixed loop. If the best correct object engine is broadly unpromising,
finish the phase evidence honestly with a `stop` recommendation rather than
adding machinery.

## Decision log

| Date | Decision | Reason |
|---|---|---|
| 2026-08-31 | Define Phase 2 as a correct generic object engine plus internal loop selection, not unary core migration. | The parent design assigns function-specific semantics to Phase 3. |
| 2026-08-31 | Use `sequence` for fully realized generic values/completion, but not for construction or input-batch timing. | Direct 1.12.5 evidence shows `sequence` eagerly touches the source/xform and iterator-chunks output, contradicting the explicit parent contract. |
| 2026-08-31 | Use one mixed reduced-aware Java step as the provisional and default product boundary. | It is the simplest safe engine for arbitrary xforms and mixed seq tails. |
| 2026-08-31 | Track ordinary returned accumulators even though `TransformerIterator` does not. | The reduction/transducer contract and parent design require it; copying an iterator limitation would make the driver unsound. |
| 2026-08-31 | Do not use caller metadata to select no-reduced code. | Arbitrary metadata is not a structural proof and can silently break early termination. |
| 2026-08-31 | Give repaired candidates v2 IDs instead of reusing Phase 0 names. | Phase 0 IDs explicitly describe semantically non-equivalent identity-stop code. |
| 2026-08-31 | Add JMH 1.37 in Phase 2 and run only the symmetric direct-linking-on local lane. | Correct loop selection needs allocation-aware forked data; an off lane and other JDKs are premature. |
| 2026-08-31 | Specialization must clear a 5% benefit / 3% regression rule and have a whole-source proof. | Small noisy wins do not justify core-level branches or duplicated loops. |
| 2026-08-31 | Leave `consume`, `drain`, primitive sources, and ASM outside the repair. | They are separate phase/design questions and cannot justify an incomplete ordinary seq. |
| 2026-08-31 | Slice 1 routes generic `xf-seq` through `LazySeq(ObjectXFSeqInit)` and the repaired `XFSeqStepSimple`; generated step setup is removed from `xfseq.core`. | One deferred initializer and one mixed reduced-aware loop satisfy the provisional semantic boundary without a second engine. |
| 2026-08-31 | Slice 1 keeps `ObjectBuffer` object-backed while inheriting primitive IFn entry points invisibly to the analyzer. | Existing unary wrapper definitions still need primitive reducing-function casts; object-only filters must remain safe for arbitrary values. |
| 2026-08-31 | Slice 1 records correctness and build evidence only; no performance selection is made. | JMH and allocation evidence are owned by Slices 3–4 after all candidate contracts are repaired. |

## Planning validation evidence

| Check | Result |
|---|---|
| Current clean build | `clojure -Srepro -T:build check` exit 0; lint 0/0, compiler reflection passed, tests 1/46/0/0. |
| Runtime | Clojure 1.12.5; CLI 1.12.5.1664; Homebrew OpenJDK 26.0.2.1; macOS 26.2 / Darwin 25.2.0 arm64. |
| Current commit | `8e13124de85237556316db78ba8841f52b46d8f3`. |
| Current object sources | Seven retained object loop shapes plus `ObjectBuffer`; all inspected directly. |
| Clojure source | Local 1.12.5 jar `core.clj` inspected for `sequence`, direct `map`/`filter`, `take`, and `transduce`. |
| Clojure bytecode | `TransformerIterator`, `LazySeq`, `ArrayChunk`, and `ChunkedCons` inspected with `javap`. |
| Construction probe | `sequence` recorded source `seq`, xform application, and first step before the returned value was observed. |
| Chunk probe | On a 64-vector, direct unary map/filter called 32 functions after `first`; `sequence` map called 33 and 50%-filter called 64. |
| Historical semantic report | Phase 0 1.12.5 EDN records `XFSeqHead`, two xform applications, missing empty completion, and the green but narrow 46-assertion suite. |

These are planning facts, not Phase 2 completion evidence or performance data.

## Slice 1 implementation evidence

Slice 1 uses one canonical object path:

```text
xfseq.core/xf-seq
  -> LazySeq(ObjectXFSeqInit)
  -> ObjectBuffer + transformed reducing function
  -> XFSeqStepSimple (mixed, explicit Reduced-aware state)
```

`ObjectXFSeqInit` keeps source sequencing, buffer creation, transducer
application, and empty-input completion inside the initial lazy thunk. The
step stores every ordinary returned accumulator, unwraps `Reduced` once, and
completes only after natural exhaustion or terminal reduction. `ObjectBuffer`
now routes terminal output through the bounded chunking path, checks growth
before multiplication, clears transferred slots, and resets exposed/oversized
working arrays. Its inherited primitive IFn entry points remain available for
the existing type-aware unary wrappers while `getInterfaces()` still reports
only the object buffer contract to the analyzer; storage and the public engine
remain object-only.

The old generated-step setup, `XFSeqHead`, and primitive selection were removed
from the active `xfseq.core` path. The preserved hand-written primitive,
alternate object, and historical generated sources remain present and are not
reachable from generic `xf-seq`. Public unary wrapper definitions and fusion
code were not changed.

Deterministic Slice 1 tests are in
[`test/xfseq/object_engine_test.clj`](../../test/xfseq/object_engine_test.clj).
They cover construction/initialization order, empty and terminal completion,
returned accumulators, explicit reduction, chunked/dechunked/mixed-tail input,
expansion and output chunk bounds, buffer ownership/slot clearing, the
ordinary `LazySeq` surface, exception retry behavior, and concurrent forcing
of one node. Alternate-candidate adapters and benchmark harnesses are outside
this slice.

### Slice 1 validation evidence

| Check | Result |
|---|---|
| Recorded pre-change baseline | `clojure -Srepro -T:build check` exit 0; lint 0/0; reflection clean; 1 test / 46 assertions / 0 failures / 0 errors. |
| Focused object-engine suite | `clojure -Srepro -M:test` exit 0; 14 tests / 180 assertions / 0 failures / 0 errors. |
| Full local check after Slice 1 | `clojure -Srepro -T:build check` exit 0; lint 0/0; reflection clean; 14 tests / 180 assertions / 0 failures / 0 errors. |
| Parent differential probe | 2,016 source/size/xform cells across vector, list, and range sources; boundary sizes through 1,000; map, filter, keep, take, mapcat, partition-all, and composed transforms all matched Clojure 1.12.5 `sequence`. |
| Diff hygiene | `git diff --check` passed. |
| Active generated path | Fresh `target/classes` contains `ObjectXFSeqInit.class` and repaired Java candidates; no `xfseq.core` generated step initialization runs (the prior `classes:` load output is absent). |
| Performance readiness | Java compiles with the existing `--release 8` build; no throughput or allocation claim is made. JMH belongs to Slices 3–4. |

### Slice 1 decisions

| Date | Decision | Reason |
|---|---|---|
| 2026-08-31 | Use `LazySeq(ObjectXFSeqInit)` directly and make `XFSeqStepSimple` the sole provisional mixed object loop. | It gives the standard cached sequence surface with one deferred setup and no second canonical engine. |
| 2026-08-31 | Preserve the actual returned accumulator and use `RT.isReduced`/one `Reduced.deref()` in the mixed step. | Identity comparison silently loses ordinary accumulator changes and can miss valid terminal results. |
| 2026-08-31 | Have `ObjectBuffer.toTail()` use the same bounded `toSeq` path as continuations. | Terminal expansion must preserve order without exposing a chunk above 32 or retaining the mutable working array. |
| 2026-08-31 | Add inherited primitive IFn bridge methods without exposing primitive interfaces directly from `ObjectBuffer`. | Existing unary wrapper definitions still produce primitive-aware reducing-function casts; direct interface exposure would make ordinary object filters cast arbitrary values. |
| 2026-08-31 | Remove generated `xfseq.core` step setup and `XFSeqHead` from the active source. | Generic object execution must not load or select runtime-generated/primitive steps; historical research remains in its preserved sources. |
| 2026-09-01 | Persist per-item/chunk progress while a retained step node is retried. | A step/source exception after partial buffering must resume at the failed input without duplicating values already buffered in that node. |

## Slice 2 implementation evidence

Slice 2 keeps all seven retained object class identities and gives the six
remaining candidates one shared `XFSeqObjectStep` state/completion
implementation. The canonical mixed `XFSeqStepSimple` is re-audited through
the same contract suite. Reduced-aware mixed,
dechunked, and chunked candidates now preserve ordinary accumulator returns,
unwrap `Reduced` once, defer source-tail probing until a continuation is forced,
complete once, and resume a partially processed chunk at the failed input on a
`LazySeq` retry. No-reduced candidates use the same state machine with the
step-level `Reduced` check disabled; they are not reachable from `xf-seq`.
`XFSeqStep.ObjectStep` delegates to the shared object implementation while the
preserved `LongStep` and `DoubleStep` code is unchanged. The common completion
retry test also found and repaired one narrow Slice 1 defect in
`XFSeqStepSimple`: after a reduced step whose completion throws, retry now
retries completion without reprocessing the terminal input.

Test/benchmark-only adapters in
[`test/xfseq/phase_2_candidates.clj`](../../test/xfseq/phase_2_candidates.clj)
validate finite specialized source shapes before construction and require an
adapter-owned `NonReducingOperation` proof for no-reduced candidates. The same
namespace records seven repaired v2 IDs and an explicit old-to-new mapping;
Phase 0 IDs remain historical. Candidate contract tests are in
[`test/xfseq/object_candidate_test.clj`](../../test/xfseq/object_candidate_test.clj)
and re-audit `XFSeqStepSimple` with every retained object candidate. The
reduced-aware matrix uses fresh dechunked, chunked, dechunked-to-chunked, and
chunked-to-dechunked fixtures at all 16 required sizes (`0,1,2,3,4,5,7,8,9,
31,32,33,63,64,65,1000`) and eight representative transducer families. It
covers 1,280 applicable candidate/shape/size/transform cells and compares
each candidate with both `sequence` and `transduce` (2,560 oracle assertions).
Mixed candidates exercise both tail directions explicitly. No-reduced
candidates use an adapter-owned expanding operation over 1,000 inputs and
assert output order, 2,000 outputs, and the 32-element chunk bound. Every
candidate is directly reinvoked after terminal realization to verify that
completion and source steps are not repeated. A minimal external-package Java
probe also compiled a call to inherited `invoke()` while `XFSeqObjectStep`
remained package-private, so no public support-class expansion was needed.

### Slice 2 validation evidence

| Check | Result |
|---|---|
| Candidate-focused suite | `clojure -Srepro -M:dev -e "(require 'xfseq.object-candidate-test) (println (select-keys (clojure.test/run-tests 'xfseq.object-candidate-test) [:test :pass :fail :error])) (shutdown-agents)"` exit 0; 15 tests / 2,726 assertions / 0 failures / 0 errors. |
| Differential evidence | 1,280 fresh candidate/shape/size/transform cells; 2,560 `sequence`/`transduce` oracle assertions, including all 16 required sizes and both mixed-tail directions. |
| Full local check | `clojure -Srepro -T:build check` exit 0; lint 0/0, compiler reflection clean, 29 tests / 2,906 assertions / 0 failures / 0 errors. |
| Diff hygiene | `git diff --check` passed. |
| Java compatibility | `clojure -Srepro -T:build javac` exit 0 with existing `--release 8` target. |
| External Java accessibility | Temporary external-package `Phase2CandidateProbe` calling `XFSeqStepSimpleNoReduced.invoke()` compiled with `javac -cp target/classes:<clojure-1.12.5.jar>` exit 0; probe source removed and production support class remains package-private. |
| Scope | No public dispatch, JMH harness, primitive/ASM, fusion, or Phase 3+ changes; `LongStep`/`DoubleStep` sources remain behaviorally untouched. |
| Performance evidence | No throughput/allocation claim; JMH is owned by Slices 3–4. |

### Slice 2 decisions

| Date | Decision | Reason |
|---|---|---|
| 2026-09-01 | Share state, completion, and retry handling through `XFSeqObjectStep` across the six new candidates and `ObjectStep`. | The contract is identical; one implementation keeps accumulator, `Reduced`, completion, and buffer ownership behavior auditable before JMH. |
| 2026-09-01 | Defer `more`/`chunkedMore` until an output continuation is forced. | Specialized candidates must not over-read lazy tails merely to expose a prefix; pending state also preserves retry boundaries. |
| 2026-09-01 | Validate specialized finite source shape in a test/benchmark adapter and fail before xform construction or casts. | `IChunkedSeq` describes a node, not an arbitrary tail; production `xf-seq` remains mixed and source-lazy. |
| 2026-09-01 | Require adapter-owned non-reducing operations for no-reduced candidates. | Caller metadata or an arbitrary transducer cannot prove that `Reduced` is impossible. |
| 2026-09-01 | Keep `XFSeqStep.ObjectStep` as a subclass with a delegate and leave primitive siblings unchanged. | Preserves the historical class identity and primitive boundary while avoiding a second object state machine. |
| 2026-09-01 | Set the canonical mixed step's source to terminal state before completion after `Reduced`. | A completion exception is retryable under `LazySeq`; retrying the terminal step duplicated buffered output and violated the shared candidate contract. |
| 2026-09-01 | Use fresh source fixtures for each `sequence`, `transduce`, and candidate observation in the 1,280-cell differential matrix. | Consumable/lazy sources can otherwise make an apparently differential check compare different source states; both mixed tail directions are now exercised. |
| 2026-09-01 | Make expansion and completion-counting operations adapter-owned no-reduced proofs. | The no-reduced candidates must demonstrate structural non-reduction, expanded order/chunk bounds, and terminal idempotence without accepting arbitrary caller metadata. |
| 2026-09-01 | Keep `XFSeqObjectStep` package-private after an external Java compile probe. | Public candidate subclasses can be called directly from another package through inherited `invoke()`; widening the shared support class would add API surface without evidence. |

## Slice 3 implementation evidence

Slice 3 adds an isolated `bench/java` and `bench/clj` tree plus the `:bench`
alias's pinned JMH 1.37 dependencies. `bench-aot` first compiles the normal
Java sources into `target/classes`, copies those classes into an isolated
benchmark class directory, AOT-compiles `xfseq.core`, the benchmark-only
candidate adapters, and `xfseq.bench.calls` with direct linking enabled, then
runs the JMH annotation processor over the Java benchmark sources. `bench-jar`
packages those classes and dependencies into the standalone
`target/bench/xfseq-phase2-jmh.jar` without adding benchmark paths to the
normal library or test aliases.

The Java groups are `Phase2PublicBenchmark` (public end-to-end construction,
first, prefix, traversal, vector, and reduce sinks), `Phase2JavaBenchmark`
(direct repaired-candidate loop rows), and `Phase2BufferBenchmark` (isolated
object-buffer append/flush). Fixtures are created in JMH setup. Public plans
and candidate plans are selected once in setup; each candidate plan checks its
expected concrete class before timing, while timed methods contain no
candidate map lookup, reflection, or Clojure Var forwarding. The AOT caller
function classes are called through `invokeStatic`, whose bytecode directly
links to `xfseq.core$xf_seq` and
`xfseq.phase_2_candidates$instantiate_candidate`; the linkage gate rejects
Var references in those callers.

The checked-in registry records the complete Phase 2 source, size, workload,
sink, public-implementation, and repaired-candidate vocabularies, including
source-shape and no-reduced applicability. The runner validates required JMH
JSON metrics, benchmark identities, and distinct candidate IDs. Durable JSON
and EDN metadata use existence checks plus `CREATE_NEW`; a second run at the
same artifact path is rejected rather than overwriting evidence.

### Slice 3 validation evidence

| Check | Result |
|---|---|
| Normal semantic gate before timing | `clojure -Srepro -T:build check` exit 0; lint 0/0, compiler reflection clean, 29 tests / 2,906 assertions / 0 failures / 0 errors (also run by `bench-smoke` before AOT). |
| Isolated AOT/JMH build | `clojure -Srepro -T:build bench-jar` completed; JMH 1.37 `META-INF/BenchmarkList` and `META-INF/CompilerHints` present in the jar. |
| Linkage gate | `clojure -Srepro -T:build bench-linkage` completed; representative `javap -c` output records direct calls to `xfseq/core$xf_seq.invokeStatic` and `xfseq/phase_2_candidates$instantiate_candidate.invokeStatic`; no `Var` reference in caller function classes. Linkage output hash: `9cef4511d6bc2367c600379acc151d1e7b384e6f2e022d4949c5d6d88b8c7d30` (machine-local `target/bench/linkage-9a271d791d8971369c1b1a94b49e185a32162118.txt`). |
| Tiny smoke child groups | Three one-fork JMH 1.37 groups completed with two 100-ms warmups and two 100-ms measurements: public (`xfseq`, `sequence`, list/8/identity), Java (`java-mixed-object-reduced-aware-v2`, `java-dechunked-object-reduced-aware-v2`, list/8/identity), and buffer (8 values). Temporary outputs were under `/private/tmp/xfseq-phase2-smoke-15883971341235910704/`. |
| Smoke result validation | `clojure -Srepro -M:bench -m xfseq.bench.runner validate-smoke results/phase-2/bench/smoke-9a271d791d8971369c1b1a94b49e185a32162118.json` exit 0; 19 rows, 10 benchmark identities, both candidate IDs present. Result SHA-256: `11971f05500e8ea2d86058346277eea3b07eb2794a4fbd68e65fc41ffa334397`. |
| Original strict validation boundary | `clojure -Srepro -M:bench -m xfseq.bench.runner validate results/phase-2/bench/smoke-9a271d791d8971369c1b1a94b49e185a32162118.json` exits 1 with `JMH result row is missing required metrics` because all 19 short-run errors are `"NaN"`; this expected rejection proves the original smoke receipt is not accepted as screen/decision evidence. |
| Environment metadata | `clojure -Srepro -M:bench -m xfseq.bench.runner environment ...` exit 0; `results/phase-2/environment.edn` records direct linking, JMH 1.37, Clojure 1.12.5, Java 26.0.2.1, macOS 26.2 / arm64, GC/heap, dirty commit, exact command vectors, jar hash, and result hash. Environment SHA-256: `02cf64cb0b3b1a237ee5c87becf71bff7de17a8cb29266f3e5b44d4ffc7a64f9`. |
| Parent-discovered initial smoke defect | The parent ran `clojure -Srepro -T:build bench-smoke` and all three JMH child groups completed, but the subsequent strict `merge` exited 1 on JMH's two-sample `scoreError: "NaN"`. The original child outputs and durable `smoke-9a271d791d8971369c1b1a94b49e185a32162118.json` receipt were preserved. |
| Profile-aware merge regression test | `clojure -Srepro -M:bench -e "(require 'xfseq.bench.registry 'xfseq.bench.runner 'xfseq.bench.registry-test) (let [r (clojure.test/run-tests 'xfseq.bench.registry-test)] (prn (select-keys r [:test :pass :fail :error])) (when (pos? (+ (:fail r) (:error r))) (System/exit 1)))"` exit 0; 1 test / 5 assertions / 0 failures / 0 errors. Smoke validation and `merge-smoke` accept `"NaN"`; strict `validate` and `merge` reject it before writing a decision artifact. |
| Intermediate non-overwriting receipt | The first post-fix run `bench-smoke '{:run-id "followup-20260901"}'` was completed and remains preserved at `results/phase-2/bench/smoke-9a271d791d8971369c1b1a94b49e185a32162118-followup-20260901.json` (SHA-256 `361ad067c159a92ee1a7c46fff6cb8d328afb9ddb69dbdc8ee33a7c6c98b0d56`) and `results/phase-2/environment-9a271d791d8971369c1b1a94b49e185a32162118-followup-20260901.edn` (SHA-256 `2fbdcf52fc789b06d51f7f06199b33636a640c2085fd4e548b879b1ccd129210`). |
| Fresh source-matched one-command smoke receipt | `clojure -Srepro -T:build bench-smoke '{:run-id "followup-20260901b"}'` exit 0 after semantic gates, isolated AOT/linkage, and all three child groups. Temporary child JSON was under `/private/tmp/xfseq-phase2-smoke-1927737577364742442/`; durable result: `results/phase-2/bench/smoke-9a271d791d8971369c1b1a94b49e185a32162118-followup-20260901b.json`, SHA-256 `2cba72ee95debe89184f7ffbd7e5f27ee85b4bc0f4e9e401d23a9abb9c893888`, 19 rows / 10 benchmark identities / both candidate IDs. |
| Fresh environment/source receipt | `results/phase-2/environment-9a271d791d8971369c1b1a94b49e185a32162118-followup-20260901b.edn`, SHA-256 `d5765318618cadbd3ad3d691ecb3b7b3169ad3dea29597396690272fc2253c69`; records run ID, exact child argv, result SHA-256, JMH jar SHA-256 `8ca36cc2a9910ee673858859d59afd204ec66b371850e6f8dae2d8bfb39946d3`, tracked dirty-diff SHA-256 `1ba7c5dc7ab9954eca1e67fe070b6d90f9d977195902e74a80f0dbe27b01c753`, and benchmark source manifest SHA-256 `f1764ae6aa8a593db721cb391a62f6454437ad6b5d7895de6651e68c2f895eab` with eight per-file hashes. `bench-validate '{:run-id "followup-20260901b"}'` revalidated the receipt without rewriting it. |
| Parent lifecycle receipt | The parent reran `clojure -Srepro -T:build bench-smoke '{:run-id "parent-20260901"}'`; semantic gates, linkage, all three child JMH groups, `merge-smoke`, `validate-smoke`, and environment writing completed. The result `results/phase-2/bench/smoke-9a271d791d8971369c1b1a94b49e185a32162118-parent-20260901.json` is preserved with SHA-256 `55f0150a76b5cfe89803f4ce19ac2656e4fd43ea0e1728e355218091b5dcd6e7`; its environment `results/phase-2/environment-9a271d791d8971369c1b1a94b49e185a32162118-parent-20260901.edn` is preserved with SHA-256 `b04dd8912b8cafe0bf25abaecbb5d0c19eb2ee37b2bfdee693bffd47f816bb1b`. The parent observed the runner subprocess remained alive after output files existed and interrupted the wait; no raw file was overwritten or removed. |
| Runner lifecycle probes | On fresh temporary paths under `/private/tmp/xfseq-runner-lifecycle-tdsRe1/`, `/usr/bin/time -p clojure -Srepro -M:bench -m xfseq.bench.runner validate-smoke /private/tmp/xfseq-runner-lifecycle-tdsRe1/validate.json` exited 0 in `real 0.58` seconds; `/usr/bin/time -p clojure -Srepro -M:bench -m xfseq.bench.runner environment /private/tmp/xfseq-runner-lifecycle-tdsRe1/environment.edn smoke runner-lifecycle-20260901 results/phase-2/bench/smoke-9a271d791d8971369c1b1a94b49e185a32162118-parent-20260901.json target/bench/xfseq-phase2-jmh.jar '[]'` exited 0 in `real 0.68` seconds; temporary environment SHA-256 was `437b79cb05852c66b1181959869b0810b00bcd8d51679b139882103899486b07`. The strict `/usr/bin/time -p clojure -Srepro -M:bench -m xfseq.bench.runner validate /private/tmp/xfseq-runner-lifecycle-tdsRe1/validate.json` probe exited 1 in `real 0.64` seconds on expected smoke `"NaN"` rows, and `pgrep -fl 'xfseq.bench.runner'` found no lingering process. |
| Final lifecycle-fixed smoke receipt | `clojure -Srepro -T:build bench-smoke '{:run-id "lifecycle-20260901"}'` exited 0 and the outer build returned after semantic gates, isolated linkage, all three child groups, profile-aware merge/validation, and environment capture. Temporary child JSON was under `/private/tmp/xfseq-phase2-smoke-3476777049237605159/`; result `results/phase-2/bench/smoke-9a271d791d8971369c1b1a94b49e185a32162118-lifecycle-20260901.json` SHA-256 `6cf48e142c92d2adb33b613b1963b6368972a8b5077c00996a6f2c2bfa013aa9`; environment `results/phase-2/environment-9a271d791d8971369c1b1a94b49e185a32162118-lifecycle-20260901.edn` SHA-256 `28e9ebfe9a64d8d9b63bb32135a9d6b6d243f8fa0f2fbe0e6a0538584b362ba9`. The environment records JMH jar SHA-256 `f52ce02748a3832c6674516da70e80e16369767e5c903f6d21a6c5bdbe63e300`, tracked dirty-diff SHA-256 `d962e3fb69319afc17ce1de944d4d50f0a38b3d94bbc1fe605f8dd7c4352ebac`, and benchmark source manifest SHA-256 `3f938917cc99c86e363200ddbfed185b406fbe9ed3bf9002516fa303f34d1d7b` with eight per-file hashes. |
| Result-writer safety | `bench-validate` exit 0; a subsequent `bench-smoke` attempt refused to overwrite the existing result path, as required. |
| Benchmark-only lint | `clojure -Srepro -M:lint -m clj-kondo.main --lint bench` exit 0; 0 errors / 0 warnings. |
| Diff hygiene | `git diff --check` exit 0. |

The parent-discovered first smoke merge correctly exposed that JMH serializes
the two-sample error as the string `"NaN"`, but it also exposed a workflow
defect: the generic merge path was strict and made the otherwise successful
smoke command exit 1. The fix keeps `merge`/`validate` strict for screen and
decision evidence and routes only the short profile through explicit
`merge-smoke`/`validate-smoke` handling. Child rows are validated before a
durable merge is written, so a strict decision merge cannot leave an invalid
artifact. The fresh run above proves the one-command smoke now exits 0; no
score was used for a production decision.

The parent then found a separate lifecycle defect: after a successful smoke,
the runner had emitted all result and environment files but its
`clojure.java.shell/sh` agent pools kept the short-lived subprocess alive. The
runner now calls `shutdown-agents` from a `finally` block around every CLI
command, preserving command failures while releasing those pools. Fresh
temporary `validate-smoke` and `environment` probes exit in under a second,
and the final suffixed smoke above proves the outer `bench-smoke` task also
returns 0 without an orphaned runner.

### Slice 3 decisions

| Date | Decision | Reason |
|---|---|---|
| 2026-09-01 | Keep all benchmark source and AOT output under isolated `bench/*` and `target/bench/*` paths. | Normal `check` must retain its existing classpath and test semantics; benchmark classes are not production classes. |
| 2026-09-01 | Use regular AOT Clojure function classes and call `invokeStatic` directly from Java. | `gen-class` forwarding methods perform Var lookup on every call; direct generated function classes preserve direct linking in the timed caller boundary. |
| 2026-09-01 | Bind the AOT loader's dynamic `*warn-on-reflection*` and `*ns*` vars once in benchmark support. | Direct Java startup bypasses Clojure's normal namespace loader; one-time binding permits production namespaces to initialize without adding lookup work to timed methods. |
| 2026-09-01 | Split the smoke into three JMH invocations and merge their JSON arrays. | Public, candidate, and buffer groups have different parameter vocabularies; separate invocations keep the smoke tiny and avoid invalid source-shape Cartesian cells. |
| 2026-09-01 | Accept JMH `"NaN"` score errors only for short smoke validation. | Two measurement samples cannot produce a confidence interval; screen/decision profiles remain responsible for numeric uncertainty and fork evidence. |
| 2026-09-01 | Keep generic `merge`/`validate` strict and add an explicit `merge-smoke` path. | The parent-discovered NaN failure was a profile mismatch, not permission to weaken decision validation; validating child rows before writing also prevents invalid strict artifacts. |
| 2026-09-01 | Require a safe explicit `run-id` for follow-up smoke receipts. | `CREATE_NEW` protects the original receipt while making reruns deterministic and easy to identify at the same commit. |
| 2026-09-01 | Record tracked dirty-diff and benchmark source-tree hashes in environment metadata. | HEAD predates this uncommitted slice; the fresh receipt must identify the exact dirty implementation and every benchmark/harness file used. |
| 2026-09-01 | Shut down Clojure agent pools in the benchmark runner's `finally` boundary. | `clojure.java.shell/sh` can leave stream-reader agents alive after output is complete; the CLI must return promptly on both success and failure. |
| 2026-09-01 | Reject existing durable result and environment paths before any JMH child starts. | Raw evidence must be non-overwriting and a failed/partial run must not silently replace an earlier artifact. |

## Plan review findings

### Review 1: semantics, scope, and measurement

Verdict: revise, then review again.

#### Uncertain decisions

1. **Can shared state/completion code stay out of the hot-loop cost?** The
   options are a common base/delegate or duplicated self-contained loops. The
   common form is much easier to prove correct; the duplicate form may expose
   more to C2. Recommendation: implement common semantics first, retain the
   named self-contained classes as candidates, and allow duplication in the
   selected product only after Java-level JMH and inlining evidence clears the
   5%/3% rule.
2. **Do the one-to-four `Cons` paths and capacity 8 still pay for their
   branches?** The current comment refers to Java 8-era observations and no raw
   evidence survives. Recommendation: keep the policy in the first correct
   buffer so semantics and optimization are not changed together, then compare
   it with one regular all-chunk policy using bytes/op and dechunked latency.
3. **Can a whole source be classified safely once?** `IChunkedSeq` proves only
   the current node's shape. Recommendation: require a documented whole-tail
   invariant before product dispatch; otherwise the phase selects the mixed
   loop even when a specialized benchmark class wins a restricted row.

All three uncertainties have experiments before the dependent selection and a
simple fallback. None requires a user decision before implementation.

#### Confident changes

A. Corrected the phase boundary: existing unary collection arities already
call `xf-seq`; Phase 2 leaves their definitions unchanged while Phase 3 retains
their compatibility and adoption decision.

B. Required all seven named hand-written object classes to remain present and
be repaired in place or through correct delegation. Replacement/deletion would
contradict the repository's preservation rule.

C. Kept `IXFSeqBuffer` unchanged by default. The object engine can use the
concrete `ObjectBuffer`; broad interface edits could accidentally pull primitive
buffers into Phase 2.

D. Corrected concurrency/exception language. Successful `LazySeq` forcing is
cached; a thunk that throws is left available and may run again, so tests must
match retry behavior rather than require a cached exception.

E. Strengthened the specialization rule: a 5% point estimate is insufficient
without supporting fork samples, uncertainty, and a fresh rerun.

F. Corrected the environment label to macOS 26.2 / Darwin kernel 25.2.0.

### Review 2: post-revision check

Verdict: pass with no unresolved finding.

- The problem is the smallest necessary Phase 2 problem: one correct object
  engine and an evidence-based internal loop/buffer selection.
- The oracle split is explicit where Clojure 1.12.5 `sequence` conflicts with
  the parent design, so no timing difference is hidden behind value equality.
- The production fallback is always one mixed reduced-aware loop. No uncertain
  optimization is needed for correctness or phase completion.
- All named hand-written object variants remain first-class candidates, while
  primitive, ASM, unary-compatibility, multi-source, and fusion work stays in
  its assigned later phase.
- Correctness gates precede timing; direct-on linkage, allocations, fork data,
  cell-level baselines, and selection thresholds prevent an easy or asymmetric
  comparison.
- The four sequential slices have reviewable ownership and a parent checkpoint
  before any later slice or selection.

## Pre-implementation review

Gate performed after both plan reviews against the complete parent design,
current Clojure/Java production paths, retained object candidates, buffer,
tests/build, Phase 0 semantic evidence, Phase 1 handoff, and the local Clojure
1.12.5 source/bytecode observations.

### Findings by severity

**Blockers:** none.

**Medium impact, fixed before passing:** the reviewed draft could add a new
canonical mixed loop in Slice 1 and then repair `XFSeqStepSimple` in Slice 2,
creating an eighth object loop before any evidence justified it. The plan now
routes the provisional product through repaired `XFSeqStepSimple`, permits only
shared state/completion support rather than a second engine, and assigns the
remaining six candidates to Slice 2.

**Medium impact, fixed before passing:** repairing the shared
`XFSeqStep.ObjectStep` through its superclass could unintentionally rewrite the
preserved primitive `LongStep`/`DoubleStep`. Slice 2 now requires object-only
override/delegation when necessary and treats primitive behavior as outside the
phase boundary.

**Low impact, fixed before passing:** the differential matrix named consumable
Java iterators without requiring fresh fixtures. The plan now prevents an
already-consumed oracle source from producing false equality or timing results.

### Gate assessment

| Gate | Result |
|---|---|
| Problem validity | Pass. A correct generic object engine and fastest-correct Java baseline are necessary before any unary core migration. |
| Semantic fidelity | Pass. Values, construction, batch realization, accumulator changes, `Reduced`, completion, exceptions, sequence protocols, concurrency, and retention each have a named oracle and decisive tests. |
| Performance validity | Pass. Correctness precedes JMH; the primary lane is symmetric direct-on, forked, allocation-aware, cell-level, and explicitly local. |
| Structural simplicity | Pass. One repaired mixed loop is the default; specialization, duplication, and buffer branches must earn themselves under a recorded rule. No new canonical loop, generated path, flag, or source whitelist is planned. |
| Hot-path quality | Pass for planning. Direct Java rows, allocation data, wrapper bytecode, and inlining/JIT evidence decide interface, reduced-check, source-test, buffer, and small-batch costs. |
| Upstream fitness | Pass. The phase preserves named hand-written candidates, confines primitive/fusion/function-specific work, records raw evidence, and can end with an honest stop recommendation. |

Verdict: `ready for implementation`.

### What matters

- Phase 2 builds one ordinary lazy sequence engine, not another wrapper type.
- `sequence` is the value oracle; direct unary core behavior supplies the lazy
  construction and input-chunk contract that `sequence` itself does not match.
- The engine must keep real accumulator values, stop on `Reduced`, and complete
  exactly once, including empty input.
- Repaired `XFSeqStepSimple` is the simplest provisional product loop; every
  other hand-written object class remains a measured candidate.
- No-reduced or source-specialized code cannot reach arbitrary `xf-seq` without
  a structural proof.
- Direct-on JMH and allocation evidence choose complexity; a 5% point estimate
  alone is not enough.
- A correct but broadly slow result produces a stop recommendation, not flags,
  generation, or hidden special cases.

## Agent run log

| Date | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-08-31 | Initial plan | `/root` (`gpt-5.6-sol`, high) | Read all required workflow skills, the complete parent design, Phase 1 handoff, relevant Phase 0 evidence/history, current object engine/buffer/tests/build, Clojure 1.12.5 source and bytecode; ran the clean check and direct construction/chunk probes. | Draft created. |
| 2026-08-31 | Plan review 1 | `/root` (`gpt-5.6-sol`, high) | Reviewed semantics, scope, candidate preservation, buffer boundary, exception behavior, environment identity, and selection evidence. | Revised six confident issues; retained three evidence-gated uncertainties with one-loop fallbacks. |
| 2026-08-31 | Plan review 2 | `/root` (`gpt-5.6-sol`, high) | Rechecked the revised oracle split, phase boundary, sequential ownership, performance symmetry, and unresolved decisions. | Pass; no unresolved finding or user decision. |
| 2026-08-31 | Pre-implementation review | `/root` (`gpt-5.6-sol`, high) | Applied the strict problem, semantic, performance, simplicity, hot-path, and upstream gate. | Fixed the redundant-loop ambiguity, isolated the shared object candidate from primitive siblings, required fresh consumable fixtures; verdict `ready for implementation`. |
| 2026-08-31 | Implementation Slice 1 | `/root/phase2_slice1` | Added deferred object initializer, repaired mixed reduced-aware step, bounded/ownership-safe object buffer, direct LazySeq boundary, removed active generated-step setup, and deterministic object-engine tests. | Full local check passes: 14 tests / 180 assertions / 0 failures / 0 errors; parent inspection and checkpoint commit pending; no JMH claim. |
| 2026-09-01 | Slice 1 verification | `/root/phase2_slice1` | Recompiled the Java step after adding retained-node retry progress and reran focused/full checks plus diff hygiene. | Focused and full checks pass: 14 tests / 180 assertions / 0 failures / 0 errors; parent inspection and checkpoint commit pending; no JMH claim. |
| 2026-09-01 | Slice 1 parent checkpoint | `/root` (`gpt-5.6-sol`, medium) | Inspected the complete diff and mutable state transitions; checked the object-only scope and preserved research paths; independently ran focused tests, the full check, diff hygiene, and a 2,016-cell differential probe. | Accepted for checkpoint commit; no blocking semantic, scope, build, or hygiene finding. |
| 2026-09-01 | Slice 1 checkpoint | `/root` (`gpt-5.6-sol`, medium) | Committed the accepted Slice 1 code, tests, evidence, and plan updates without pushing. | `10e19e2bcccbf59dbe3d2f95e30d7d8134d749f2`; Slice 2 may start. |
| 2026-09-01 | Implementation Slice 2 | `/root/phase2_slice2` | Repaired the six retained object candidate classes through shared `XFSeqObjectStep` state/completion code; delegated `ObjectStep` while preserving primitive siblings; fixed the demonstrated reduced-completion retry defect in canonical `XFSeqStepSimple`; added test/benchmark-only shape/proof adapters, v2 registry/mapping, and candidate contract tests. | Initial candidate suite passed 11 tests / 117 assertions; parent-check follow-up expanded it to 15 / 2,726 and the full check to 29 / 2,906; Java accessibility probe passed; parent review and checkpoint commit pending; no JMH claim. |
| 2026-09-01 | Slice 2 parent-check follow-up | `/root/phase2_slice2` | Added fresh 16-size / eight-transform differential coverage for all applicable reduced-aware candidates, explicit dechunked-to-chunked and chunked-to-dechunked mixed tails, adapter-owned expanding no-reduced coverage, and terminal direct-reinvoke checks. | Candidate suite 15 tests / 2,726 assertions; full check 29 / 2,906; lint/reflection, Java 8 javac, diff hygiene, and external inherited-`invoke` probe all passed; awaiting parent review/checkpoint, no JMH claim. |
| 2026-09-01 | Slice 2 parent checkpoint | `/root` (`gpt-5.6-sol`, medium) | Inspected the shared state machine, canonical retry fix, primitive-sibling boundary, adapters, registry, and complete expanded candidate suite; independently reran focused tests, the full check, and diff hygiene. | Accepted for checkpoint commit; 15 / 2,726 focused and 29 / 2,906 full assertions pass with no semantic, scope, build, reflection, lint, or hygiene blocker. |
| 2026-09-01 | Slice 2 checkpoint | `/root` (`gpt-5.6-sol`, medium) | Committed the accepted Slice 2 code, tests, evidence, and plan updates without pushing. | `9ef99d1c81876909c0acad8a38505023d0a7db9c`; Slice 3 may start. |
| 2026-09-01 | Implementation Slice 3 | `/root/phase2_slice3` | Added the isolated JMH 1.37 Java/Clojure harness, direct-linked AOT callers, public/candidate/buffer groups, parameter registry, environment/result validator, non-overwriting smoke artifacts, and benchmark documentation. | Semantic gate passed (29 / 2,906); standalone jar and linkage gate passed; three tiny smoke groups completed; 19-row merged smoke validated; result/environment hashes recorded above. Parent review and checkpoint commit pending; no screen/decision or production-selection claim. |
| 2026-09-01 | Slice 3 parent-check follow-up | `/root/phase2_slice3` | Reproduced the parent-reported strict merge failure on JMH `"NaN"`; added profile-aware `merge-smoke`, strict pre-write merge validation, registry regression coverage, explicit non-overwriting smoke run IDs, and dirty-source hashes. | Fresh source-matched `bench-smoke '{:run-id "followup-20260901b"}'` exits 0; durable result/environment paths and hashes are recorded above. Original and intermediate receipts remain unchanged and validate under their appropriate profiles; no screen/decision or production-selection claim. Parent review and checkpoint commit pending. |
| 2026-09-01 | Slice 3 runner-lifecycle follow-up | `/root/phase2_slice3` | Preserved the parent `parent-20260901` receipts; fixed lingering `clojure.java.shell/sh` agents with `shutdown-agents` in a `finally`; measured fresh validate/environment subprocess termination and ran a final suffixed smoke. | Runner probes exit in 0.58s/0.68s with no lingering process; final `bench-smoke '{:run-id "lifecycle-20260901"}'` exits 0. Parent review and checkpoint commit pending; no screen/decision or production-selection claim. |
| 2026-09-01 | Slice 3 parent checkpoint | `/root` (`gpt-5.6-sol`, medium) | Inspected timed paths, applicability, AOT bytecode, result/environment writers, raw receipts, merge-profile isolation, and runner lifecycle; independently built the jar, ran all smoke groups, reproduced both workflow defects, revalidated the final lifecycle receipt, reran the registry test and full check, and ignored the generated clj-kondo cache. | Accepted for checkpoint commit; final smoke and validation helpers terminate cleanly, 19-row identity receipt validates, registry 1 / 5 and normal 29 / 2,906 suites pass, linkage/lint/reflection/hygiene are clean, and no performance decision claim is made. |
| 2026-09-01 | Slice 3 checkpoint | `/root` (`gpt-5.6-sol`, medium) | Committed the accepted Slice 3 harness, documentation, raw smoke receipts, evidence, and plan updates without pushing. | `ec3670ba9c7fcf8f48221fa0e9d53c52e78cb95e`; Slice 4 may start. |
