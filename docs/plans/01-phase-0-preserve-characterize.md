# Implementation #1, Phase 0: preserve and characterize

Status: Awaiting final review

Stage: implemented

Run stage: complete; final review: pending.

Last updated: 2026-08-31

Parent design: [`docs/01-transducer-backed-lazy-seqs.md`](../01-transducer-backed-lazy-seqs.md)

## Plain-English problem

Freeze and describe the last 2020 research implementation well enough that we
can simplify it later without losing its candidate loops, known behavior, or
the limited performance evidence it actually contains.

The goal is preservation and truthful characterization. A tag, a runner, and
benchmark output are mechanisms. They are useful only if a future contributor
can answer: “What exactly was the old implementation, what did it expose, what
was already broken, and how can I compare a repaired candidate with it?”

## Phase goal

1. Give the last 2020 research tree an immutable, human-readable Git reference
   and record its full commit identity.
2. Inventory the public namespace surface, the generated paths, and every
   hand-written Java step variant without deleting or repairing any of them.
3. Reproduce the existing tests and record the known semantic failures against
   direct Clojure behavior.
4. Capture a best-effort historical timing snapshot before semantic repairs,
   with exact environment and linking metadata and an explicit warning that it
   is not upstream performance evidence.
5. Add a short architecture/history note that points from the preserved code
   to the parent #1 design and gives every candidate a stable benchmark ID.

## Non-goals

- Do not repair `XFSeqHead`, completion, `Reduced`, `consume`, `drain`, map
  arities, buffer retention, or any Java step loop.
- Do not change `src/`, `src-java/`, or the legacy tests and benchmark in
  `test/xfseq/`.
- Do not modernize `deps.edn`, add the clean build, upgrade the stable target,
  or add JMH. Those are Phase 1 responsibilities.
- Do not select a production loop. Phase 2 first makes the object candidates
  semantically equivalent and then measures them.
- Do not make primitive specialization or ASM generation part of
  Implementation #1. They remain preserved research comparators.
- Do not redesign fusion. `consume` and `drain` are recorded as experimental
  and broken; Phase 6 owns any repair.
- Do not claim that a historical timing result supports a Clojure core change.

## Why this matters upstream

- **Core maintainer:** the eventual patch needs an auditable before-state and
  must not compare itself only with an easy or reconstructed baseline.
- **Library user:** old value-equality tests hide ordinary sequence and
  realization differences; the failure record prevents accidental claims of
  compatibility.
- **JVM engineer:** stable IDs and exact runtime/linking metadata let later
  measurements distinguish loop shape from source type and compiler setup.
- **Future contributor:** the history note explains why apparently unused Java
  and ASM code exists and which later phase may change it.

## Current repository facts

### Preservation point and history

- `168ce02f2dcb796045990fe1647205f4da20c1f5`, committed on 2020-05-10,
  is the last 2020 commit and the last commit that changes `src/`, `src-java/`,
  `test/`, or `deps.edn`.
- A path-limited diff from that commit to current `HEAD` is empty for `src/`,
  `src-java/`, `test/`, `deps.edn`, `README.md`, `CHANGELOG.md`, `doc/`, and
  `.gitignore`.
- The repository has 48 commits, one `master` branch, and no tags.
- The current worktree was clean when this plan was prepared.
- The tree listing for the research paths at the preservation commit hashes to
  `d9db3f7db3c8c62b786e2241c648038daa7ba5ac4f7c52e9431f25eb67da882b`.
  A Git archive of `src`, `src-java`, `test`, and `deps.edn` hashes to
  `effaac4ee627ceb17581dc2e6c9c40b12b9e72a46c282d0239e041fdb5befc67`.

The selected immutable reference is an annotated tag:

```text
research-2020-05-10 -> 168ce02f2dcb796045990fe1647205f4da20c1f5
```

The full SHA and checksums remain authoritative if the tag is not fetched. The
run must stop if that tag already exists and resolves to another commit; it
must not move an existing tag silently.

### Build and runtime state

- `deps.edn` pins Clojure 1.10.1 and Criterium 0.4.5.
- The Criterium coordinate is unqualified and produces a deprecation warning
  with the installed Clojure CLI.
- The source path includes `classes/production/xfseq`, an ignored IDE output
  directory. There is no Java compilation command and no `:test` alias.
- From a checkout without ignored class files, requiring `xfseq.core` fails
  with `ClassNotFoundException: xfseq.ILongSeq`.
- Compiling the tracked Java sources to a temporary directory with
  `javac --release 8` makes the legacy suite runnable without changing the
  repository.
- The installed CLI is 1.12.5.1664. The only installed JDK is Homebrew OpenJDK
  26.0.2.1 on macOS 26.2, arm64.

The clean build is intentionally not fixed here. Phase 0 documents a temporary
reproduction command; Phase 1 replaces it with the supported build.

### Existing API surface

The history note must distinguish intended/experimental user operations from
incidental public Vars generated by the prototype.

| Surface | Current arities or role | Phase 0 classification |
|---|---|---|
| `xfseq.core/xf-seq` | `[xf coll]` | Intended generic sequence entry point |
| `map`, `filter`, `remove`, `take` | transducer and one-collection arities | Intended prototype replacements; incomplete relative to core |
| `consume` | `[rf init coll]` | Experimental fusion sink; known completion bug |
| `drain` | `[coll]` | Experimental destructive fusion wrapper; known composition bug |
| `long-*`, `double-*`, primitive seq protocols/classes | primitive research helpers | Implementation #2 research; preserved, not #1 API |
| `xfseq.gen/xf-seq` and generation functions | runtime ASM path | Historical comparator; excluded from #1 product path |
| generated constructors, factories, maps, and analyzers visible in `ns-publics` | implementation support | Incidental public surface; inventory only, no compatibility decision |

The README, introductory document, and changelog are still template text and
do not define a trustworthy API contract.

### Candidate inventory and stable benchmark IDs

The Phase 0 history note and characterization runner must use these IDs. A
later rename may add an alias, but must not silently reuse an ID for another
loop.

| Stable ID | Existing implementation | Intended shape |
|---|---|---|
| `core-direct` | direct `clojure.core` function | External semantic and performance baseline |
| `core-sequence` | `clojure.core/sequence` with equivalent transducer | Secondary generic-engine baseline |
| `legacy-clj-generated` | generated `XFSeqStep_*` deftypes in `xfseq.core` | Prototype Clojure-generated path |
| `legacy-asm-<arg>-<input>-<identity-stop|no-stop>-<mode>` | 54 constructors in `xfseq.gen` | ASM matrix: 3 argument types × 3 input types × identity stop check on/off × mixed/chunked/dechunked |
| `java-polymorphic-object-identity-stop` | `XFSeqStep.ObjectStep` | Shared base class, object input/output, identity-based stop check |
| `java-polymorphic-long-identity-stop` | `XFSeqStep.LongStep` | Shared base class, primitive long input, identity-based stop check |
| `java-polymorphic-double-identity-stop` | `XFSeqStep.DoubleStep` | Shared base class, primitive double input, identity-based stop check |
| `java-mixed-object-identity-stop` | `XFSeqStepSimple` | Mixed chunked/dechunked object loop with identity-based stop check |
| `java-mixed-object-no-stop` | `XFSeqStepSimpleNoReduced` | Mixed object loop with no stop check |
| `java-dechunked-object-identity-stop` | `XFSeqStepSingleOnly` | Known dechunked object source with identity-based stop check |
| `java-dechunked-object-no-stop` | `XFSeqStepSingleOnlyNoReduced` | Known dechunked object source with no stop check |
| `java-chunked-object-identity-stop` | `XFSeqStepChunkedOnly` | Known chunked object source with identity-based stop check |
| `java-chunked-object-no-stop` | `XFSeqStepChunkedOnlyNoReduced` | Known chunked object source with no stop check |
| `java-mixed-long-from-object` | `XFSeqStepSimpleLong` | Primitive-long reducing call over ordinary object seq/chunk input |
| `java-mixed-long-from-long` | `XFSeqStepSimpleLongLong` | Primitive-long reducing call over primitive-long seq/chunk input |
| `java-mixed-object-from-long` | `XFSeqStepSimpleObjectLong` | Object reducing call over primitive-long seq/chunk input |
| `java-mixed-double-from-long` | `XFSeqStepSimpleDoubleLong` | Primitive-double reducing call over primitive-long seq/chunk input |

The commit that introduced the source-shape Java variants describes them as
inspiration for ASM. The parent #1 design deliberately promotes the object
variants to production candidates, but only after Phase 2 repairs them to one
semantic contract.

### Baseline behavior already reproduced during planning

The following are planning observations, not Phase 0 completion evidence. They
must be reproduced by the committed characterization runner during the run.

- With temporary Java compilation, the legacy suite passes all 46 assertions
  on both Clojure 1.10.1 and Clojure 1.12.5 under JDK 26.0.2.1.
- The suite contains one test and compares fully realized values. It does not
  exercise the ordinary sequence surface, construction/realization timing,
  completion on empty input, transducer application count, or fusion behavior.
- On both Clojure versions, direct probes reproduce these known differences:
  - `xfseq.core/map` returns `XFSeqHead`; `count` throws
    `UnsupportedOperationException` and `vec` throws `RuntimeException`.
  - A transducer is not applied at construction, but is applied twice when the
    first value is realized.
  - A completion-emitting transducer produces `[:completed]` through
    `clojure.core/sequence` on empty input and `[]` through `xfseq.core/xf-seq`.
  - `consume` returns step output without the ordinary reducing-function
    completion result.
  - Draining two nested maps loses one transformation: two `inc` maps over
    `[1 2]` produce `[2 3]`, not `[3 4]`.
  - Constructing `xfseq.gen/xf-seq` calls `seq` on a traceable source.
  - `xfseq.core/map` has only `[f]` and `[f coll]` arities.
- Requiring `xfseq.core` directly from the declared checkout paths, without
  manually compiled Java, fails before tests can start.

### Existing performance evidence

- `test/xfseq/bench.clj` uses Criterium 0.4.5, size 10,000, eight source shapes,
  an identity-style map, a full `reduce` sink, and a reducing function that
  discards every value.
- The core and `xfseq.core` sections are reader-disabled. Only `xfseq.gen` is
  active in the checked-in file. History contains no raw Criterium output.
- The only numeric result in source is an informal `time` comment claiming
  roughly 111 ms for core versus 13 ms for xfseq on a primitive-array reduction.
  The environment, forks, uncertainty, allocations, and linking symmetry were
  not recorded.
- The source-loaded caller and candidate default to direct linking off while
  the released Clojure core jar is direct-linked. Such results are asymmetric
  and cannot decide release performance.

Therefore no existing number is accepted as upstream performance evidence.

## Options and trade-offs

### Preservation mechanism

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| Annotated tag plus full SHA and checksums | Small, immutable by convention, preserves actual Git history, easy to diff | Tag must be fetched/pushed separately | **Choose** |
| Preservation branch | Familiar worktree workflow | Branch is mutable and adds no value over the exact commit | Reject |
| Copy the 2020 source into a historical source tree | Survives missing refs | Duplicates thousands of lines and can drift | Reject; the full SHA is sufficient |
| Commit a binary source archive | Portable snapshot | Repository bloat and duplicated source | Reject; record the archive checksum only |

### Characterization form

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| Documentation only | Least code | Easy for claims and commands to go stale | Reject |
| One small historical characterization runner plus raw EDN reports | Repeatable, diffable, keeps expected historical failures explicit | Temporary compilation command remains awkward until Phase 1 | **Choose** |
| Add failures to the normal test suite | Uses familiar tooling | Later repairs would make tests of broken behavior fail; confuses product correctness with archaeology | Reject |
| Build the full future differential suite now | Strong semantics | Pulls Phase 2 work into Phase 0 | Reject |

### Historical timing snapshot

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| Preserve source/comments only | Perfectly honest | Does not satisfy the phase request to capture a before-repair snapshot | Reject |
| Run the pinned Criterium harness in fresh JVM processes and save raw output | Small extension of the original experiment; captures a useful before-state | No allocation data and not a JMH-quality matrix | **Choose as historical context only** |
| Introduce JMH and the release matrix now | Could support real claims | Duplicates Phase 1 and delays preservation | Reject |
| Reconstruct an assumed 2020 JDK/hardware environment | Superficial historical resemblance | Original vendor/build, flags, OS, and hardware are unknown | Reject |

## Selected approach: simplest viable Phase 0

1. Tag the exact last 2020 commit and record its SHA/checksums.
2. Add one concise history/architecture note with the API, candidates, stable
   IDs, known failures, and links to the parent design.
3. Add one development-only semantic characterization runner. It records both
   expected legacy successes and expected legacy failures as data; it does not
   alter normal product tests.
4. Add one development-only historical benchmark runner that makes the three
   existing top-level paths selectable and emits stable case IDs and raw EDN or
   Criterium output. It must not alter the preserved sources at the tag.
5. Run the evidence tooling from the current working tree, whose production
   files are byte-for-byte unchanged from the tagged tree, using temporary Java
   output. Commit raw reports and exact commands under `results/phase-0/`.
6. Record that the timing snapshot is non-decisional. Phase 1 supplies the
   modern build/JMH infrastructure; Phase 2 supplies correctness repairs and
   the fastest-correct-candidate comparison.

## Impact / Effort / Value priorities

| Item | Impact | Effort | Value | Dependency/evidence | Decision |
|---|---|---|---|---|---|
| Immutable commit reference and manifest | High | Low | High | Exact 2020 commit and empty production diff are known | Now |
| Candidate/API inventory with stable IDs | High | Medium | High | Source and history inspection complete | Now |
| Repeatable semantic characterization | High | Medium | High | Direct Clojure oracle; temporary Java compilation | Now |
| Raw legacy timing snapshot | Medium | Medium | Medium | Pinned Criterium; semantic labels; exact runtime metadata | Now, historical only |
| Benchmark every Java variant | High eventually | High | High eventually | Variants must first share the Phase 2 semantic contract | Later phase |
| Allocation/JMH evidence | High | High | High | Modern build and benchmark aliases | Phase 1 infrastructure, Phase 2 decisions |
| Build modernization | High | Medium | High | Preservation must land first | Phase 1 |
| Semantic repairs | High | High | High | Preserved before-state | Phase 2 and later |
| Primitive/ASM product path | Low for #1 | High | Low for #1 | Implementation #2 | Reject from #1; preserve only |
| Fusion repair | Low for Phase 0 | High | Low for initial engine | Separate contract needed | Phase 6 |

## Confidence ledger

| ID | Kind | Statement / failure mode | Resolution | Confidence |
|---|---|---|---|---|
| C1 | Fact | `168ce02...` is the last production/research change from 2020, and the current research paths do not differ from it. | Re-run path-limited `git diff --exit-code` and tree checksums in the phase run. | High; Git inspection agrees across log and diff. |
| C2 | Fact | The clean declared classpath cannot load `xfseq.core` because Java classes are not built. | Capture the failure, then compile tracked Java to a fresh temporary directory and rerun. | High; reproduced directly. |
| C3 | Fact | The legacy value suite passes while major sequence semantics fail. | Run the committed characterization on Clojure 1.10.1 and 1.12.5 and save both reports. | High; reproduced directly on both versions. |
| C4 | Fact | The existing benchmark has no raw results, allocation evidence, or valid direct-linking comparison. | Preserve the file/history and state the limits in every report. | High; repository-wide search and history show no result artifacts. |
| C5 | Assumption | An annotated tag plus SHA/checksums is sufficient preservation without copying source. | Verify the tag target and document checkout/archive commands; retain the full SHA as fallback. | High; Git objects already contain the full tree. |
| C6 | Assumption | Small development-only runners are enough to make characterization reproducible before Phase 1. | Validate from a checkout with ignored output absent, using only documented temporary paths. | Medium; the manual commands work, but the runner does not exist yet. |
| C7 | Unknown | Whether every hand-written Java variant can be invoked safely by one historical adapter without changing behavior. A broad adapter could accidentally repair or reinterpret a loop. | Smoke-construct each variant only where its source and reducing-function preconditions are structurally satisfied. Record unsupported combinations; do not add dispatch or shims. Full comparison waits for Phase 2. | Medium; class contracts are visible, but they are currently unwired. |
| C8 | Unknown | Criterium 0.4.5 behavior and output stability on JDK 26 may limit machine-readable capture. | Run one small warmup case first. If structured output is unavailable, preserve stdout plus metadata and checksums; do not upgrade Criterium in this phase. | Medium; dependency resolves, benchmark has not been run in planning. |
| C9 | Failure mode | A green legacy suite could be presented as semantic equivalence. | Reports must show both the 46 passing assertions and the direct-oracle failure matrix side by side. | High confidence in mitigation. |
| C10 | Failure mode | Fast but incorrect or asymmetrically linked rows could be presented as performance evidence. | Every performance row carries semantic status and linking mode; the summary states “historical context only” and makes no winner/adoption claim. | High confidence in mitigation. |
| C11 | Failure mode | Creating a tag at current `HEAD` would mix 2026 planning files into the historical tree. | Tag the explicit `168ce02...` commit and verify `^{commit}` exactly. | High confidence in mitigation. |
| C12 | Unknown | The exact 2020 JDK, OS, hardware, JVM flags, and original benchmark output are unrecoverable from tracked files. | Record the absence. Do not infer them from the Java 8 source target or comments. Use the exact current snapshot environment instead. | High that the limitation is real; repository evidence is exhausted. |

Every phase-critical unknown has a resolution before its dependent evidence is
accepted. C7 may result in an explicitly unsupported historical adapter row;
it cannot block preservation because full Java-candidate selection belongs to
Phase 2. C8 may fall back from structured Criterium data to checksummed raw
stdout, but it may not fall back to an unrecorded performance claim.

## Decisive experiments

1. **Preservation identity:** verify the tag target, path-limited zero diff, Git
   tree checksum, and archive checksum.
2. **Clean-load failure:** from a checkout with no ignored class output, show
   the declared configuration cannot require `xfseq.core`.
3. **Temporary reproduction:** compile all tracked Java with `--release 8` to a
   fresh temporary directory and run the unchanged legacy suite.
4. **Semantic oracle:** compare the public prototype with direct Clojure for
   result surface, construction source access, transducer initialization,
   empty completion, reducing-function completion, drain composition, and map
   arities. Save results for Clojure 1.10.1 and 1.12.5.
5. **Candidate reachability:** map every stable ID to a class/constructor and
   smoke only its declared source/reduction shape. No timing row is accepted if
   setup itself throws or silently uses another candidate.
6. **Historical timing smoke:** run one tiny Criterium case on JDK 26 before the
   full snapshot. If the pinned tool fails, capture the exact failure and keep
   the plan `Needs replanning`; do not upgrade dependencies silently.
7. **Snapshot repeatability:** run each historical timing group in three fresh
   JVM processes and retain every raw result, environment record, and command.
   Variation is reported; no results are pooled into an overall speedup.

## Evidence files and ownership boundaries

Expected Phase 0 changes are restricted to these areas:

| Area | Responsibility |
|---|---|
| `docs/history/01-2020-research-state.md` | Preservation manifest, architecture note, API/candidate inventory, limitations |
| `dev/xfseq/phase_0_characterize.clj` | Development-only semantic report; no product implementation |
| `dev/xfseq/phase_0_bench.clj` | Development-only legacy timing selection and stable IDs; no product dispatch |
| `results/phase-0/semantic/` | Raw EDN reports for the two Clojure versions |
| `results/phase-0/performance/` | Raw Criterium output and per-fork environment metadata |
| This plan | Commands, decisions, evidence links, review findings, and run log |

Use these exact result stems so evidence is discoverable and cannot be
silently overwritten:

```text
results/phase-0/semantic/clj-1.10.1-jdk-26.0.2.1.edn
results/phase-0/semantic/clj-1.12.5-jdk-26.0.2.1.edn
results/phase-0/performance/clj-1.10.1-jdk-26.0.2.1/
  legacy-original-shape/fork-{1,2,3}.{edn,stdout,meta.edn}
  historical-on/fork-{1,2,3}.{edn,stdout,meta.edn}
```

If Criterium cannot emit structured EDN, omit the `.edn` result rather than
inventing a parser; the checksummed `.stdout` and `.meta.edn` files remain the
raw record.

No slice owns or may modify `src/`, `src-java/`, `test/xfseq/`, or `deps.edn`.
If a runner exposes a production defect, it records the defect and stops; it
does not repair it.

## Ordered implementation slices

Workers run sequentially. The parent orchestrator validates each slice before
starting the next one.

### Slice 1: preservation manifest and immutable reference

Ownership: `docs/history/01-2020-research-state.md`, the annotated tag, and the
preservation entries in this plan.

1. Recheck the worktree and exact preservation SHA.
2. Create `research-2020-05-10` only if absent; never move an existing ref.
3. Record tree/archive checksums, checkout commands, API inventory, candidate
   IDs, and links to the old code and parent design.
4. Confirm no production or legacy test/benchmark file changed.

Parent integration check: resolve the tag, recompute checksums, and review the
manifest against every tracked source and test file.

### Slice 2: semantic characterization

Ownership: `dev/xfseq/phase_0_characterize.clj` and
`results/phase-0/semantic/`.

1. Implement a data-oriented runner for the legacy suite summary and the known
   direct-oracle differences.
2. Keep expected historical failures visibly labeled as failures, not as
   product test successes.
3. Run it with Clojure 1.10.1 and 1.12.5 using fresh temporary Java output.
4. Record Java/Clojure/CLI/OS/architecture, direct-linking property, complete
   command, exit status, and raw result checksum.

Parent integration check: rerun both lanes, compare raw reports, and inspect
that the runner does not alter or bypass the behavior under test.

### Slice 3: candidate registry and historical timing snapshot

Ownership: `dev/xfseq/phase_0_bench.clj` and
`results/phase-0/performance/`.

1. Encode the stable IDs without adding production dispatch.
2. Make the existing top-level core, Clojure-generated, and ASM-generated
   benchmark paths selectable with identical sources, functions, and sinks.
3. Smoke reachable hand-written variants only under their declared shape and
   record any unreachable variants for Phase 2.
4. Run the two linking lanes and three fresh JVM processes per timing group.
5. Save raw per-fork data. Do not summarize an overall winner.

Parent integration check: verify implementation identity, case symmetry,
semantic labels, three process IDs/forks, raw checksums, and the absence of any
release-performance claim.

### Slice 4: evidence integration and phase handoff

Ownership: this plan and final corrections to the history note only.

1. Link every raw artifact and record all commands and versions.
2. Audit every exit criterion and production-path diff.
3. Record unresolved historical limitations without creating flags or shims.
4. Mark the plan `Awaiting final review` and stop.

Parent integration check: run `git diff --check`, inspect the complete diff,
and verify that Phase 1 work has not started.

## Semantic validation method

### Oracles

- Clojure 1.12.5 direct functions are the current semantic oracle.
- Clojure 1.10.1 is the historical dependency lane.
- `clojure.core/sequence` is the generic `xf-seq` oracle for values and
  transducer completion; direct `map`, `filter`, `remove`, and `take` remain the
  oracle for their public realization behavior.

### Required report cases

- Existing 46 assertions and their exact summary.
- Public result class and behavior of `seq?`, `sequential?`, `count`, and `vec`.
- Construction-only source trace for core, `xfseq.core`, and `xfseq.gen`.
- Number and timing of transducer applications.
- Empty source with completion output.
- Early reduction within chunked and dechunked input, recorded without claiming
  correctness beyond cases actually probed.
- `consume` reducing-function completion.
- Two-stage `drain` composition.
- Public arities of `map`, `filter`, `remove`, and `take`.
- Clean-checkout require result before temporary Java compilation.

The Phase 0 report freezes observations. It is not the full differential,
trace, concurrency, exception, or retention suite specified for Phases 2–4.

## Historical performance method

### Exact runtime matrix

| Lane | Clojure | Java | Linking | Purpose |
|---|---|---|---|---|
| `legacy-original-shape` | 1.10.1 | Homebrew OpenJDK 26.0.2.1, arm64 | Released core jar direct-linked; source-loaded candidate/caller default off | Reproduce the checked-in experiment shape; diagnostic and asymmetric |
| `historical-on` | 1.10.1 | Homebrew OpenJDK 26.0.2.1, arm64 | `-Dclojure.compiler.direct-linking=true` for candidate/caller; released core jar on | Best available before-repair timing context, still non-decisional |

Both lanes use macOS 26.2, Criterium 0.4.5, Java 8 target bytecode, G1 GC,
`-Xms2g -Xmx2g`, and three separately launched JVM processes. The runner must
record the full JVM version string, flags, OS build, architecture, available
processor count, heap, and candidate commit for each process.

No direct-linking-off release comparison is claimed: obtaining one would
require rebuilding Clojure itself without direct linking, which belongs to the
modern symmetric harness, not this phase.

### Matrix retained from the legacy benchmark

- Sources: repeat, vector of repeated objects, range, vector of range, set,
  object array, long array, and double array.
- Size: 10,000.
- Work: identity or typed identity map.
- Sink: full `reduce` with the existing value-discarding reducing function.
- Top-level paths: direct core, `xfseq.core/xf-seq`, and
  `xfseq.gen/xf-seq` under identical cases.

The runner may add construction-only smoke and implementation-identity checks,
but must not enlarge the workload matrix or tune candidates in Phase 0.

### Interpretation rule

- Preserve raw scores and uncertainty from each process.
- Do not average unrelated sources or report an overall speedup.
- Mark every legacy candidate semantically non-equivalent until later repaired.
- State that allocation was not measured and that these rows cannot support an
  upstream performance claim.
- The release decision remains reserved for forked JMH with allocation evidence
  and symmetric direct linking after the modern build exists.

## Exit criteria

Phase 0 may move to final review only when all are true:

1. `research-2020-05-10^{commit}` resolves exactly to
   `168ce02f2dcb796045990fe1647205f4da20c1f5`.
2. The preservation manifest records the full SHA, checksums, checkout command,
   current production zero-diff, API surface, candidate inventory, stable IDs,
   benchmark intent, and links to the parent design.
3. Every tracked hand-written Java, primitive, and ASM path remains reachable
   through Git and is neither deleted nor rewritten.
4. A fresh temporary compilation can run the unchanged legacy suite on both
   Clojure 1.10.1 and 1.12.5, with raw reports committed.
5. The direct-oracle report reproduces and labels every confirmed Phase 0
   failure listed above on both dependency lanes, or the plan records and
   resolves any changed observation before continuing.
6. Stable candidate IDs map unambiguously to source classes or generated
   constructor keys.
7. The historical timing smoke and three-process snapshot complete for both
   linking lanes, with raw files, commands, metadata, and checksums. If pinned
   Criterium is incompatible, the plan is marked `Needs replanning`; dependency
   modernization is not pulled into this phase.
8. Timing documentation makes no correctness, allocation, release-equivalence,
   or upstream adoption claim.
9. `src/`, `src-java/`, `test/xfseq/`, and `deps.edn` are unchanged from the
   preservation commit.
10. The complete diff passes `git diff --check`; the plan contains the final
    validation evidence, decisions, and sequential agent run log.
11. The phase ends as `Awaiting final review`. Phase 1 does not begin without a
    new `$xfseq-phase` invocation.

## Decision log

| Date | Decision | Reason |
|---|---|---|
| 2026-08-31 | Preserve explicit commit `168ce02...`, not current `HEAD`. | Current `HEAD` includes 2026 design and agent files; the research paths themselves last changed in 2020. |
| 2026-08-31 | Use an annotated tag plus full SHA/checksums, not a branch or copied source tree. | This is the smallest durable preservation mechanism and avoids duplicate code. |
| 2026-08-31 | Keep characterization outside the normal product tests. | Later repairs must not fail because an archaeology test expects a bug. |
| 2026-08-31 | Preserve and name all Java/primitive/ASM candidates, but benchmark selection waits for semantic repair. | Fast incorrect candidates cannot be production baselines. |
| 2026-08-31 | Capture Criterium only as historical context. | The original harness lacks allocation evidence and valid release-equivalent symmetry. |
| 2026-08-31 | Use the exact available JDK 26 environment instead of inventing a 2020 environment. | The original runtime and hardware are not recorded. |
| 2026-08-31 | Leave build modernization and JMH to Phase 1. | Phase 0 needs preservation evidence, not a second build design. |
| 2026-08-31 | Name legacy stop checks `identity-stop`, not `reduced`. | The loops compare accumulator identity and are not yet correct implementations of Clojure's `Reduced` contract. |
| 2026-08-31 | Treat the full SHA in the committed manifest as authoritative; the tag is a human-readable local/published convenience. | Tags are transferred separately and the run has no implicit authority to publish refs. |
| 2026-08-31 | Create `research-2020-05-10` as a local annotated tag at the explicit preservation SHA, and do not publish it in Slice 1. | The tag gives local history a readable name without moving or duplicating the 2020 tree; the full SHA and checksums remain the durable manifest. |
| 2026-08-31 | Keep Slice 2 as one dependency-free, data-oriented semantic runner with expected historical differences labeled in EDN. | The unchanged suite stays a green preservation smoke while direct Clojure oracles make the incomplete surface and known defects auditable without turning archaeology into product tests. |
| 2026-08-31 | Keep Slice 3 timing expressions statically linked at their candidate call sites, and resolve no vars inside timed closures. | The two direct-linking lanes must measure the requested caller/candidate compiler modes without adding development-runner dispatch overhead to every Criterium iteration. |
| 2026-08-31 | Keep Criterium 0.4.5 with bounded samples/warmup/target and preserve every raw sample and uncertainty field. | The pinned JDK 26 smoke succeeded; bounded execution keeps six fresh JVM snapshots practical while retaining honest historical context and explicitly avoiding an adoption claim. |

## Validation evidence

Planning-time evidence, to be replaced or supplemented with durable run
artifacts:

| Date | Command or inspection | Result |
|---|---|---|
| 2026-08-31 | `git log`, `git diff 168ce02..HEAD -- <research paths>`, tag/branch inspection | 48 commits; no tag; no research-path change after `168ce02`. |
| 2026-08-31 | Clean declared-classpath require with Clojure 1.10.1 | Failed with `ClassNotFoundException: xfseq.ILongSeq`. |
| 2026-08-31 | Temporary `javac --release 8` plus unchanged legacy tests, Clojure 1.10.1/JDK 26 | 1 test, 46 assertions, 0 failures, 0 errors. |
| 2026-08-31 | Same temporary build and tests, Clojure 1.12.5/JDK 26 | 1 test, 46 assertions, 0 failures, 0 errors. |
| 2026-08-31 | Direct semantic probes on both Clojure lanes | Reproduced incomplete sequence surface, double transducer application, lost empty completion, incomplete `consume`, broken `drain`, eager ASM construction, and missing map arities. |
| 2026-08-31 | Benchmark/history inspection | One Criterium source file; only ASM block active; no raw result files; one unqualified timing comment. |
| 2026-08-31 | `git rev-parse 168ce02f2dcb796045990fe1647205f4da20c1f5^{commit}` and `git show -s --format=fuller 168ce02f2dcb796045990fe1647205f4da20c1f5` | Exact preservation commit confirmed: `168ce02f2dcb796045990fe1647205f4da20c1f5`, committed 2020-05-10 14:08:31 -0700; subject is the chunk-processing extraction. |
| 2026-08-31 | `git tag -a research-2020-05-10 168ce02f2dcb796045990fe1647205f4da20c1f5 -m 'Preserve 2020 research implementation for xfseq Implementation #1'`; then `git rev-parse research-2020-05-10^{commit}` and `git cat-file -t research-2020-05-10` | Local annotated tag created; it resolves exactly to the preservation commit and has type `tag`. Tag object: `555c01620cce3b1eeb59384008a7d30786e4a427`. |
| 2026-08-31 | `git ls-remote origin refs/tags/research-2020-05-10 refs/tags/research-2020-05-10^{}` | No rows; tag is local only and was not published. |
| 2026-08-31 | `git ls-tree -r --full-tree research-2020-05-10 -- src src-java test deps.edn \| shasum -a 256`; `git archive --format=tar research-2020-05-10 -- src src-java test deps.edn \| shasum -a 256` | Tree-list SHA-256 `d9db3f7db3c8c62b786e2241c648038daa7ba5ac4f7c52e9431f25eb67da882b`; archive SHA-256 `effaac4ee627ceb17581dc2e6c9c40b12b9e72a46c282d0239e041fdb5befc67`. |
| 2026-08-31 | `git ls-tree -r --name-only research-2020-05-10 -- src src-java test deps.edn \| wc -l` and path inventory | 34 tracked preservation files: 4 Clojure, 27 Java, 2 test/benchmark, and `deps.edn`; every path is listed in [`docs/history/01-2020-research-state.md`](../history/01-2020-research-state.md). |
| 2026-08-31 | `git diff --quiet 168ce02f2dcb796045990fe1647205f4da20c1f5..HEAD -- src src-java test deps.edn README.md CHANGELOG.md doc .gitignore; echo path_diff_exit=$?` | Exit `0`, no output; the production, legacy test/benchmark, dependency, and legacy documentation paths remain byte-for-byte unchanged. |
| 2026-08-31 | Slice 2 runner, Clojure 1.10.1/JDK 26.0.2.1, fresh `javac --release 8` output | Exit `0`; unchanged suite summary is 1 test, 46 assertions, 0 failures, 0 errors. Report: [`clj-1.10.1-jdk-26.0.2.1.edn`](../../results/phase-0/semantic/clj-1.10.1-jdk-26.0.2.1.edn), 10,932 bytes, SHA-256 `2b74bdc387ec6415a1c2c4482d815def0c3295689bc036da5dea1ddfd3af63d4`; direct-linking property `false`; clean declared-classpath child require exit `1`, `ClassNotFoundException`, missing `xfseq.ILongSeq`. |
| 2026-08-31 | Slice 2 runner, Clojure 1.12.5/JDK 26.0.2.1, fresh `javac --release 8` output | Exit `0`; unchanged suite summary is 1 test, 46 assertions, 0 failures, 0 errors. Report: [`clj-1.12.5-jdk-26.0.2.1.edn`](../../results/phase-0/semantic/clj-1.12.5-jdk-26.0.2.1.edn), 10,965 bytes, SHA-256 `ee1a06313dad555efc3105efdcd7ea587e734281b055ee1430e0bab7e4e6cf87`; direct-linking property `false`; clean declared-classpath child require exit `1`, `ClassNotFoundException`, missing `xfseq.ILongSeq`. |

Slice 2 used Homebrew Clojure CLI `1.12.5.1664`, Homebrew OpenJDK
`26.0.2.1` (`aarch64`, macOS `26.2`), and exact Maven jars for each Clojure
lane plus its pinned `spec.alpha` and `core.specs.alpha` dependencies. Each
lane compiled all tracked Java sources into a new `/private/tmp` directory,
then launched the runner with direct linking explicitly set to `false` and
the declared source paths. The runner independently launched the clean-load
child with only the Clojure/dependency jars and `src`, so the missing Java
class was observed after the temporary compiled run as metadata rather than
silently inferred. The reports retain the exact suite output and all required
direct-oracle observations: result surface, construction source traces,
transducer application counts/timings, empty completion, chunked/dechunked
early reduction, `consume` completion, two-stage `drain`, public arities, and
the clean classpath failure. Timing fields are diagnostic only; expected
historical differences remain labeled and no correctness or performance claim
is made.

Reproducible command shape (the reports contain the fully expanded command,
temporary output path, classpath, and runtime metadata):

```sh
javac --release 8 -cp <clojure-jar>:<spec-alpha-jar>:<core-specs-alpha-jar> \
  -d <fresh-temp-classes> $(find src-java -name '*.java' -print | sort)
java -cp <clojure-jar>:<spec-alpha-jar>:<core-specs-alpha-jar>:<fresh-temp-classes>:<repo>/src:<repo>/test:<repo>/dev \
  -Dclojure.compiler.direct-linking=false clojure.main -m xfseq.phase-0-characterize \
  --output results/phase-0/semantic/<report>.edn \
  --clojure-version <1.10.1-or-1.12.5> --cli-version 1.12.5.1664 \
  --source-root <repo>/src \
  --clean-classpath <clojure-jar>:<spec-alpha-jar>:<core-specs-alpha-jar>:<repo>/src \
  --command '<full javac/java description>'
```

### Slice 3 validation evidence (2026-08-31)

The pinned dependency smoke resolved Clojure 1.10.1 with its declared
`spec.alpha` 0.2.176 and `core.specs.alpha` 0.2.44 jars, plus Criterium 0.4.5,
without editing `deps.edn`. The Criterium jar was downloaded from
`https://repo.clojars.org/criterium/criterium/0.4.5/criterium-0.4.5.jar`.
The dependency SHA-256 values are:

| Artifact | SHA-256 |
|---|---|
| `clojure-1.10.1.jar` | `d4f6f991fd9ed2a59e7ea4779010b3b069a2b905f3463136c42201106b4ad21a` |
| `spec-alpha-0.2.176.jar` | `fc4e96ecff34ddd2ab7fd050e74ae1379342ee09daa6028da52024c5de836cc4` |
| `core-specs-alpha-0.2.44.jar` | `3b1ec4d6f0e8e41bf76842709083beb3b56adf3c82f9a4f174c3da74774b381c` |
| `criterium-0.4.5.jar` | `c8d798059a7d185dcb528ed4edb0af6313aabcfa54cf9c8e1f84928d548dd3d9` |

The required one-case compatibility smoke ran first in a fresh JVM with JDK
26, G1, and the 2 GiB heap:

```sh
/opt/homebrew/Cellar/openjdk/26.0.2.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  -Xms2g -Xmx2g -XX:+UseG1GC \
  -Dclojure.compiler.direct-linking=false \
  -cp /private/tmp/xfseq-phase0-jars/clojure-1.10.1.jar:/private/tmp/xfseq-phase0-jars/spec-alpha-0.2.176.jar:/private/tmp/xfseq-phase0-jars/core-specs-alpha-0.2.44.jar:/private/tmp/xfseq-phase0-jars/criterium-0.4.5.jar \
  clojure.main -e "(require '[criterium.core :as c]) (let [r (c/benchmark* (fn [] (reduce + 0 [1 2 3])) {:samples 3 :warmup-jit-period 1000000 :target-execution-time 1000000 :bootstrap-size 20 :max-gc-attempts 1 :overhead 0 :supress-jvm-option-warnings true})] (prn (select-keys r [:execution-count :sample-count :samples :mean :lower-q :upper-q :options])))"
```

It exited `0`; the durable raw smoke output is
[`criterium-smoke.stdout`](../../results/phase-0/performance/clj-1.10.1-jdk-26.0.2.1/legacy-original-shape/criterium-smoke.stdout)
with SHA-256 `b4894206a21601582bc5523c2fdc9c722c736de0afafee829c92ab006b820cd2`.
Its exact command, environment, pinned-jar checksums, exit status, and explicit
compatibility-only limitations are recorded in
[`criterium-smoke.meta.edn`](../../results/phase-0/performance/clj-1.10.1-jdk-26.0.2.1/legacy-original-shape/criterium-smoke.meta.edn)
(SHA-256 `fc5f5e9eb4305fb4abb7e40a4fba1996faf94dafcbbe59805b7bc35199b4ba4b`).
The earlier `/private/tmp/xfseq-phase0-criterium-smoke.stdout` path was staging
only and is not relied upon. The bounded smoke retained Criterium raw samples
and uncertainty; it was not substituted for the full snapshot.

`dev/xfseq/phase_0_bench.clj` now contains the exact stable registry: 4
top-level identities, all 13 hand-written Java identities mapped to their
fully-qualified source classes, and 54 generated ASM identities. Each ASM ID
encodes argument type, input type, `identity-stop`/`no-stop`, and
`mixed`/`chunked`/`dechunked` mode, with its exact constructor key and class
name. Constructor smoke used each candidate's declared buffer, reducing
function, and source shape; all 67 Java/ASM constructor rows were `:ok` and
`:unsupported-or-unreachable` was `[]` in every timing report. No hand-written
variant was added to the comparable timing matrix.

The six timing JVMs reused the fresh Slice 2 `javac --release 8` output at
`/private/tmp/xfseq-phase0-final-101.gcNg8H`; representative `javap -verbose`
inspection reports class-file major version `52` (minor `0`). Each process
used OpenJDK 26.0.2.1/Homebrew, macOS 26.2 build 25C56 arm64, G1,
`-Xms2g -Xmx2g`, and Clojure 1.10.1. The canonical command shape was:

```sh
<java> -Xms2g -Xmx2g -XX:+UseG1GC \
  -Dclojure.compiler.direct-linking=<false-or-true> -cp <full-classpath> \
  clojure.main -m xfseq.phase-0-bench \
  --lane <legacy-original-shape-or-historical-on> --fork <1-or-2-or-3> \
  --clojure-version 1.10.1 --direct-linking <false-or-true> \
  --output results/phase-0/performance/clj-1.10.1-jdk-26.0.2.1/<lane>/fork-<n>.edn \
  --stdout-output results/phase-0/performance/clj-1.10.1-jdk-26.0.2.1/<lane>/fork-<n>.stdout \
  --meta-output results/phase-0/performance/clj-1.10.1-jdk-26.0.2.1/<lane>/fork-<n>.meta.edn
```

The fully expanded command, JVM flags, process ID, linking interpretation,
classpath/jar checksums, preservation SHA/tag, Criterium options, uncertainty
fields, implementation/case identity, semantic label, and raw EDN/stdout
checksums are retained in every timing `.meta.edn` file. The six final reports contain
24 successful timing rows each (the three required top-level paths × the
original eight sources, size 10,000, identity/typed-identity transforms, and
the value-discarding full-reduce sink). Criterium was bounded explicitly to
3 samples, 100 ms warmup, 25 ms target execution, 100 bootstrap samples, and
3 GC attempts (versus its 60/10 s/1 s/1000/100 defaults); all raw samples,
means, quantiles, variance, confidence intervals, warmup, and GC fields remain
in the structured reports.

| Lane | Fork | `fork-n.edn` SHA-256 | `fork-n.stdout` SHA-256 | `fork-n.meta.edn` SHA-256 |
|---|---:|---|---|---|
| `legacy-original-shape` | 1 | `a64fd95260a475316414744191d05f4ea4f9c7aad85b7de3ab5453c0db345ef9` | `66e43646560b73ef42399ec2697961dfb9365a2b97478a9dd5681e27103274c2` | `1d59427e902bcedca5770c4d66d9a1d283c3b3eb868b108dd3cb1be955d427e9` |
| `legacy-original-shape` | 2 | `1249745186395067abe31b0fa321823c78fde9965a23d56337d6995f92a38675` | `4e5721a3ae2141cf106d83fe3d5ca93829c7d6e79e4aa04c949ac629d166ae94` | `68e64160f4ec992957621e5b2aaf5bb3a617da7be4eec30c2431a4df4e9d6068` |
| `legacy-original-shape` | 3 | `b6ef5f57af343a1ae8a88adb5fd0bcc01071cdb3b8cddb89cba04182812f9fb3` | `4b99c4560383c78f4fd494749ff2e8bc785ab3d0a90ee4156ffd0e2f002be387` | `d1ae1aa43a5a1836bcbab51b457a2e2f245f3b5ec74de79b3c77ec9b28161a31` |
| `historical-on` | 1 | `33e1029ac049153ff92cbcd6e5f3bda03a04b1ad8ec4f919533a4f8810851235` | `11f70ca3e8795a4e3896b21ad235ee6a079c4f7f16158fc6e280498aa9c4e4fe` | `06496f2cae41827cb1f5ccaaad952b7fdcafb7c7c261539a33dacf24a2854aec` |
| `historical-on` | 2 | `db1d4271c14643470962cb64837941c52d6a718974e5f6e79ea268a73583f746` | `c5d0c389509c27854767eaeef7b9e3db55f94bac9b4968790779af970c7585c0` | `5ee9886725986491af8ec59602341402a2cb3dbeaf915051507748e196e29f9b` |
| `historical-on` | 3 | `9374c40a788359ff2eeacc913a7825355a58209cb7c36e46b8b0e140d3195fd6` | `bffaa855f9be9cb37c10f6964f0c9b5af2bd226bc01b112d6e33a060ba8c768f` | `580d185e87dd95e64072f491c541ffc7a98d9e4ab6976ace42d94c45d8a02048` |

The reports explicitly mark legacy timing rows `:semantically-non-equivalent`.
They also explicitly state: no allocation measurement; no release-equivalent
or upstream-adoption evidence; no overall winner; and no pooled speedup. The
`legacy-original-shape` lane has source caller/candidate direct linking
explicitly false with the released core jar on; `historical-on` sets the
source caller/candidate property true while retaining the released core jar and
labels that configuration `:symmetric-on`; the false lane remains labeled
`:asymmetric-released-core-on-source-off`. No timing conclusion is drawn from
either lane.

### Slice 4 validation evidence and exit-criteria audit (2026-08-31)

Slice 4 inspected the actual Slice 1–3 files and performed the parent
integration checks before handing the run to final review. No correction to
the history note was necessary: its tag identity, 34-file preservation
inventory, checksums, candidate IDs, and limitations agree with the artifacts.

The semantic reports
[`clj-1.10.1-jdk-26.0.2.1.edn`](../../results/phase-0/semantic/clj-1.10.1-jdk-26.0.2.1.edn)
and
[`clj-1.12.5-jdk-26.0.2.1.edn`](../../results/phase-0/semantic/clj-1.12.5-jdk-26.0.2.1.edn)
were parsed afresh. Both contain the unchanged suite result of one test, 46
passing assertions, zero failures, and zero errors. Both retain the confirmed
historical observations: incomplete `XFSeqHead` result surface, repeated
transducer application, missing empty completion, missing `consume`
completion, broken two-stage `drain`, ASM construction source access, and
missing multi-collection `map` arities. The clean declared-classpath child
fails with exit status `1` and missing `xfseq.ILongSeq`, as expected. The
report SHA-256 values remain `2b74bdc387ec6415a1c2c4482d815def0c3295689bc036da5dea1ddfd3af63d4`
and `ee1a06313dad555efc3105efdcd7ea587e734281b055ee1430e0bab7e4e6cf87`.

The six timing reports under
[`results/phase-0/performance/`](../../results/phase-0/performance/) were
parsed afresh. They contain 144 `:ok` rows (24 per fork), six distinct process
IDs, the complete 71-ID registry (4 top-level, 13 Java, 54 ASM), all 67
Java/ASM constructor smokes `:ok`, and no unsupported or unreachable rows.
Every timing EDN and stdout SHA-256 matches its metadata; the durable
[`criterium-smoke.stdout`](../../results/phase-0/performance/clj-1.10.1-jdk-26.0.2.1/legacy-original-shape/criterium-smoke.stdout)
SHA-256 also matches
[`criterium-smoke.meta.edn`](../../results/phase-0/performance/clj-1.10.1-jdk-26.0.2.1/legacy-original-shape/criterium-smoke.meta.edn).
The original-shape metadata is labeled
`:asymmetric-released-core-on-source-off`; all three historical-on metadata
files are labeled `:symmetric-on`. All six processes exited `0` with the
declared JDK, Clojure, Criterium, heap, GC, and linking metadata.

| Exit criterion | Slice 4 result and evidence |
|---|---|
| 1. Immutable preservation reference | Pass: `research-2020-05-10^{commit}` resolves to `168ce02f2dcb796045990fe1647205f4da20c1f5`; `git cat-file -t research-2020-05-10` is `tag`; annotated tag object is `555c01620cce3b1eeb59384008a7d30786e4a427`. |
| 2. Preservation manifest | Pass: history note and plan contain the full SHA, checkout command, API/candidate inventory, parent-design links, tree SHA `d9db3f7db3c8c62b786e2241c648038daa7ba5ac4f7c52e9431f25eb67da882b`, and archive SHA `effaac4ee627ceb17581dc2e6c9c40b12b9e72a46c282d0239e041fdb5befc67`. |
| 3. All preserved paths reachable and unchanged | Pass: `git ls-tree` lists 34 preservation files; the path-limited preservation diff exits `0`. |
| 4. Fresh legacy suite on both Clojure lanes | Pass: both semantic reports contain the exact 1-test/46-assertion/0-failure/0-error summary and full commands/runtime metadata. |
| 5. Direct-oracle failure record | Pass: both reports retain and label every confirmed Phase 0 observation listed above, including the clean-load failure. |
| 6. Stable candidate IDs | Pass: every report has 71 unique IDs and the exact 4/13/54 top-level/Java/ASM registry split; all 67 constructor smokes are `:ok`. |
| 7. Historical smoke and snapshot | Pass: durable Criterium 0.4.5 smoke plus two lanes × three fresh JVM forks; raw EDN/stdout/metadata files, commands, and SHA-256 values are present and verified. |
| 8. Timing interpretation | Pass: reports and plan state historical context only, semantic non-equivalence, no allocation measurement, no release-equivalence or upstream-adoption evidence, and no winner or pooled speedup. |
| 9. Protected paths | Pass: `src/`, `src-java/`, `test/xfseq/`, and `deps.edn` remain byte-for-byte unchanged from the preservation commit. |
| 10. Final run validation | Pass: `git diff --check` and the protected-path check exit `0`; expected changes are limited to the history note, development runners, results, and this plan. |
| 11. Handoff state | Pass: this plan is `Awaiting final review`, the run stage is complete, final review is pending, and no Phase 1 work was started. |

The benchmark remains a historical snapshot only. No allocation, release,
adoption, correctness, or production-loop conclusion is drawn from its timing
rows. The local tag remains unpublished, and no commit or push is part of this
run.

## Plan review findings

Review performed after reading the complete parent design, plan, production
sources, Java variants, tests, benchmark, and relevant history.

### Uncertain decisions

1. **How durable must the tag itself be?** A local annotated tag is readable
   and useful, but a clone will not receive it unless it is published. Copying
   the source or blocking Phase 0 on an external push would add duplication or
   require authority outside this phase. **Recommendation accepted:** the full
   SHA and checksums in the committed manifest are authoritative; create the
   tag locally, record whether it was published, and never claim publication
   unless verified. Evidence needed in the run: exact tag resolution and the
   manifest in the phase commit.
2. **Will Criterium 0.4.5 provide usable output on JDK 26?** Upgrading it would
   stop being a historical reproduction and overlap Phase 1. **Recommendation
   accepted:** perform the one-case compatibility smoke first, retain
   checksummed stdout if structured output is unavailable, and mark the phase
   `Needs replanning` if the benchmark cannot run at all. Evidence needed:
   smoke command, exit status, and raw output.
3. **Should Phase 0 adapt every hand-written Java class into a timing path?** A
   universal adapter could conceal invalid source assumptions or introduce the
   production dispatch that Phase 2 is meant to design. **Recommendation
   accepted:** require an exact stable-ID-to-class registry and constructor
   smoke under each declared shape, but defer comparable timings until all
   object loops share one correct contract in Phase 2. Evidence needed: registry
   coverage and explicit unsupported rows.

### Confident changes

A. Renamed IDs from `reduced` to `identity-stop` or `no-stop`; the old names
   implied semantic correctness the code does not have.
B. Added exact result path stems per Clojure version, linking lane, and process
   fork so reruns cannot overwrite or scatter evidence.
C. Kept semantic and timing runners separate. This avoids making Criterium a
   prerequisite for the semantic record and is simpler than a mode-heavy
   combined tool.
D. Kept the two historical linking lanes, but neither is called a release
   comparison. The original-shape lane is asymmetric; the on lane remains
   non-decisional because candidates are incorrect and allocation is absent.
E. Kept build, JMH, repair, and candidate selection out of Phase 0.

No finding conflicts with repository evidence. The changes clarify names and
artifact durability without materially redesigning the phase, so a second
plan-review pass is not required.

## Pre-implementation review

Gate performed 2026-08-31 against the complete parent design, current source,
Java candidates, tests, benchmark, history, and planning-time probes.

### Findings by severity

**Blockers:** none.

**High-impact constraints already enforced by the plan:**

1. The legacy value suite is not semantic-equivalence evidence. The plan
   requires direct Clojure oracles and retains the known failure matrix beside
   the green legacy summary.
2. The Criterium snapshot is not release-performance evidence. Candidates are
   incorrect, the original-shape lane is linking-asymmetric, and allocation is
   absent. The plan permits raw historical context only and reserves every
   adoption decision for the later JMH matrix.
3. The Java `identity-stop` loops must not be described as correct `Reduced`
   implementations. Stable IDs now encode the actual mechanism, and repairs
   remain Phase 2 work.
4. A local tag alone is not a remotely durable artifact. The committed full
   SHA and checksums are authoritative, and tag publication status must be
   recorded rather than assumed.

**Medium-impact execution risks with decisive checks:**

1. Pinned Criterium compatibility on JDK 26 is unknown. A one-case smoke
   precedes the snapshot; total failure returns the phase to replanning instead
   of silently upgrading the dependency.
2. Some hand-written variants are currently unwired and accept narrower source
   shapes. Phase 0 requires registry coverage and shape-correct constructor
   smoke, not a universal adapter or misleading comparative timing.

### Gate assessment

- **Problem validity:** preservation is a necessary prerequisite to aggressive
  simplification and does not elaborate the production design.
- **Semantic fidelity:** direct Clojure behavior is the named oracle; Phase 0
  records differences and makes no equivalence claim.
- **Performance validity:** competing top-level paths and exact linking/runtime
  lanes are named, while the limits of the historical harness are explicit.
- **Structural simplicity:** one tag, one committed manifest, and two small
  evidence runners are the smallest approach that remains repeatable without
  duplicating the source tree or starting Phase 1.
- **Hot-path quality:** no product or hot-path code changes are permitted.
- **Upstream fitness:** the phase produces an auditable before-state, stable
  candidate names, raw evidence, and a narrow handoff.

Verdict: `ready for implementation`.

### What matters

- Preserve commit `168ce02...`; do not tag current `HEAD` as the 2020 state.
- The old suite is green but semantically incomplete.
- Candidate names describe loop mechanics, not correctness.
- Historical timings are context only; they cannot establish a winner.
- No production, build-modernization, repair, or Phase 1 work belongs here.
- If pinned Criterium cannot run, replan instead of changing dependencies.
- The run must end at `Awaiting final review`.

## Agent run log

| Date | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-08-31 | Plan | `/root` (`gpt-5.6-sol`, high) | Read all required planning skills; inspected the complete parent design, history, source, Java candidates, tests, benchmark, toolchain, and direct Clojure behavior. | Plan reviewed; pre-implementation verdict `ready for implementation`. |
| 2026-08-31 | Run / Slice 1 | `/root/phase0_slice1` | Rechecked the clean worktree and explicit 2020 commit; created the local annotated `research-2020-05-10` tag; verified its target, tag type, remote publication status, preservation tree/archive checksums, complete 34-file inventory, and path-limited zero diff; added the preservation/history manifest. | Slice 1 checkpoint complete. Changed only `docs/history/01-2020-research-state.md` and preservation evidence/log entries in this plan; no production or legacy test/benchmark files changed; tag remains local only. |
| 2026-08-31 | Run / Slice 2 | `/root/phase0_slice2` | Added `dev/xfseq/phase_0_characterize.clj`; compiled the unchanged tracked Java sources into fresh temporary outputs for exact Clojure 1.10.1 and 1.12.5 lanes; ran the legacy suite, direct-oracle characterization, and clean declared-classpath child require. | Slice 2 checkpoint complete. Added only the development runner, the two semantic EDN reports, and Slice 2 plan evidence/decision/log entries. Both lanes exited `0` with 1 test, 46 assertions, 0 failures, 0 errors; expected historical differences are visibly labeled, and the clean-load child exited `1` with `ClassNotFoundException: xfseq.ILongSeq`. |
| 2026-08-31 | Run / Slice 3 | `/root/phase0_slice3` | Added `dev/xfseq/phase_0_bench.clj`; resolved pinned Clojure 1.10.1/Criterium 0.4.5 dependencies to `/private/tmp`; ran the compatibility smoke first; registered and constructor-smoked 54 ASM keys and 13 hand-written Java IDs; ran six fresh JDK 26 JVMs across both linking lanes with the exact 24-case top-level matrix. | Slice 3 checkpoint complete. Added only the development benchmark runner, the two planned lane directories with 20 raw artifacts (18 timing artifacts plus durable smoke stdout/metadata), and Slice 3 plan evidence/decision/log entries. Every process exited `0`; all six reports have 24 `:ok` timing rows, all 71 registry IDs are unique, all 67 Java/ASM constructor smokes are `:ok`, unsupported/unreachable is `[]`, each timing metadata file's raw EDN/stdout checksums match, and the durable smoke metadata records its raw stdout checksum. Timing remains historical context only. |
| 2026-08-31 | Run / Slice 4 | `/root/phase0_slice4` | Parsed both semantic reports and all six timing reports; verified the durable Criterium smoke, raw artifact hashes, preservation tag/checksums, registry and constructor coverage, lane labels, and protected paths; added the explicit exit-criteria audit and handoff state. | Slice 4 complete. No history-note correction was needed; `git diff --check` and protected-path checks exit `0`. The implementation/run stage is complete, the plan is `Awaiting final review`, and final semantic/performance/simplicity review remains pending. |
