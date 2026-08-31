# Implementation #1, Phase 0: preserve and characterize

Status: Ready for implementation

Stage: planned

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
