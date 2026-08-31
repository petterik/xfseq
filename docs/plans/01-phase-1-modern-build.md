# Implementation #1, Phase 1: modern local build

Status: Ready for implementation

Stage: plan complete; pre-implementation gate passed

Run stage: not started; final review: not started.

Last updated: 2026-08-31

Parent design: [`docs/01-transducer-backed-lazy-seqs.md`](../01-transducer-backed-lazy-seqs.md)

## Plain-English problem

Make this checkout build, lint, and test reliably on the machine we actually
have, without spending time on CI or compatibility matrices before the engine
has proved worthwhile.

The first useful foundation is one trustworthy local loop: Clojure 1.12.5 on
the installed Java 26. If later semantic and performance work looks promising,
we can pay for broader compatibility evidence before publication or upstream
discussion.

## Phase goal

1. Make Clojure 1.12.5 the only project library version.
2. Compile every tracked Java source to `target/classes`, removing the hidden
   IDE-output dependency.
3. Add small, pinned `:build`, `:test`, `:lint`, `:reflection`, `:bench`, and
   optional `:dev` aliases.
4. Provide one local acceptance command,
   `clojure -Srepro -T:build check`, that cleans, builds Java, lints, checks
   compiler reflection output, and runs all tests.
5. Make the checked-in clj-kondo policy return no active-code findings and make
   actual compiler reflection warnings fail.
6. Validate the full command once from a fresh detached worktree on Clojure
   1.12.5 and the installed Java 26.0.2.1.
7. Preserve the Phase 0 behavior record; this phase changes the build, not the
   sequence engine.

## Non-goals

- No CI workflow.
- No Java 17/21/25 matrix, Java 25 publication lane, Clojure prerelease lane,
  or alternate local JDK installation.
- No direct-linking-off build or diagnostic suite. The single exploratory
  benchmark alias uses direct linking on, matching the released Clojure jar.
- No publishable JMH matrix, allocation campaign, or performance conclusion.
- No repair of `XFSeqHead`, completion, `Reduced`, buffers, Java loops,
  `consume`, `drain`, or missing `map` arities.
- No primitive specialization, fusion redesign, or Phase 2 implementation.
- No broad Java warning cleanup. The known serialization warnings in preserved
  research classes are recorded, not pulled into this phase.

Compatibility is deferred, not declared unnecessary. A local result can decide
whether to continue; it cannot support an upstream compatibility claim.

## Why this matters upstream

- **Core maintainer:** later evidence must come from tracked sources, not stale
  IDE classes.
- **Library user:** a green local check must build what is actually checked out.
- **Performance/JVM engineer:** one controlled runtime is enough for early
  direction-finding; a broad matrix is useful only after there is a candidate.
- **Future contributor:** one command and one environment make the early work
  easy to reproduce without introducing a second build system.

## Current repository facts and baseline

- Planning started at `24596c3` on `petter/phase-1-aug-31`; Phase 0 is complete.
- `deps.edn` still uses Clojure 1.10.1, an unqualified Criterium coordinate, and
  the IDE path `classes/production/xfseq`.
- A clean declared classpath cannot require `xfseq.core` because no supported
  command compiles the Java sources.
- The 27 tracked Java files compile to 30 class files with
  `javac --release 8 -Xlint:-options` on the installed JDK.
- `-Xlint:all -Werror` additionally exposes 13 serialization warnings in the
  preserved seq/primitive research classes. Java-warning policy is not needed
  to solve the clean-build problem.
- The installed runtime is Homebrew OpenJDK 26.0.2.1 on arm64. The installed
  Clojure CLI is 1.12.5.1664.
- With temporary Java output, Clojure 1.12.5 and the pinned Cognitect test
  runner pass the unchanged suite: 1 test, 46 assertions, 0 failures, 0 errors.
- Loading all current namespaces with `*warn-on-reflection*` true emits no
  compiler reflection warnings.
- Pinned clj-kondo 2026.05.25 runs on the JVM. Under the chosen active-code
  boundary it currently returns 25 findings: 20 warnings and 5 informational
  findings.
- Two additional clj-kondo errors come from valid dynamic array-class positions
  inside `extend-protocol`; a call-scoped rule for only `:syntax` and
  `:unresolved-symbol` removes those two errors without rewriting runtime code.
- Historical `comment` forms contain stale interactive scratch code. They are
  non-executable and are excluded with `:skip-comments true`.

### Exact Phase 1 runtime

| Component | Version |
|---|---|
| Clojure library | 1.12.5 |
| Clojure CLI | 1.12.5.1664 |
| Java | Homebrew OpenJDK 26.0.2.1, arm64 |
| tools.build | 0.10.14 |
| clj-kondo | 2026.05.25 |
| test-runner | v0.5.1 / `dfb30dd6605cb6c0efc275e1df1736f6e90d4d73` |
| exploratory Criterium | 0.4.6 |

## Options and trade-offs

### Runtime coverage

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| Clojure 1.12.5 + installed Java 26 only | Fastest path to a trustworthy development loop; no environment provisioning | Cannot establish broad compatibility | **Choose now** |
| Java 17/21/25 plus CI | Earlier compatibility knowledge | Infrastructure and repeated work before candidate value is known | Defer until promising |
| Clojure prerelease lane | Early forward signal | Adds another dimension with no current decision value | Reject for now |

### Build shape

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| `tools.build` plus deps aliases | Official, small, explicit Java output, one Clojure dependency model | A small `build.clj` is required | **Choose** |
| Shell-only build | Direct | Duplicates classpath/process behavior and is less portable later | Reject |
| Leiningen, Maven, or Babashka | Full task systems | Adds another tool/build model | Reject |

Compile with `--release 8 -Xlint:-options` into `target/classes`. Do not use
`classes/production/xfseq`, raise the bytecode target, or repair unrelated
serialization warnings.

### Checks

| Option | Advantages | Costs and risks | Decision |
|---|---|---|---|
| One local `check` task | Same path for normal work and fresh-worktree proof | Must propagate every child failure | **Choose** |
| Separate manual commands only | Simple implementation | Easy to skip a required check | Reject as acceptance path; retain debugging tasks |
| CI workflow | Automates remote runs | No current CI need and no additional runtime target | Defer |

Use Cognitect test-runner for test discovery rather than a custom or hard-coded
runner. Use the official `clj-kondo.core/run!` API so every returned finding,
including info severity, fails; the clj-kondo CLI cannot fail on info severity.
Keep a separate compiler reflection check because clj-kondo is not the compiler
oracle.

Selected optional linters are `:warn-on-reflection`, `:unused-alias`,
`:used-underscored-binding`, `:missing-protocol-method-arity`, and
`:unused-value`. Default active-code linters remain enabled. The only
suppression is the call-scoped `extend-protocol` rule; historical `comment`
bodies are skipped. No namespace/file-wide ignore is allowed.

### Benchmark linking

Phase 1 has one `:bench` alias with
`-Dclojure.compiler.direct-linking=true`. That matches the released Clojure jar
and avoids an asymmetric off experiment. It is an exploratory entry point, not
a performance result. Direct-linking-off and the final same-revision core build
are deferred until a correct candidate looks promising.

## Selected approach

1. Upgrade `deps.edn` to Clojure 1.12.5, qualified coordinates, and
   `target/classes`.
2. Add pinned build/test/lint/reflection/dev/bench aliases. Keep `:bench`
   direct-linking-on only.
3. Add a small `build.clj` with `clean`, `javac`, `test`, and `check` tasks.
   `check` runs clean, Java compilation, lint, reflection checking, and tests,
   and fails immediately on a nonzero child result.
4. Add one clj-kondo config, a small all-severity lint entry point, and a small
   compiler-reflection entry point.
5. Fix only active, behavior-neutral lint findings: unused imports/requires,
   metadata/docstring placement, unused bindings, and redundant forms/coercions.
6. Document the local command and record one detached-worktree validation on
   the exact runtime above.

If tools.build cannot propagate child failures correctly on this machine, do
not add a second build path. Return the plan to draft and replace the
orchestration entry point with one minimal script while retaining tools.build
for Java compilation.

## Impact / Effort / Value priorities

| Item | Impact | Effort | Value | Dependency/evidence | Decision |
|---|---|---|---|---|---|
| Clean Java build to `target/classes` | High | Medium | High | Current clean require fails; temporary compile passes | Now |
| Clojure 1.12.5 and qualified dependencies | High | Low | High | User-selected single version | Now |
| One local fail-fast check | High | Medium | High | Depends on build, lint, reflection, tests | Now |
| Zero active clj-kondo findings | High | Medium | High | Current 25-finding baseline is known | Now |
| Compiler reflection gate | High | Low | High | Current compiler probe is clean | Now |
| Direct-linking-on bench alias | Medium | Low | Medium | Matches released core; no timing claim | Now |
| CI and other JDKs | Low now, high later | High | Low now | Candidate value not yet established | Later if promising |
| Direct-linking-off diagnostics | Low now | Medium | Low now | Requires symmetric core build to be meaningful | Later if promising |
| JMH/allocation matrix | High later | High | Low in Phase 1 | Needs a correct candidate | Later phase |
| Engine/API repairs | High later | High | High later | Modern local build first | Phase 2+ |

## Confidence ledger

| ID | Kind | Statement / failure mode | Resolution | Confidence |
|---|---|---|---|---|
| C1 | Fact | The clean declared classpath cannot load `xfseq.core`. | Reproduce once in the detached worktree, then run the supported check successfully. | High; already reproduced. |
| C2 | Fact | All Java compiles to 30 Java 8-compatible classes on JDK 26 with the selected options. | Use the same options through tools.build and inspect output count/location/class version. | High; direct probe passed. |
| C3 | Fact | Clojure 1.12.5/JDK 26 passes the existing 46 assertions. | Make it the only Phase 1 test runtime and repeat after cleanup. | High; direct probe passed. |
| C4 | Fact | Current namespaces emit no compiler reflection warnings under the selected runtime. | Add the compiler-authoritative check and rerun after lint cleanup. | High locally. |
| C5 | Fact | The clj-kondo JVM API returns all 25 active findings as data. | Check in the exact config/API entry point and require an empty findings collection. | High; direct probe passed. |
| C6 | Assumption | tools.build can express the local clean/javac/process chain simply. | Implement the smallest task and verify child failure propagation locally. | High; standard API, not yet implemented. |
| C7 | User decision | One runtime is enough to decide whether to continue. | State clearly that Phase 1 proves only the local environment and creates no compatibility claim. | High; explicitly resolved by user. |
| C8 | Failure mode | Stale classes make broken sources appear green. | `check` cleans first; detached worktree begins without ignored output. | High confidence in mitigation. |
| C9 | Failure mode | A local pass is later cited as broad compatibility. | Record exact runtime on every result and keep compatibility as an explicit later gate. | High confidence in mitigation. |
| C10 | Failure mode | Lint cleanup changes prototype semantics. | Review each active edit and rerun the Phase 0 characterization summary. | High confidence in mitigation. |
| C11 | Failure mode | clj-kondo is mistaken for proof of no reflection. | Keep the compiler check separate and authoritative. | High confidence in mitigation. |
| C12 | Failure mode | An off-linked candidate is compared with the released on-linked core. | Do not create or run an off lane in Phase 1. | High confidence in mitigation. |

No Phase 1 unknown depends on another JVM. Other-JDK behavior is deliberately
unknown and cannot block this local-build phase.

## Decisive experiments

1. **Clean repair:** from a detached worktree with no ignored output, reproduce
   the old missing-class failure, then run
   `clojure -Srepro -T:build check` successfully.
2. **Java output:** require exactly 30 class files under `target/classes`, none
   elsewhere, and class-file major version 52 for representative classes.
3. **Failure propagation:** temporarily make one child command fail; confirm
   `check` exits nonzero, then remove the temporary change.
4. **Lint:** require the pinned API entry point to return zero findings over
   `src`, `dev`, `test`, `build.clj`, and `deps.edn`.
5. **Reflection:** require zero compiler reflection warnings; temporarily add a
   known reflective form to prove the check fails, then remove it.
6. **Tests:** require 1 test, 46 assertions, 0 failures, and 0 errors on the
   exact local runtime.
7. **No semantic drift:** rerun the Phase 0 characterization summary and retain
   all known historical difference labels.
8. **Linking identity:** non-timed `:bench` smoke must report direct linking
   true. No off mode or timing is run.

## Evidence and ownership

Expected durable evidence is intentionally small:

```text
results/phase-1/validation/fresh-worktree-check.stdout
```

The active plan records the commit, command, Clojure library/CLI versions,
exact Java runtime/vendor, OS/architecture, output class count/version,
direct-linking property, exit status, and Phase 0 characterization summary. The
raw log must not contain credentials or dependency-cache contents.

| Area | Ownership |
|---|---|
| `deps.edn`, initial `build.clj` | Dependencies, aliases, Java build, tests, direct-on bench entry |
| `.clj-kondo/config.edn`, check namespaces, active Clojure cleanup | Lint and compiler reflection gates |
| `README.md`, `results/phase-1/`, this plan | Local command, evidence, decisions, run state |

No slice owns `src-java/` semantics or Phase 2 code.

## Ordered implementation slices

Workers run sequentially. The parent validates, commits, and records each slice
SHA before starting the next worker.

### Slice 1: local build and tests

Ownership: `deps.edn`, initial `build.clj`, and Slice 1 plan entries.

1. Upgrade dependencies and replace the IDE classpath with `target/classes`.
2. Add pinned build, test, bench, and dev aliases; bench is direct-linking-on.
3. Implement clean and Java compilation with the selected Java 8 options.
4. Add discovered test execution and fatal child-process handling.
5. Validate clean/javac/test locally; inspect the exact 30-class output.

Parent check: remove ignored output, independently run clean/javac/test, inspect
the class files, verify failure propagation, and confirm no semantic source or
Java change.

### Slice 2: quality gates and local proof

Ownership: clj-kondo config, lint/reflection entry points, minimal active
Clojure cleanup, final `check` integration in `build.clj`, README build section,
Phase 1 validation results, and remaining plan entries.

1. Add the selected all-severity lint policy and compiler reflection check.
2. Make behavior-neutral active-code fixes; add no broad ignores.
3. Complete `clojure -Srepro -T:build check`.
4. Rerun tests and Phase 0 characterization.
5. Run the command once in a fresh detached worktree on the exact local
   runtime; retain the raw log above and record its result here.
6. Audit the exit criteria and mark the plan `Awaiting final review`. Do not
   add CI, another runtime, off linking, or Phase 2 work.

Parent check: inspect every Clojure diff, independently run lint/reflection and
the full fresh-worktree check, parse the evidence, run `git diff --check`, and
confirm scope.

## Semantic validation

Phase 1 claims build reproducibility, not semantic equivalence.

- The legacy suite remains a build smoke: 1 test and 46 assertions.
- Phase 0 characterization must retain its known failure labels.
- Lint edits must be limited to imports/requires, metadata/docstrings, unused
  bindings, and provably redundant forms/coercions.
- If a lint finding requires a semantic rewrite, leave it unresolved and return
  the plan to draft rather than changing engine behavior here.
- Direct Clojure behavior remains the later semantic oracle.

## Performance and direct-linking methodology

There is no performance acceptance rule in Phase 1 because no performance
claim is made.

The only Phase 1 lane is:

| Clojure | Java | Linking | Purpose |
|---|---|---|---|
| 1.12.5 | OpenJDK 26.0.2.1 | released core on; exploratory caller/candidate on | Local build and direction-finding only |

If the later candidate is correct and locally promising, the project must
define broader JDK coverage, forked JMH/allocation evidence, and same-revision
direct-linking-on builds before an upstream performance claim. Off-linking
diagnostics remain separate and optional until then.

## Exit criteria

1. `deps.edn` uses Clojure 1.12.5, qualified coordinates, and no IDE output path.
2. `clojure -Srepro -T:build check` succeeds from a fresh detached worktree on
   OpenJDK 26.0.2.1.
3. Exactly 30 Java 8-compatible class files are produced only under
   `target/classes`.
4. The discovered suite passes 1 test/46 assertions/0 failures/0 errors.
5. clj-kondo 2026.05.25 returns zero findings over all selected active paths,
   with no namespace/file-wide ignore.
6. The compiler-authoritative check emits no reflection warning.
7. The `:bench` alias is direct-linking-on and no off lane or timing claim is
   introduced.
8. Phase 0 characterization shows no semantic drift.
9. README and this plan record the one command and exact runtime; the single
   fresh-worktree raw log is retained.
10. `git diff --check` passes; no CI, compatibility matrix, Java-loop repair,
    or later-phase code appears in the diff.
11. Commands, pins, result paths, decisions, slice SHAs, and agent runs are
    recorded here.
12. The run ends at `Awaiting final review`; Phase 2 does not start.

## Decision log

| Date | Decision | Reason |
|---|---|---|
| 2026-08-31 | Use only Clojure 1.12.5 and installed OpenJDK 26.0.2.1 in early phases. | User chose fast local proof before compatibility investment. |
| 2026-08-31 | Do not add CI in Phase 1. | There is one machine/runtime and no candidate yet worth a remote matrix. |
| 2026-08-31 | Rename the acceptance task from `ci` to `check`. | It is a local quality command, not a CI workflow. |
| 2026-08-31 | Defer Java 17/21/25, Java 25 publication data, and Clojure prereleases. | Compatibility becomes worthwhile only if the design is promising. |
| 2026-08-31 | Keep only direct-linking-on for exploratory benchmarks. | It matches the released core jar and avoids an asymmetric diagnostic lane. |
| 2026-08-31 | Use tools.build 0.10.14 and `target/classes`. | Small official build layer; no IDE output or second build system. |
| 2026-08-31 | Compile with `--release 8 -Xlint:-options`. | Preserves Clojure-compatible bytecode without importing serialization cleanup. |
| 2026-08-31 | Use pinned test-runner and clj-kondo JVM API. | Standard discovery and a real all-severity lint gate. |
| 2026-08-31 | Compiler output remains the reflection authority. | Static lint cannot prove emitted code is reflection-free. |
| 2026-08-31 | Do not add JMH in Phase 1. | A benchmark harness has value only after a correct candidate exists. |

## Planning validation evidence

The earlier planning probes already used the selected local runtime:

| Check | Result |
|---|---|
| Clojure CLI | 1.12.5.1664 |
| Java | Homebrew OpenJDK/javac 26.0.2.1, arm64 |
| Java compile | 27 sources to 30 classes with Java 8 target |
| Tests on Clojure 1.12.5 | 1 test, 46 assertions, 0 failures/errors |
| Compiler reflection | No reflection warnings while loading current namespaces |
| clj-kondo API | 25 active findings returned: 20 warning, 5 info |
| Narrow macro rule | Removes only the two dynamic `extend-protocol` errors |
| Strict javac diagnostic | 13 serialization warnings, explicitly outside Phase 1 |

Temporary probe classes lived under `/private/tmp/xfseq-phase1-plan.nomZ3I`.
They are feasibility evidence, not completion evidence or performance data.

## Plan review findings

### Review 1: scope and redundancy

Verdict: revise, then review again.

1. The original Phase 1 plan spent effort on CI, Java 17/21/25, a Java 25
   publication lane, a Clojure prerelease lane, and direct-linking-off. Those
   do not answer the current decision: whether the implementation is worth
   pursuing on the available machine. They were removed and compatibility was
   made an explicit later upstream-readiness gate.
2. The first simplified draft still required both a normal final check and a
   detached-worktree check. Development already exercises the tasks; one fresh
   detached-worktree acceptance run proves the clean-checkout property. The
   duplicate final run was removed.
3. The draft proposed three Phase 1 evidence files. Only the raw fresh-worktree
   log needs its own file. Commands, versions, exit status, class facts, and the
   Phase 0 summary belong in this durable plan, so the two generated metadata
   files were removed.
4. The lint and compiler-reflection checks are not redundant: one is static
   policy and the other is the compiler oracle. Both remain, with one narrow
   macro rule and no broad suppression.

### Review 2: post-revision check

Verdict: pass with no unresolved finding.

- The phase has one exact environment: Clojure 1.12.5 on installed OpenJDK
  26.0.2.1.
- It has one acceptance path and one raw acceptance artifact.
- The two sequential slices have distinct ownership: establish the clean build
  first, then add quality gates and produce the fresh-worktree proof.
- No semantic engine work, compatibility matrix, CI, off-linked lane, JMH
  campaign, or performance claim remains in scope.
- The plan preserves the Phase 0 behavior record and makes the limits of local
  evidence explicit. Nothing in Phase 1 can be cited as cross-JDK support.

## Pre-implementation review

Verdict: **PASS**.

| Gate | Result |
|---|---|
| Semantic fidelity | Pass. No seq-engine behavior is intentionally changed; direct Clojure remains the later oracle, and the Phase 0 characterization must not drift. |
| Build correctness | Pass. Tracked Java sources, clean output, discovered tests, fatal child failures, lint, and compiler reflection checks are all covered by one command. |
| Performance validity | Pass for this phase's claim. There is no timing or speed claim; the bench alias only proves a direct-linking-on exploratory entry point. |
| Simplicity | Pass. One Clojure version, one installed JDK, no CI, one build model, one check command, two slices, and one raw log. |
| Maintainability | Pass. Dependencies are pinned, suppression is call-scoped, IDE output is removed, and no alternate build path or generated machinery is introduced. |
| Upstream credibility | Pass as foundation work only. Broader compatibility and release-equivalent forked performance evidence remain mandatory later if the candidate is promising. |

Implementation must return this plan to `Draft` instead of improvising if the
one-command build requires a second orchestration path, a lint fix changes seq
semantics, the Phase 0 characterization drifts, or the local Java/test baseline
cannot be reproduced.

## Agent run log

| Date | Stage | Agent | Work | Result |
|---|---|---|---|---|
| 2026-08-31 | Initial plan | `/root` (`gpt-5.6-sol`, high) | Planned CI and Java 17/21/25 compatibility plus local Java 26. | Superseded as premature infrastructure. |
| 2026-08-31 | Replan | `/root` (`gpt-5.6-sol`, high) | Applied the user's one-runtime decision; simplified the parent design and Phase 1 plan to local Clojure 1.12.5/Java 26, one direct-on lane, two slices, and no CI. | Revised draft. |
| 2026-08-31 | Plan review 1 | `/root` (`gpt-5.6-sol`, high) | Applied confidence, prioritization, and review-plan checks to the revised scope. | Removed duplicate final validation and two unnecessary metadata artifacts. |
| 2026-08-31 | Plan review 2 | `/root` (`gpt-5.6-sol`, high) | Rechecked the material redesign for scope, evidence, ordering, and unresolved assumptions. | Pass; no unresolved finding. |
| 2026-08-31 | Pre-implementation review | `/root` (`gpt-5.6-sol`, high) | Applied the strict semantic, build, performance, simplicity, maintainability, and upstream gate. | Pass; ready for Phase 1 run only. |
