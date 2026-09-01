# Phase 2 JMH benchmark harness

The Phase 2 harness is isolated under `bench/java` and `bench/clj`.  It is
not on the normal library or test classpath.  The `:bench` alias pins JMH
1.37, and `bench-jar` produces a self-contained jar at
`target/bench/xfseq-phase2-jmh.jar` with the production Java classes and AOT
Clojure callers.

Run the required smoke, including the semantic gates, with:

```sh
clojure -Srepro -T:build bench-smoke
```

Every durable path is reserved with `CREATE_NEW`.  The canonical first run
uses `smoke-<git-commit>.json` and `environment.edn`; every subsequent receipt
must provide an explicit safe suffix, for example:

```sh
clojure -Srepro -T:build bench-smoke '{:run-id "followup-20260901"}'
```

That produces `smoke-<git-commit>-followup-20260901.json` and the matching
`environment-<git-commit>-followup-20260901.edn` without replacing an earlier
receipt.  The smoke merge uses the profile-specific `merge-smoke` command:
JMH may report the string `"NaN"` for a two-sample confidence error, while
the generic `merge` and `validate` commands remain strict for screen/decision
inputs.  The benchmark-only registry test covers both sides of this
boundary.

The task runs `check` first, builds the isolated jar, verifies representative
caller bytecode, then runs three small JMH groups:

* `Phase2PublicBenchmark` compares `xfseq`, `sequence`, `eduction`, and
  `transduce` through separate `construct`, `first`, `prefix8`, `traverse`,
  `vector`, and `reduce` methods.
* `Phase2JavaBenchmark` calls the selected repaired Java candidate directly.
  Candidate selection, source-shape validation, and an expected implementation
  class-identity check happen in JMH setup; the timed methods only invoke the
  candidate and consume a checksum.
* `Phase2BufferBenchmark` isolates `ObjectBuffer` append/flush behavior with
  fixture values created in setup.

The smoke uses one fork, two 100-ms warmups, two 100-ms measurements, list
input of size 8, identity workload, and two distinct reduced-aware candidate
IDs.  It is an identity/output check, not a performance decision.  The screen
and decision validation profiles are defined in the benchmark registry; the
tools.build task mirrors their execution fields in its isolated build
namespace. Slice 4 runs the checked-in applicable subsets described below; the
manifests intentionally do not claim to cover the full Cartesian product.

Run them with an explicit suffix so every raw receipt remains immutable:

```sh
clojure -Srepro -T:build bench-screen '{:run-id "screen-YYYYMMDD"}'
clojure -Srepro -T:build bench-decision '{:run-id "decision-YYYYMMDD"}'
clojure -Srepro -T:build bench-decision-gc '{:run-id "decision-gc-YYYYMMDD"}'
```

`bench-screen` uses two forks and three one-second warmup/measurement
iterations. `bench-decision` uses three fresh forks, five one-second
warmup/measurement iterations, `-Xms2g -Xmx2g -XX:+UseG1GC`, and direct linking
on. `bench-decision-gc` repeats exactly the decision manifest and JVM settings
with JMH's separate `-prof gc` profiler; its throughput and allocation metrics
are not combined with the unprofiled run.

The checked-in `bench/manifests/phase2-screen.edn` contains 25 explicit cells
(98 expanded identities), covering public, Java candidate, boundary, and
buffer-policy probes. `bench/manifests/phase2-decision.edn` contains 24
explicit cells (93 identities) selected from the screen for selection-critical
and every apparent-reversal follow-up. Each manifest is validated before any fork,
and merge/validation requires the exact identity set. Java source-specialized
IDs appear only for their declared list/vector shape; no-reduced IDs remain
adapter-owned benchmark rows. `Phase2BufferBenchmark`'s `all-chunk` policy is
benchmark-only and is never silently substituted into production.

The `bench-jit` task reruns five representative direct-on Java cells with one
fork and captures raw `PrintCompilation`/`PrintInlining` output under
`results/phase-2/jit/`. It covers the small list identity apparent reversal,
the selected list traversal, the vector chunked boundary reversal, the added
vector/33 filter-first reversal, and the vector map no-reduced reversal. JIT
output is evidence for the structural decision, not a replacement for forked
timing or GC data.

`xfseq.bench.calls` is compiled AOT with direct linking enabled.  Java calls
the generated function classes' `invokeStatic` methods rather than generated
`gen-class` Var-forwarding methods.  `bench-linkage` disassembles the public
and candidate caller classes and rejects a Var lookup or a missing direct link
to `xfseq.core/xf-seq` and the candidate adapter.  Namespace initialization is
performed once in benchmark support, before any timed method.

The complete parameter vocabulary is in
`xfseq.bench.registry/parameter-registry`: seven source kinds, the required
size boundaries through 1,000,000, six workloads, six sinks, four public
implementations, and all seven repaired candidate IDs.  Specialized Java
candidates are only valid for their declared source shape.  No-reduced rows
must use one of the adapter-owned structurally non-reducing operations; an
arbitrary caller transducer is not accepted.

Each smoke group writes a temporary JMH JSON file under `/private/tmp`, then
the runner merges the arrays into the selected durable paths:

```text
results/phase-2/bench/smoke-<git-commit>[-<run-id>].json
results/phase-2/environment[-<git-commit>-<run-id>].edn
```

The runner validates required JMH fields, benchmark-group identities, and the
two candidate IDs.  Durable paths use `CREATE_NEW`/existence checks and refuse
to overwrite an earlier result.  Smoke validation allows only the known
two-sample `"NaN"` score error; ordinary validation rejects it before a
strict merge writes a durable artifact.  The environment records the exact
command vectors, commit/dirty state, SHA-256 of the tracked dirty diff,
per-file SHA-256s plus a manifest hash for the benchmark source tree,
Java/OS/GC/heap details, JMH jar hash, result hash, and full parameter
registry.  `bench-validate '{:run-id "..."}'` reruns validation without
rewriting either artifact.

The runner shuts down Clojure agent pools in a `finally` block at the CLI
boundary.  This matters because `clojure.java.shell/sh` uses agent-backed
stream readers: otherwise a command can print its result and still keep the
short-lived runner process alive.  The build smoke therefore waits for a
promptly terminating runner after merge, validation, and environment capture.

The direct-on local lane is the only Phase 2 lane.  It uses the released
Clojure 1.12.5 library, the installed CLI, and the local OpenJDK runtime.  No
direct-linking-off, alternate-JDK, CI, or production-selection claim is made
from this smoke.  The same restriction applies to screen, decision, GC, and
JIT receipts: they are local evidence for the Phase 2 loop/buffer decision and
do not establish a Phase 3 direct-core adoption claim.
