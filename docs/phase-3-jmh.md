# Phase 3 direct-unary JMH harness

Phase 3 uses a separate JMH jar, manifest vocabulary, and result directory so
its direct-core compatibility measurements cannot overwrite the accepted
Phase 2 receipts. Build and linkage are isolated under `target/bench/`:

```sh
clojure -Srepro -T:build phase3-bench-jar
```

The AOT caller namespace has one direct wrapper for each core and candidate
unary function. `phase3-bench-linkage` disassembles those wrappers and rejects
Var lookup; the required calls are `clojure.core/map`, `filter`, `remove`, and
`take`, plus their `xfseq.core` counterparts. Generic controls use setup
transducers, while direct candidate-owned xform construction remains in the
timed public call. `Phase3UnaryBenchmark` selects one implementation/operation
construction plan and one sink-specific plan during `@Setup`; timed methods
invoke those plans directly. The transduce implementation is terminal: its
first/prefix/checksum/vector/reduction plans call direct-linked transduce
wrappers and never build an intermediate vector. It has no honest
`construct`-sink identity, which the registry rejects. The linkage gate
disassembles every AOT helper used by timed sinks, including first, prefix,
checksum, vector, reduction, and the direct transduce wrappers.

`Phase3UnaryBenchmark` compares:

- direct `clojure.core` unary functions;
- the public `xfseq.core` unary candidate;
- generic `xf-seq`, `sequence`, `eduction`, and direct `transduce` controls.

Each invocation receives a fresh source, including a fresh Java iterator
adapter. The `reduceUnretained` method covers every implementation, while
`reduceRetained` stores a newly constructed lazy head in benchmark state
before reduction and releases it only from `@TearDown(Level.Invocation)`.
Direct transduce has no head, so its retained-reduction identities are
inapplicable and are rejected by the registry.

Run the non-timed fresh-fixture trial first (or let the smoke invoke it). It
constructs or directly terminally reduces all six implementations for all
four operations on fresh list, vector, and iterator fixtures. It checks each
applicable sink (420 sink checks) for exact first/prefix/checksum, vector, and
reduction values against direct core; transduce uses its sink-specific terminal
wrapper, while its retained-head row is inapplicable. It then runs a separate
focused correctness lane (339 fresh workload/source cases) covering map identity/arithmetic/heavy,
filter/remove output selectivities, take counts, map-entry collections,
terminating repeat/iterate, and every applicable repaired reduced-aware/no-
reduced Java candidate. Filter/remove selectivity labels always mean output
percentage. Those fixtures are independent of every subsequent measured
one-shot source. The required tiny smoke then uses one fork, two 100-ms
warmups, and two 100-ms measurements, validates all six implementations across
all four list-first operations, and runs fresh iterator retained/unretained
reduction rows:

```sh
clojure -Srepro -T:build phase3-bench-trial
clojure -Srepro -T:build phase3-bench-smoke '{:run-id "smoke-YYYYMMDD"}'
clojure -Srepro -T:build phase3-bench-validate '{:run-id "smoke-YYYYMMDD"}'
```

The timing profiles are available after the semantic and linkage gates:

```sh
clojure -Srepro -T:build phase3-bench-screen '{:run-id "screen-YYYYMMDD"}'
clojure -Srepro -T:build phase3-bench-decision '{:run-id "decision-YYYYMMDD"}'
clojure -Srepro -T:build phase3-bench-decision-gc '{:run-id "decision-gc-YYYYMMDD"}'
```

The broader Slice 4 diagnosis lane is separate from the compact primary
manifests. It exposes the plan's workload/selectivity/take-count vocabulary,
map-entry and terminating infinite-source fixtures, and applicable repaired
Java candidates without changing the primary four-key manifest contract:

```sh
clojure -Srepro -T:build phase3-bench-focused-screen '{:run-id "focused-YYYYMMDD"}'
clojure -Srepro -T:build phase3-bench-focused '{:profile "decision" :manifest-file "bench/manifests/phase3-focused-decision.edn" :run-id "focused-decision-YYYYMMDD"}'
```

Focused manifests must use the six keys `implementation`, `operation`,
`sourceKind`, `size`, `workload`, and `takeCount`. Registry validation rejects
invalid workload/operation pairs, unsupported candidate source shapes,
non-reducing candidates for `take`, and non-terminating full sinks on
`repeat`/`iterate`.

`phase3-screen.edn` and `phase3-decision.edn` are explicit applicable-cell
subsets. Each operation has dechunked/chunked list/vector, small/boundary
sizes (8/32), steady-state size 1,000, partial/full sinks, and retained and
unretained reductions. The decision-GC task repeats the decision identities
with JMH's separate `gc` profiler; its allocation metric is never merged into
throughput. All child JSON is merged only after strict identity/metric
validation, and durable paths use existence checks plus `CREATE_NEW`.
Environment EDN records the exact commands, source-tree hashes, jar/result
hashes, direct-linking mode, runtime, JVM, OS, heap, and GC metadata.

Slice 3 adds the harness and smoke only. It makes no speed or adoption claim;
screen/decision/GC evidence belongs to the later measurement slice.
