# xfseq

A Clojure library designed to ... well, that part is up to you.

## Usage

FIXME

## Local build

The project currently targets the installed Java 26 runtime and Clojure 1.12.5.
Run the complete local build, lint, compiler-reflection check, and test suite
with:

```sh
clojure -Srepro -T:build check
```

The build compiles Java 8-compatible classes into `target/classes`. This is a
local development command; broader Java and Clojure compatibility is deferred
until the implementation proves promising.

The isolated Phase 2 JMH 1.37 smoke also runs the semantic gates, verifies
direct-linked AOT callers, and writes non-overwriting evidence under
`results/phase-2/`:

```sh
clojure -Srepro -T:build bench-smoke
```

Use an explicit run ID for another receipt at the same commit, for example
`clojure -Srepro -T:build bench-smoke '{:run-id "followup-20260901"}'`.

Run the checked-in Phase 2 screen and direct-on decision subset with an
explicit run ID. The GC lane is separate so allocation metrics never stand in
for throughput:

```sh
clojure -Srepro -T:build bench-screen '{:run-id "screen-YYYYMMDD"}'
clojure -Srepro -T:build bench-decision '{:run-id "decision-YYYYMMDD"}'
clojure -Srepro -T:build bench-decision-gc '{:run-id "decision-gc-YYYYMMDD"}'
clojure -Srepro -T:build bench-jit '{:run-id "jit-YYYYMMDD"}'
```

These tasks validate the semantic/build/linkage gates first, run only the
applicable cells listed in `bench/manifests/phase2-{screen,decision}.edn`,
and reserve non-overwriting JSON/EDN evidence under `results/phase-2/`.

See [`docs/phase-2-jmh.md`](docs/phase-2-jmh.md) for the parameter registry,
candidate applicability rules, and result-validation details.

## License

Copyright © 2020 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
