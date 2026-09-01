(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]))

(set! *warn-on-reflection* true)

(def class-dir "target/classes")
(def basis (delay (b/create-basis {:project "deps.edn"})))

(def bench-class-dir "target/bench/classes")
(def bench-jar-path "target/bench/xfseq-phase2-jmh.jar")
(def phase3-bench-jar-path "target/bench/xfseq-phase3-jmh.jar")
(def phase2-manifest-dir "bench/manifests")
(def phase3-manifest-dir "bench/manifests")
(def bench-basis
  (delay (b/create-basis {:project "deps.edn"
                          :aliases [:bench]})))
(def bench-namespaces
  '[xfseq.core
    xfseq.phase-2-candidates
    xfseq.bench.calls
    xfseq.bench.registry
    xfseq.bench.runner])

(defn clean [_]
  (b/delete {:path "target"}))

(defn javac [_]
  (b/javac {:basis @basis
            :src-dirs ["src-java"]
            :class-dir class-dir
            :javac-opts ["--release" "8" "-Xlint:-options"]}))

(defn process! [command-args]
  (let [{:keys [exit]} (b/process {:command-args command-args
                                    :out :inherit
                                    :err :inherit})]
    (when-not (zero? exit)
      (throw (ex-info "Child process failed"
                      {:command-args command-args
                       :exit exit})))))

(defn test [_]
  (process! ["clojure" "-Srepro" "-M:test"]))

(defn lint [_]
  (process! ["clojure" "-Srepro" "-M:lint" "-m" "xfseq.phase-1-lint"]))

(defn reflection [_]
  (process! ["clojure" "-Srepro" "-M:reflection" "-m"
             "xfseq.phase-1-reflection"]))

(defn check [_]
  (clean nil)
  (javac nil)
  (lint nil)
  (reflection nil)
  (test nil))

(defn- git-commit []
  (let [{:keys [exit out]} (shell/sh "git" "rev-parse" "HEAD")]
    (when-not (zero? exit)
      (throw (ex-info "Unable to identify benchmark commit" {:exit exit})))
    (str/trim out)))

(defn- ensure-absent! [path]
  (when (.exists ^java.io.File (io/file path))
    (throw (ex-info "Refusing to overwrite benchmark artifact"
                    {:path (str path)})))
  path)

(defn- artifact-id
  [commit run-id]
  (let [suffix (some-> run-id str str/trim)]
    (when (and (seq suffix)
               (not (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]*" suffix)))
      (throw (ex-info "Smoke run-id must be a safe artifact suffix"
                      {:run-id run-id})))
    (if (seq suffix)
      (str commit "-" suffix)
      commit)))

(def smoke-artifact-id artifact-id)

(defn- capture-process! [command-args]
  (let [{:keys [exit out err]}
        (b/process {:command-args command-args
                    :out :capture
                    :err :capture})]
    (when-not (zero? exit)
      (throw (ex-info "Child process failed"
                      {:command-args command-args
                       :exit exit
                       :out out
                       :err err})))
    out))

(defn bench-aot
  "Compile the benchmark callers and JMH classes into an isolated output."
  [_]
  ;; Keep the normal production classes available to the isolated benchmark
  ;; classpath, but never put benchmark output in target/classes.
  (javac nil)
  (b/delete {:path bench-class-dir})
  (b/copy-dir {:src-dirs [class-dir]
               :target-dir bench-class-dir})
  (b/compile-clj {:basis @bench-basis
                  :class-dir bench-class-dir
                  :src-dirs ["src" "test" "bench/clj"]
                  :ns-compile bench-namespaces
                  :compile-opts {:direct-linking true}})
  ;; The JMH annotation processor is supplied by the pinned
  ;; jmh-generator-annprocess dependency in :bench.
  (b/javac {:basis @bench-basis
            :src-dirs ["bench/java"]
            :class-dir bench-class-dir
            :javac-opts ["--release" "8"
                         "-Xlint:-options"
                         "-processorpath"
                         (str/join java.io.File/pathSeparator
                                   (filter #(.endsWith ^String % ".jar")
                                           (:classpath-roots @bench-basis)))
                         "-processor"
                         "org.openjdk.jmh.generators.BenchmarkProcessor"]})
  (println "AOT benchmark classes written to" bench-class-dir))

(defn bench-linkage
  "Disassemble AOT callers and reject Var-based timed-call boundaries."
  [_]
  (bench-aot nil)
  (let [classes ["xfseq.bench.calls$_publicXfSeq"
                 "xfseq.bench.calls$_publicSequence"
                 "xfseq.bench.calls$_candidate"]
        output (capture-process!
                 (into ["javap" "-classpath" bench-class-dir "-c"] classes))
        linkage-file (str "target/bench/linkage-" (git-commit) ".txt")]
    (b/write-file {:path linkage-file :string output})
    (when (re-find #"clojure/lang/Var|Var\.intern|Var\.get" output)
      (throw (ex-info "AOT benchmark caller contains a Var lookup"
                      {:classes classes :linkage-file linkage-file})))
    (doseq [required ["xfseq/core$xf_seq.invokeStatic"
                      "xfseq/phase_2_candidates$instantiate_candidate.invokeStatic"]]
      (when-not (str/includes? output required)
        (throw (ex-info "AOT benchmark caller did not direct-link required call"
                        {:required required
                         :linkage-file linkage-file}))))
    (println "Direct-linking verification written to" linkage-file)
    linkage-file))

(def phase3-linkage-classes
  ["xfseq.bench.calls$_coreMap"
   "xfseq.bench.calls$_coreFilter"
   "xfseq.bench.calls$_coreRemove"
   "xfseq.bench.calls$_coreTake"
   "xfseq.bench.calls$_focusedCoreMap"
   "xfseq.bench.calls$_focusedCoreFilter"
   "xfseq.bench.calls$_focusedCoreRemove"
   "xfseq.bench.calls$_focusedCoreTake"
   "xfseq.bench.calls$_candidateMap"
   "xfseq.bench.calls$_candidateFilter"
   "xfseq.bench.calls$_candidateRemove"
   "xfseq.bench.calls$_candidateTake"
   "xfseq.bench.calls$_focusedCandidateMap"
   "xfseq.bench.calls$_focusedCandidateFilter"
   "xfseq.bench.calls$_focusedCandidateRemove"
   "xfseq.bench.calls$_focusedCandidateTake"
   "xfseq.bench.calls$_candidate"
   "xfseq.bench.calls$_publicXfSeq"
   "xfseq.bench.calls$_publicSequence"
   "xfseq.bench.calls$_publicEduction"
   ;; These callers are reached from the timed sink methods rather than from
   ;; construction, so include them in the same no-Var disassembly gate.
   "xfseq.bench.calls$_firstValue"
   "xfseq.bench.calls$_prefixChecksum"
   "xfseq.bench.calls$_checksum"
   "xfseq.bench.calls$_vectorValue"
   "xfseq.bench.calls$_reduceChecksum"
   "xfseq.bench.calls$_transduceFirst"
   "xfseq.bench.calls$_transducePrefixChecksum"
   "xfseq.bench.calls$_transduceChecksum"
   "xfseq.bench.calls$_transduceVector"])

(defn phase3-bench-linkage
  "Disassemble every Phase 3 AOT wrapper and verify direct unary links."
  [_]
  (bench-aot nil)
  (let [classes phase3-linkage-classes
        output (capture-process!
                 (into ["javap" "-classpath" bench-class-dir "-c"] classes))
        linkage-file (str "target/bench/phase3-linkage-" (git-commit) ".txt")]
    (b/write-file {:path linkage-file :string output})
    (when (re-find #"clojure/lang/Var|Var\.intern|Var\.get" output)
      (throw (ex-info "Phase 3 AOT caller contains a Var lookup"
                      {:classes classes :linkage-file linkage-file})))
    (doseq [required ["clojure/core$map.invokeStatic"
                     "clojure/core$filter.invokeStatic"
                     "clojure/core$remove.invokeStatic"
                     "clojure/core$take.invokeStatic"
                     "xfseq/core$map.invokeStatic"
                     "xfseq/core$filter.invokeStatic"
                     "xfseq/core$remove.invokeStatic"
                     "xfseq/core$take.invokeStatic"]]
      (when-not (str/includes? output required)
        (throw (ex-info "Phase 3 caller did not direct-link required unary call"
                        {:required required
                         :linkage-file linkage-file}))))
    (println "Phase 3 direct-linking verification written to" linkage-file)
    linkage-file))

(defn bench-jar
  "Build the self-contained JMH 1.37 benchmark jar."
  [_]
  (bench-aot nil)
  (bench-linkage nil)
  (b/uber {:class-dir bench-class-dir
           :uber-file bench-jar-path
           :basis @bench-basis
           :main 'org.openjdk.jmh.Main})
  (println "Standalone benchmark jar written to" bench-jar-path))

(defn phase3-bench-aot
  "Compile the isolated benchmark classes including the Phase 3 harness."
  [_]
  (bench-aot nil))

(defn phase3-bench-jar
  "Build the standalone Phase 3 JMH jar without replacing Phase 2 output."
  [_]
  (phase3-bench-aot nil)
  (phase3-bench-linkage nil)
  (b/uber {:class-dir bench-class-dir
           :uber-file phase3-bench-jar-path
           :basis @bench-basis
           :main 'org.openjdk.jmh.Main})
  (println "Standalone Phase 3 benchmark jar written to" phase3-bench-jar-path))

(defn- temp-directory []
  (str (java.nio.file.Files/createTempDirectory
         (java.nio.file.Paths/get "/private/tmp" (make-array String 0))
         "xfseq-phase2-smoke-"
         (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- smoke-command
  [temporary-output benchmark-class params]
  (into ["java" "-jar" bench-jar-path
         "-f" "1"
         "-wi" "2"
         "-w" "100ms"
         "-i" "2"
         "-r" "100ms"
         "-rf" "json"
         "-rff" temporary-output]
        (concat (mapcat (fn [[name values]]
                          ["-p" (str name "=" (str/join "," values))])
                        params)
                [benchmark-class])))

(defn- run-bench-runner!
  [args]
  (capture-process!
    (into ["clojure" "-Srepro" "-M:bench" "-m"
          "xfseq.bench.runner"]
          args)))

(def phase2-profile-options
  {:screen {:forks 2
            :warmups 3
            :measurements 3
            :warmup-time "1s"
            :measurement-time "1s"
            :jvm-opts []}
   :decision {:forks 3
              :warmups 5
              :measurements 5
              :warmup-time "1s"
              :measurement-time "1s"
              :jvm-opts ["-Xms2g" "-Xmx2g" "-XX:+UseG1GC"]}
   :decision-gc {:forks 3
                 :warmups 5
                 :measurements 5
                 :warmup-time "1s"
                 :measurement-time "1s"
                 :jvm-opts ["-Xms2g" "-Xmx2g" "-XX:+UseG1GC"]
                 :profiler "gc"}})

(defn- phase2-manifest-file
  [profile]
  (str phase2-manifest-dir "/phase2-"
       (if (= profile :decision-gc) "decision" (name profile))
       ".edn"))

(defn- phase2-manifest
  [profile]
  (let [file (phase2-manifest-file profile)]
    (edn/read-string (slurp file))))

(defn- manifest-jmh-command
  [profile-options jar-path profile output {:keys [class method params]}]
  (let [{:keys [forks warmups measurements warmup-time measurement-time
                jvm-opts profiler]} (get profile-options profile)
        include (str "^" (java.util.regex.Pattern/quote class) "\\."
                     (java.util.regex.Pattern/quote method) "$")
        jvm-args (str/join " " jvm-opts)
        fixed (cond-> ["java" "-jar" jar-path
                       "-f" (str forks)
                       "-wi" (str warmups)
                       "-w" warmup-time
                       "-i" (str measurements)
                       "-r" measurement-time]
                (seq jvm-opts) (into ["-jvmArgs" jvm-args])
                profiler (into ["-prof" profiler]))]
    (into (into fixed ["-rf" "json" "-rff" output])
          (concat (mapcat (fn [[name values]]
                            ["-p" (str name "=" (str/join "," values))])
                          (sort-by key params))
                  [include]))))

(defn- phase2-jmh-command
  [profile output cell]
  (manifest-jmh-command phase2-profile-options bench-jar-path profile output cell))

(defn- manifest-cell-output
  [phase-label temporary index cell]
  (let [id (:id cell)]
    (when-not (and (string? id)
                   (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]*" id))
      (throw (ex-info (str "Invalid " phase-label " manifest cell ID")
                      {:id id})))
    (str temporary "/" (format "%03d-%s.json" index id))))

(defn- phase2-cell-output
  [temporary index cell]
  (manifest-cell-output "Phase 2" temporary index cell))

(defn- run-manifest-profile!
  "Run one explicit manifest profile with phase-specific paths and commands.

  The execution/merge/validation lifecycle is shared by Phase 2 and Phase 3;
  callers supply only their jar, manifest, output namespace, and environment
  argv so the two phases remain isolated without duplicating orchestration."
  [profile {:keys [run-id] :as _opts}
   {:keys [label manifest-file manifest jar-path result-prefix
           environment-prefix temporary-prefix command-fn cell-output-fn
           environment-command]}]
  (let [commit (git-commit)
        run-id (some-> run-id str str/trim)
        artifact (artifact-id commit run-id)
        prefix (name profile)
        result (str result-prefix prefix "-" artifact ".json")
        environment (str environment-prefix prefix "-" artifact ".edn")
        temporary (str (java.nio.file.Files/createTempDirectory
                         (java.nio.file.Paths/get "/private/tmp"
                           (make-array String 0))
                         (str temporary-prefix prefix "-")
                         (make-array java.nio.file.attribute.FileAttribute 0)))
        cells (:cells manifest)
        outputs (mapv (fn [index cell]
                        (cell-output-fn temporary index cell))
                      (range)
                      cells)
        commands (mapv (fn [output cell]
                         (command-fn profile output cell))
                       outputs cells)
        _ (ensure-absent! result)
        _ (ensure-absent! environment)]
    ;; Parse and applicability-check the checked-in manifest before any fork.
    (run-bench-runner! ["manifest" manifest-file])
    (doseq [command commands]
      (println "Running" label prefix ":" (str/join " " command))
      (process! command))
    (run-bench-runner!
      (into ["merge-manifest" result manifest-file (name profile)] outputs))
    (run-bench-runner!
      ["validate-manifest" result manifest-file (name profile)])
    (run-bench-runner!
      (environment-command environment profile run-id result jar-path commands))
    (println "Validated" label prefix "result written to" result)
    (println "Environment metadata written to" environment)))

(defn- bench-profile
  [profile opts]
  ;; Every timing profile starts with the complete semantic/build/linkage gate.
  (check nil)
  (bench-jar nil)
  (run-manifest-profile!
    profile
    opts
    {:label "Phase 2"
     :manifest-file (phase2-manifest-file profile)
     :manifest (phase2-manifest profile)
     :jar-path bench-jar-path
     :result-prefix "results/phase-2/bench/"
     :environment-prefix "results/phase-2/environment-"
     :temporary-prefix "xfseq-phase2-"
     :command-fn phase2-jmh-command
     :cell-output-fn phase2-cell-output
     :environment-command
     (fn [environment profile run-id result jar-path commands]
       ["environment" environment (name profile) (or run-id "") result
        jar-path (pr-str commands)])}))

(defn bench-screen
  "Run the explicit two-fork Phase 2 screen matrix after semantic gates."
  [opts]
  (bench-profile :screen opts))

(defn bench-decision
  "Run the fresh three-fork direct-on Phase 2 decision matrix."
  [opts]
  (bench-profile :decision opts))

(defn bench-decision-gc
  "Run the same decision subset with JMH's separate GC profiler."
  [opts]
  (bench-profile :decision-gc opts))

(def phase2-jit-cells
  [{:id "java-list-8-identity-first"
    :class "xfseq.bench.Phase2JavaBenchmark"
    :method "loopFirst"
    :params {"candidateId" ["java-polymorphic-object-reduced-aware-v2"
                             "java-mixed-object-reduced-aware-v2"
                             "java-mixed-object-nonreducing-v2"
                             "java-dechunked-object-reduced-aware-v2"
                             "java-dechunked-object-nonreducing-v2"]
             "sourceKind" ["list"]
             "size" ["8"]
             "workload" ["identity"]}}
   {:id "java-list-10000-filter-traverse"
    :class "xfseq.bench.Phase2JavaBenchmark"
    :method "loopTraverse"
    :params {"candidateId" ["java-polymorphic-object-reduced-aware-v2"
                             "java-mixed-object-reduced-aware-v2"
                             "java-mixed-object-nonreducing-v2"
                             "java-dechunked-object-reduced-aware-v2"
                             "java-dechunked-object-nonreducing-v2"]
             "sourceKind" ["list"]
             "size" ["10000"]
             "workload" ["filter"]}}
   {:id "java-vector-64-identity-prefix8"
    :class "xfseq.bench.Phase2JavaBenchmark"
    :method "loopPrefix8"
    :params {"candidateId" ["java-polymorphic-object-reduced-aware-v2"
                             "java-mixed-object-reduced-aware-v2"
                             "java-mixed-object-nonreducing-v2"
                             "java-chunked-object-reduced-aware-v2"
                             "java-chunked-object-nonreducing-v2"]
             "sourceKind" ["vector"]
             "size" ["64"]
             "workload" ["identity"]}}
   {:id "java-vector-33-filter-first"
    :class "xfseq.bench.Phase2JavaBenchmark"
    :method "loopFirst"
    :params {"candidateId" ["java-polymorphic-object-reduced-aware-v2"
                             "java-mixed-object-reduced-aware-v2"
                             "java-mixed-object-nonreducing-v2"
                             "java-chunked-object-reduced-aware-v2"
                             "java-chunked-object-nonreducing-v2"]
             "sourceKind" ["vector"]
             "size" ["33"]
             "workload" ["filter"]}}
   {:id "java-vector-1000-map-traverse"
    :class "xfseq.bench.Phase2JavaBenchmark"
    :method "loopTraverse"
    :params {"candidateId" ["java-polymorphic-object-reduced-aware-v2"
                             "java-mixed-object-reduced-aware-v2"
                             "java-mixed-object-nonreducing-v2"
                             "java-chunked-object-reduced-aware-v2"
                             "java-chunked-object-nonreducing-v2"]
             "sourceKind" ["vector"]
             "size" ["1000"]
             "workload" ["map"]}}])

(defn- phase2-jit-command
  [output {:keys [class method params]}]
  (let [include (str "^" (java.util.regex.Pattern/quote class) "\\."
                     (java.util.regex.Pattern/quote method) "$"),
        jvm-args "-Xms2g -Xmx2g -XX:+UseG1GC",
        diagnostics "-XX:+UnlockDiagnosticVMOptions -XX:+PrintCompilation -XX:+PrintInlining"]
    (into ["java" "-jar" bench-jar-path
           "-f" "1"
           "-wi" "3"
           "-w" "1s"
           "-i" "3"
           "-r" "1s"
           "-jvmArgs" jvm-args
           "-jvmArgsAppend" diagnostics
           "-rf" "json"
           "-rff" output]
          (concat (mapcat (fn [[name values]]
                            ["-p" (str name "=" (str/join "," values))])
                          (sort-by key params))
                  [include]))))

(defn bench-jit
  "Capture direct-on HotSpot compilation/inlining evidence for reversal cells."
  [{:keys [run-id]}]
  (check nil)
  (bench-jar nil)
  (let [commit (git-commit)
        run-id (some-> run-id str str/trim)
        artifact (artifact-id commit run-id)
        temporary (str (java.nio.file.Files/createTempDirectory
                         (java.nio.file.Paths/get "/private/tmp"
                           (make-array String 0))
                         "xfseq-phase2-jit-"
                         (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [{:keys [id] :as cell} phase2-jit-cells]
      (let [output (str "results/phase-2/jit/" id "-" artifact ".log")
            json-output (str temporary "/" id ".json")
            command (phase2-jit-command json-output cell)]
        (ensure-absent! output)
        (println "Running jit:" (str/join " " command))
        (let [{:keys [exit out err]}
              (b/process {:command-args command
                          :out :capture
                          :err :capture})]
          (when-not (zero? exit)
            (throw (ex-info "JIT evidence process failed"
                            {:command-args command
                             :exit exit
                             :out out
                             :err err})))
          (b/write-file
            {:path output
             :string (str "command: " (str/join " " command) "\n\n"
                           "--- stdout ---\n" out
                           "\n--- stderr ---\n" err)}))
        (println "JIT evidence written to" output)))))

(defn bench-smoke
  "Run only the tiny identity/output smoke after all semantic gates."
  [{:keys [run-id]}]
  ;; This ordering is intentional: a green score from an invalid engine is
  ;; never accepted as benchmark evidence.
  (check nil)
  (bench-jar nil)
  (let [commit (git-commit)
        run-id (some-> run-id str str/trim)
        artifact-id (smoke-artifact-id commit run-id)
        result (str "results/phase-2/bench/smoke-" artifact-id ".json")
        environment (if (seq run-id)
                      (str "results/phase-2/environment-" artifact-id ".edn")
                      "results/phase-2/environment.edn")
        temporary (temp-directory)
        public-output (str temporary "/public.json")
        java-output (str temporary "/java.json")
        buffer-output (str temporary "/buffer.json")
        public-command
        (smoke-command
          public-output
          "xfseq.bench.Phase2PublicBenchmark"
          [["implementation" ["xfseq" "sequence"]]
           ["sourceKind" ["list"]]
           ["size" ["8"]]
           ["workload" ["identity"]]])
        java-command
        (smoke-command
          java-output
          "xfseq.bench.Phase2JavaBenchmark"
          [["candidateId" ["java-mixed-object-reduced-aware-v2"
                           "java-dechunked-object-reduced-aware-v2"]]
           ["sourceKind" ["list"]]
           ["size" ["8"]]
           ["workload" ["identity"]]])
        buffer-command
        (smoke-command
          buffer-output
          "xfseq.bench.Phase2BufferBenchmark"
          [["count" ["8"]]
           ["policy" ["current"]]])
        commands [public-command java-command buffer-command]
        _ (ensure-absent! result)
        _ (ensure-absent! environment)]
    (doseq [command commands]
      (println "Running smoke:" (str/join " " command))
      (process! command))
    (run-bench-runner!
      (into ["merge-smoke" result] [public-output java-output buffer-output]))
    (run-bench-runner! ["validate-smoke" result])
    (run-bench-runner!
      ["environment" environment "smoke" (or run-id "") result bench-jar-path
       (pr-str commands)])
    (println "Validated smoke result written to" result)
    (println "Environment metadata written to" environment)))

(defn bench-validate
  "Validate the current commit's durable smoke result without rewriting it."
  [{:keys [run-id]}]
  (let [run-id (some-> run-id str str/trim)
        result (str "results/phase-2/bench/smoke-"
                    (smoke-artifact-id (git-commit) run-id)
                    ".json")]
    (println (run-bench-runner! ["validate-smoke" result]))))

;; Phase 3 uses its own result namespace and jar so direct-unary receipts can
;; never overwrite the accepted Phase 2 evidence.  The execution profiles
;; intentionally mirror the Phase 2 local direct-on settings.
(def phase3-profile-options
  {:smoke {:forks 1
           :warmups 2
           :measurements 2
           :warmup-time "100ms"
           :measurement-time "100ms"
           :jvm-opts []}
   :screen {:forks 2
            :warmups 3
            :measurements 3
            :warmup-time "1s"
            :measurement-time "1s"
            :jvm-opts []}
   :decision {:forks 3
              :warmups 5
              :measurements 5
              :warmup-time "1s"
              :measurement-time "1s"
              :jvm-opts ["-Xms2g" "-Xmx2g" "-XX:+UseG1GC"]}
   :decision-gc {:forks 3
                 :warmups 5
                 :measurements 5
                 :warmup-time "1s"
                 :measurement-time "1s"
                 :jvm-opts ["-Xms2g" "-Xmx2g" "-XX:+UseG1GC"]
                 :profiler "gc"}})

(defn- phase3-manifest-file
  [profile]
  (str phase3-manifest-dir "/phase3-"
       (if (= profile :decision-gc) "decision" (name profile))
       ".edn"))

(defn- phase3-jmh-command
  [profile output cell]
  (manifest-jmh-command phase3-profile-options phase3-bench-jar-path
                         profile output cell))

(defn- phase3-smoke-command
  [output method params]
  (into ["java" "-jar" phase3-bench-jar-path
         "-f" "1"
         "-wi" "2"
         "-w" "100ms"
         "-i" "2"
         "-r" "100ms"
         "-rf" "json"
         "-rff" output]
        (concat (mapcat (fn [[name values]]
                          ["-p" (str name "=" (str/join "," values))])
                        params)
                [(str "^xfseq.bench.Phase3UnaryBenchmark\\."
                      (java.util.regex.Pattern/quote method) "$")])))

(defn- phase3-cell-output
  [temporary index cell]
  (manifest-cell-output "Phase 3" temporary index cell))

(declare phase3-trial!)

(defn- phase3-bench-profile
  [profile opts]
  ;; A timing profile is never allowed to bypass the semantic/build/linkage
  ;; gates, even when the caller asks only for one checked-in subset.
  (let [manifest-file (or (:manifest-file opts)
                          (phase3-manifest-file profile))
        manifest (edn/read-string (slurp manifest-file))
        focused? (= "xfseq.bench.Phase3FocusedBenchmark"
                    (-> manifest :cells first :class))]
    (check nil)
    (phase3-bench-jar nil)
    (phase3-trial!)
    (run-manifest-profile!
      profile
      opts
      {:label (if focused? "Phase 3 focused" "Phase 3")
       :manifest-file manifest-file
       :manifest manifest
       :jar-path phase3-bench-jar-path
       :result-prefix (if focused?
                       "results/phase-3/focused/bench/"
                       "results/phase-3/bench/")
       :environment-prefix (if focused?
                             "results/phase-3/focused/environment-"
                             "results/phase-3/environment-")
       :temporary-prefix (if focused?
                          "xfseq-phase3-focused-"
                          "xfseq-phase3-")
       :command-fn phase3-jmh-command
       :cell-output-fn phase3-cell-output
       :environment-command
       (fn [environment profile run-id result jar-path commands]
         ["environment" "phase3" environment (name profile) (or run-id "")
          result jar-path (pr-str commands)])})))

(defn phase3-bench-screen
  "Run the explicit Phase 3 direct-unary screen matrix."
  [opts]
  (phase3-bench-profile :screen opts))

(defn phase3-bench-decision
  "Run the fresh three-fork Phase 3 direct-on decision matrix."
  [opts]
  (phase3-bench-profile :decision opts))

(defn phase3-bench-decision-gc
  "Run the Phase 3 decision subset with a separate GC profiler."
  [opts]
  (phase3-bench-profile :decision-gc opts))

(defn phase3-bench-focused-screen
  "Run the broader Slice 4-focused screen without changing primary receipts."
  [opts]
  (phase3-bench-profile
    :screen
    (assoc opts :manifest-file
           (str phase3-manifest-dir "/phase3-focused-screen.edn"))))

(defn phase3-bench-focused
  "Run an explicit focused Phase 3 manifest at the selected profile."
  [{:keys [profile manifest-file] :as opts}]
  (let [profile (keyword (or profile "screen"))]
    (when-not (#{:screen :decision :decision-gc} profile)
      (throw (ex-info "Focused Phase 3 profile must be screen, decision, or decision-gc"
                      {:profile profile})))
    (phase3-bench-profile
      profile
      (assoc opts :manifest-file
             (or manifest-file
                 (str phase3-manifest-dir "/phase3-focused-screen.edn"))))))

(defn- phase3-trial!
  "Run the Phase 3 trial against the already-built benchmark jar."
  []
  (process! ["java" "-cp" phase3-bench-jar-path
             "xfseq.bench.Phase3BenchmarkSupport" "trial"]))

(defn phase3-bench-trial
  "Run the non-timed fresh-fixture value/checksum trial for all implementations."
  [_]
  (phase3-bench-jar nil)
  (phase3-trial!))

(defn phase3-bench-smoke
  "Run the tiny Phase 3 identity, fresh-iterator, and reduction smoke."
  [{:keys [run-id]}]
  (check nil)
  ;; Validate complete values/checksums on independent fixtures before any
  ;; one-shot source enters a measured JMH invocation.
  (phase3-bench-jar nil)
  (phase3-trial!)
  (let [commit (git-commit)
        run-id (some-> run-id str str/trim)
        artifact (smoke-artifact-id commit run-id)
        result (str "results/phase-3/bench/smoke-" artifact ".json")
        environment (str "results/phase-3/environment-smoke-" artifact ".edn")
        temporary (str (java.nio.file.Files/createTempDirectory
                         (java.nio.file.Paths/get "/private/tmp"
                           (make-array String 0))
                         "xfseq-phase3-smoke-"
                         (make-array java.nio.file.attribute.FileAttribute 0)))
        implementations ["core-direct" "candidate-direct" "xfseq-generic"
                         "sequence" "eduction" "transduce"]
        retained-implementations (vec (remove #{"transduce"}
                                              implementations))
        first-output (str temporary "/first.json")
        unretained-output (str temporary "/reduce-unretained.json")
        retained-output (str temporary "/reduce-retained.json")
        commands [(phase3-smoke-command
                    first-output
                    "first"
                    [["implementation" implementations]
                     ["operation" ["map" "filter" "remove" "take"]]
                     ["sourceKind" ["list"]]
                     ["size" ["8"]]])
                  (phase3-smoke-command
                    unretained-output
                    "reduceUnretained"
                    [["implementation" implementations]
                     ["operation" ["map"]]
                     ["sourceKind" ["iterator"]]
                     ["size" ["8"]]])
                  (phase3-smoke-command
                    retained-output
                    "reduceRetained"
                    [["implementation" retained-implementations]
                     ["operation" ["map"]]
                     ["sourceKind" ["iterator"]]
                     ["size" ["8"]]])]
        _ (ensure-absent! result)
        _ (ensure-absent! environment)]
    (doseq [command commands]
      (println "Running Phase 3 smoke:" (str/join " " command))
      (process! command))
    (run-bench-runner! (into ["merge-smoke" result] [first-output
                                                       unretained-output
                                                       retained-output]))
    (run-bench-runner! ["validate-phase3-smoke" result])
    (run-bench-runner!
      ["environment" "phase3" environment "smoke" (or run-id "")
       result phase3-bench-jar-path (pr-str commands)])
    (println "Validated Phase 3 smoke result written to" result)
    (println "Phase 3 smoke environment metadata written to" environment)))

(defn phase3-bench-validate
  "Validate the current commit's durable Phase 3 smoke result."
  [{:keys [run-id]}]
  (let [run-id (some-> run-id str str/trim)
        result (str "results/phase-3/bench/smoke-"
                    (smoke-artifact-id (git-commit) run-id)
                    ".json")]
    (println (run-bench-runner! ["validate-phase3-smoke" result]))))
