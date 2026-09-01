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
(def phase2-manifest-dir "bench/manifests")
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

(defn- phase2-jmh-command
  [profile output {:keys [class method params]}]
  (let [{:keys [forks warmups measurements warmup-time measurement-time
                jvm-opts profiler]} (get phase2-profile-options profile)
        include (str "^" (java.util.regex.Pattern/quote class) "\\."
                     (java.util.regex.Pattern/quote method) "$")
        jvm-args (str/join " " jvm-opts)
        fixed (cond-> ["java" "-jar" bench-jar-path
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

(defn- phase2-cell-output
  [temporary index {:keys [id]}]
  (when-not (and (string? id)
                 (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]*" id))
    (throw (ex-info "Invalid Phase 2 manifest cell ID" {:id id})))
  (str temporary "/" (format "%03d-%s.json" index id)))

(defn- bench-profile
  [profile {:keys [run-id]}]
  ;; Every timing profile starts with the complete semantic/build/linkage gate.
  (check nil)
  (bench-jar nil)
  (let [commit (git-commit)
        run-id (some-> run-id str str/trim)
        manifest-file (phase2-manifest-file profile)
        manifest (phase2-manifest profile)
        artifact (artifact-id commit run-id)
        prefix (name profile)
        result (str "results/phase-2/bench/" prefix "-" artifact ".json")
        environment (str "results/phase-2/environment-" prefix "-"
                         artifact ".edn")
        temporary (str (java.nio.file.Files/createTempDirectory
                         (java.nio.file.Paths/get "/private/tmp"
                           (make-array String 0))
                         (str "xfseq-phase2-" prefix "-")
                         (make-array java.nio.file.attribute.FileAttribute 0)))
        cells (:cells manifest)
        outputs (mapv (fn [index cell]
                       (phase2-cell-output temporary index cell))
                     (range)
                     cells)
        commands (mapv (fn [output cell]
                         (phase2-jmh-command profile output cell))
                       outputs cells)
        _ (ensure-absent! result)
        _ (ensure-absent! environment)]
    ;; Parse and applicability-check the checked-in manifest before any fork.
    (run-bench-runner! ["manifest" manifest-file])
    (doseq [command commands]
      (println "Running" prefix ":" (str/join " " command))
      (process! command))
    (run-bench-runner!
      (into ["merge-manifest" result manifest-file (name profile)] outputs))
    (run-bench-runner!
      ["validate-manifest" result manifest-file (name profile)])
    (run-bench-runner!
      ["environment" environment (name profile) (or run-id "") result
       bench-jar-path (pr-str commands)])
    (println "Validated" prefix "result written to" result)
    (println "Environment metadata written to" environment)))

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
