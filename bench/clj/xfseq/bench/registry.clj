(ns xfseq.bench.registry
  "Checked-in Phase 2 benchmark parameters and evidence helpers.

  The registry deliberately lives outside the normal source path.  It is the
  single place for the release-equivalent parameter vocabulary and the
  profile-specific validation/evidence contract.  The tools.build task mirrors
  the execution fields in its isolated build namespace."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.set :as set]
            [clojure.string :as str]
            [xfseq.phase-2-candidates :as candidates])
  (:import [java.lang.management ManagementFactory]
           [java.math BigInteger]
           [java.nio.file Files LinkOption OpenOption Path Paths StandardOpenOption]
           [java.security MessageDigest]))

(set! *warn-on-reflection* true)

(def jmh-version "1.37")
(def library-clojure-version "1.12.5")

(def source-kinds
  ["list" "vector" "subvector" "range" "set" "array" "iterable"])

(def sizes
  [0 1 4 8 31 32 33 64 1000 10000 1000000])

(def workloads
  ["identity" "map" "filter" "map-filter" "five-map" "take"])

(def sinks
  ["construct" "first" "prefix8" "traverse" "vector" "reduce"])

(def buffer-policies
  ["current" "all-chunk"])

(def public-implementations
  ["xfseq" "sequence" "eduction" "transduce"])

(def candidate-ids
  (candidates/candidate-ids))

(def candidate-source-modes
  (into {}
        (map (juxt :stable-id :source-mode)
             candidates/object-candidate-registry)))

(def parameter-registry
  {:sources source-kinds
   :sizes sizes
   :workloads workloads
   :sinks sinks
   :buffer-policies buffer-policies
   :public-implementations public-implementations
   :candidate-ids candidate-ids
   :candidate-source-modes candidate-source-modes})

(def benchmark-methods
  {"xfseq.bench.Phase2PublicBenchmark"
   #{"construct" "first" "prefix8" "traverse" "vector" "reduce"}
   "xfseq.bench.Phase2JavaBenchmark"
   #{"loopFirst" "loopPrefix8" "loopTraverse"}
   "xfseq.bench.Phase2BufferBenchmark"
   #{"appendAndFlush"}})

(def phase3-source-kinds
  ["list" "vector" "subvector" "range" "set" "array" "iterable"
   "iterator"])

(def phase3-implementations
  ["core-direct" "candidate-direct" "xfseq-generic" "sequence" "eduction"
   "transduce"])

(def phase3-operations
  ["map" "filter" "remove" "take"])

(def phase3-sizes sizes)

(def phase3-parameter-registry
  {:sources phase3-source-kinds
   :sizes phase3-sizes
   :operations phase3-operations
   :implementations phase3-implementations
   :sinks ["construct" "first" "prefix8" "traverse" "vector"
           "reduceUnretained" "reduceRetained"]})

(def phase3-benchmark-methods
  {"xfseq.bench.Phase3UnaryBenchmark"
   #{"construct" "first" "prefix8" "traverse" "vector"
     "reduceUnretained" "reduceRetained"}})

(def phase3-parameter-values
  {"implementation" (set phase3-implementations)
   "operation" (set phase3-operations)
   "sourceKind" (set phase3-source-kinds)
   "size" (set (map str phase3-sizes))})

(def phase3-required-parameter-keys
  #{"implementation" "operation" "sourceKind" "size"})

;; The primary Phase 3 manifests deliberately keep the exact four-key
;; identity used by the tiny smoke.  Slice 4 needs a broader, focused lane
;; for workload/selectivity/source-shape diagnosis; it is a separate class
;; and vocabulary so adding those dimensions cannot silently change the
;; primary receipts.
(def phase3-focused-source-kinds
  ["list" "lazy-list" "vector" "subvector" "range" "set"
   "map-entries" "sorted-map-entries" "array" "iterable" "iterator"
   "repeat" "iterate"])

(def phase3-focused-implementations
  (vec (concat phase3-implementations candidate-ids)))

(def phase3-focused-workloads
  ["identity" "arithmetic" "heavy" "selectivity-0" "selectivity-1"
   "selectivity-50" "selectivity-99" "selectivity-100" "take"])

(def phase3-focused-take-counts
  ["0" "1" "8" "31" "32" "33" "source-length" "small-prefix"
   "large-prefix"])

(def phase3-focused-required-parameter-keys
  #{"implementation" "operation" "sourceKind" "size" "workload"
    "takeCount"})

(def phase3-focused-parameter-registry
  {:sources phase3-focused-source-kinds
   :sizes phase3-sizes
   :operations phase3-operations
   :workloads phase3-focused-workloads
   :take-counts phase3-focused-take-counts
   :implementations phase3-focused-implementations
   :sinks (get phase3-parameter-registry :sinks)})

(def phase3-focused-benchmark-methods
  {"xfseq.bench.Phase3FocusedBenchmark"
   #{"construct" "first" "prefix8" "traverse" "vector"
     "reduceUnretained" "reduceRetained"}})

(def phase3-focused-parameter-values
  {"implementation" (set phase3-focused-implementations)
   "operation" (set phase3-operations)
   "sourceKind" (set phase3-focused-source-kinds)
   "size" (set (map str phase3-sizes))
   "workload" (set phase3-focused-workloads)
   "takeCount" (set phase3-focused-take-counts)})

(declare valid-number? valid-score-error? valid-gc-allocation? validate-result!)

(def profiles
  {:smoke {:forks 1
           :warmups 2
           :measurements 2
           :warmup-time "100ms"
           :measurement-time "100ms"
           :format "json"
           :jvm-opts []
           :purpose :identity-and-output-validation}
   :screen {:forks 2
            :warmups 3
            :measurements 3
            :warmup-time "1s"
            :measurement-time "1s"
            :format "json"
            :jvm-opts []
            :purpose :plausible-reversal-screen}
   :decision {:forks 3
              :warmups 5
              :measurements 5
              :warmup-time "1s"
              :measurement-time "1s"
              :format "json"
              :jvm-opts ["-Xms2g" "-Xmx2g" "-XX:+UseG1GC"]
              :purpose :cell-level-production-decision}
   :decision-gc {:forks 3
                 :warmups 5
                 :measurements 5
                 :warmup-time "1s"
                 :measurement-time "1s"
                 :format "json"
                 :allocation-profiler "gc"
                 :jvm-opts ["-Xms2g" "-Xmx2g" "-XX:+UseG1GC"]
                 :purpose :cell-level-allocation-evidence}})

(def smoke-groups
  [{:class "xfseq.bench.Phase2PublicBenchmark"
    :params {:implementation ["xfseq" "sequence"]
             :sourceKind ["list"]
             :size ["8"]
             :workload ["identity"]}}
   {:class "xfseq.bench.Phase2JavaBenchmark"
    :params {:candidateId ["java-mixed-object-reduced-aware-v2"
                          "java-dechunked-object-reduced-aware-v2"]
             :sourceKind ["list"]
             :size ["8"]
             :workload ["identity"]}}
   {:class "xfseq.bench.Phase2BufferBenchmark"
    :params {:count ["8"]}}])

(defn- path
  ^Path [value]
  (Paths/get (str value) (make-array String 0)))

(defn- ensure-parent!
  [^Path target]
  (let [parent (.getParent target)]
    (when parent
      (Files/createDirectories parent (make-array java.nio.file.attribute.FileAttribute 0))))
  target)

(defn ensure-new-path!
  "Fail if `target` already exists.  JMH itself is willing to replace a result
  file, so callers must reserve every durable result path before invoking it."
  [target]
  (let [target (ensure-parent! (path target))]
    (when (Files/exists target (make-array LinkOption 0))
      (throw (ex-info "Refusing to overwrite benchmark artifact"
                      {:path (str target)})))
    (str target)))

(defn- create-new-bytes!
  [target bytes]
  (let [target (ensure-parent! (path target))]
    (with-open [out (Files/newOutputStream
                      target
                      (into-array OpenOption
                                  [StandardOpenOption/CREATE_NEW
                                   StandardOpenOption/WRITE]))]
      (.write out ^bytes bytes))
    (str target)))

(defn write-edn-new!
  "Write metadata with CREATE_NEW semantics; never replace an earlier run."
  [target value]
  (create-new-bytes! target
                     (.getBytes (pr-str value) "UTF-8")))

(defn write-json-new!
  [target value]
  (create-new-bytes! target
                     (.getBytes (json/write-str value) "UTF-8")))

(defn sha256-file
  [file]
  (let [digest (MessageDigest/getInstance "SHA-256")
        buffer (byte-array 65536)]
    (with-open [in (io/input-stream file)]
      (loop []
        (let [n (.read in buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur)))))
    (format "%064x" (BigInteger. 1 (.digest digest)))))

(defn- sha256-bytes
  [^bytes bytes]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (.update digest bytes)
    (format "%064x" (BigInteger. 1 (.digest digest)))))

(defn- command-output
  [& args]
  (let [{:keys [exit out err]} (apply shell/sh args)]
    (when-not (zero? exit)
      (throw (ex-info "Command failed while capturing benchmark metadata"
                      {:command args :exit exit :stderr err})))
    (str/trim out)))

(defn- git-diff-sha256
  []
  ;; `git diff` deliberately excludes untracked benchmark files.  Those files
  ;; are hashed independently below, while this digest captures every tracked
  ;; dirty edit that was present when the environment was recorded.
  (let [{:keys [exit out err]} (shell/sh "git" "diff" "--binary" "HEAD" "--")]
    (when-not (zero? exit)
      (throw (ex-info "Unable to capture the dirty git diff"
                      {:exit exit :stderr err})))
    (sha256-bytes (.getBytes ^String out "UTF-8"))))

(defn- benchmark-source-evidence
  []
  (let [files (->> (file-seq (io/file "bench"))
                   (filter #(.isFile ^java.io.File %))
                   (map (fn [^java.io.File file]
                          {:path (.getPath file)
                           :sha256 (sha256-file file)
                           :bytes (.length file)}))
                   (sort-by :path)
                   vec)
        manifest (pr-str (mapv #(select-keys % [:path :sha256 :bytes]) files))]
    {:root "bench"
     :sha256 (sha256-bytes (.getBytes manifest "UTF-8"))
     :files files}))

(defn- git-metadata
  []
  (let [commit (command-output "git" "rev-parse" "HEAD")
        status (command-output "git" "status" "--porcelain")]
    {:commit commit
     :dirty? (not (str/blank? status))
     :status status
     :diff-sha256 (git-diff-sha256)}))

(defn- gc-metadata
  []
  (mapv (fn [^java.lang.management.GarbageCollectorMXBean bean]
          {:name (.getName bean)
           :collection-count (.getCollectionCount bean)
           :collection-time-ms (.getCollectionTime bean)})
        (ManagementFactory/getGarbageCollectorMXBeans)))

(defn environment
  "Capture the complete environment for one benchmark result group.

  `commands` is a vector of exact argv vectors, retained as data so a result
  can be reproduced without reconstructing shell quoting."
  ([profile result-path jar-path commands]
   (environment :phase2 profile nil result-path jar-path commands))
  ([profile run-id result-path jar-path commands]
   (environment :phase2 profile run-id result-path jar-path commands))
  ([phase profile run-id result-path jar-path commands]
   (let [^java.lang.Runtime runtime (Runtime/getRuntime)
         ^java.lang.management.RuntimeMXBean mx
         (ManagementFactory/getRuntimeMXBean)
         result-file (io/file result-path)
         jar-file (io/file jar-path)]
     {:schema-version 1
      :phase phase
      :profile profile
      :run-id run-id
      :jmh-version jmh-version
      :clojure-version library-clojure-version
      :clojure-cli-version (command-output "clojure" "--version")
      :java {:version (System/getProperty "java.version")
             :runtime-version (System/getProperty "java.runtime.version")
             :vm-name (System/getProperty "java.vm.name")
             :vm-version (System/getProperty "java.vm.version")
             :input-arguments (vec (.getInputArguments mx))}
      :os {:name (System/getProperty "os.name")
           :version (System/getProperty "os.version")
           :arch (System/getProperty "os.arch")}
      :cpu {:available-processors (.availableProcessors ^java.lang.Runtime runtime)}
      :memory {:max-bytes (.maxMemory ^java.lang.Runtime runtime)
               :total-bytes (.totalMemory ^java.lang.Runtime runtime)
               :free-bytes (.freeMemory ^java.lang.Runtime runtime)}
      :gc (gc-metadata)
      :direct-linking true
      :git (git-metadata)
      :source-evidence (benchmark-source-evidence)
      :jmh-jar {:path (.getPath jar-file)
                :sha256 (sha256-file jar-file)
                :bytes (.length jar-file)}
      :result {:path (.getPath result-file)
               :sha256 (sha256-file result-file)
               :bytes (.length result-file)}
      :commands commands
      :parameters (if (= :phase3 phase)
                    (assoc phase3-parameter-registry
                           :focused phase3-focused-parameter-registry)
                    parameter-registry)})))

(defn- read-json
  [file]
  (with-open [reader (io/reader file)]
    (json/read reader :key-fn keyword)))

(defn read-manifest
  "Read and validate one checked-in applicable-cell manifest."
  [file]
  (let [file (io/file file)
        manifest (edn/read-string (slurp file))
        cells (:cells manifest)
        phase (or (:phase manifest) :phase2)
        methods (if (= :phase3 phase)
                  (merge phase3-benchmark-methods
                         phase3-focused-benchmark-methods)
                  benchmark-methods)]
    (when-not (and (= 1 (:schema-version manifest))
                   (#{:phase2 :phase3} phase)
                   (#{:screen :decision} (:profile manifest))
                   (vector? cells)
                   (seq cells))
      (throw (ex-info "Invalid benchmark manifest header"
                      {:path (.getPath file) :manifest manifest})))
    (let [ids (map :id cells)]
      (when-not (and (every? string? ids)
                     (= (count ids) (count (distinct ids))))
        (throw (ex-info "Benchmark manifest IDs must be unique strings"
                        {:path (.getPath file)}))))
    (doseq [{:keys [id class method params]} cells]
      (when-not (and (contains? methods class)
                     (contains? (get methods class) method)
                     (map? params)
                     (every? (fn [[key values]]
                               (and (string? key)
                                    (vector? values)
                                    (seq values)
                                    (every? string? values)))
                             params))
        (throw (ex-info "Invalid benchmark manifest cell"
                        {:path (.getPath file)
                         :id id
                         :cell {:class class :method method :params params}}))))
    (doseq [{:keys [class method params id]} cells]
      (cond
        (and (= phase :phase3)
             (= class "xfseq.bench.Phase3UnaryBenchmark"))
        (do
          (when-not (= phase3-required-parameter-keys (set (keys params)))
            (throw (ex-info "Phase 3 benchmark cell must name exactly the required parameters"
                            {:path (.getPath file)
                             :id id
                             :required phase3-required-parameter-keys
                             :actual (set (keys params))})))
          (doseq [[key values] params]
            (let [allowed (get phase3-parameter-values key)]
              (when-not (and allowed (every? allowed values))
                (throw (ex-info "Invalid Phase 3 benchmark parameter"
                                {:path (.getPath file)
                                 :id id
                                 :parameter key
                                 :values values})))))
          (when (and (contains? #{"construct" "reduceRetained"} method)
                     (some #{"transduce"} (get params "implementation")))
            (throw (ex-info "Phase 3 transduce has no construct/retained-head sink"
                            {:path (.getPath file) :id id
                             :method method
                             :implementations (get params "implementation")})))
          nil)

        (and (= phase :phase3)
             (= class "xfseq.bench.Phase3FocusedBenchmark"))
        (do
          (when-not (= phase3-focused-required-parameter-keys
                      (set (keys params)))
            (throw (ex-info "Focused Phase 3 benchmark cell must name exactly the required parameters"
                            {:path (.getPath file)
                             :id id
                             :required phase3-focused-required-parameter-keys
                             :actual (set (keys params))})))
          (doseq [[key values] params]
            (let [allowed (get phase3-focused-parameter-values key)]
              (when-not (and allowed (every? allowed values))
                (throw (ex-info "Invalid focused Phase 3 benchmark parameter"
                                {:path (.getPath file)
                                 :id id
                                 :parameter key
                                 :values values})))))
          (let [operations (get params "operation")
                workloads (get params "workload")
                sources (get params "sourceKind")
                implementations (get params "implementation")]
            (when (and (contains? #{"construct" "reduceRetained"} method)
                       (some #{"transduce"} implementations))
              (throw (ex-info "Focused Phase 3 transduce has no construct/retained-head sink"
                              {:path (.getPath file) :id id
                               :method method :implementations implementations})))
            (when-not (every?
                       (fn [[operation workload]]
                         (case operation
                           "map" (contains? #{"identity" "arithmetic" "heavy"}
                                             workload)
                           "take" (= "take" workload)
                           (contains? #{"selectivity-0" "selectivity-1"
                                        "selectivity-50" "selectivity-99"
                                        "selectivity-100"}
                                       workload)))
                       (for [operation operations
                             workload workloads]
                         [operation workload]))
              (throw (ex-info "Focused Phase 3 workload does not apply to operation"
                              {:path (.getPath file) :id id
                               :operations operations :workloads workloads})))
            (doseq [candidate-id implementations
                    source-kind sources]
              (when-let [source-mode (get candidate-source-modes candidate-id)]
                (when-not (or (= :mixed source-mode)
                              (and (= :dechunked source-mode)
                                   (= "list" source-kind))
                              (and (= :chunked source-mode)
                                   (= "vector" source-kind)))
                  (throw (ex-info "Focused Phase 3 candidate is inapplicable to source"
                                  {:path (.getPath file) :id id
                                   :candidate-id candidate-id
                                   :source-kind source-kind
                                   :source-mode source-mode}))))
              (when (and (str/starts-with? candidate-id "java-")
                         (str/includes? candidate-id "nonreducing")
                         (some #{"take"} operations))
                (throw (ex-info "Focused Phase 3 non-reducing candidate cannot run take"
                                {:path (.getPath file) :id id
                                 :candidate-id candidate-id
                                 :operations operations}))))
            (when (and (some #{"repeat" "iterate"} sources)
                       (contains? #{"traverse" "vector" "reduceUnretained"
                                    "reduceRetained"}
                                  method)
                       (some #(not= "take" %) operations))
              (throw (ex-info "Focused Phase 3 infinite source needs a terminating take"
                              {:path (.getPath file) :id id
                               :source-kinds sources :operations operations
                               :method method}))))
          nil)

        (= class "xfseq.bench.Phase2JavaBenchmark")
        (doseq [candidate-id (get params "candidateId")
                source-kind (get params "sourceKind")]
          (let [source-mode (get candidate-source-modes candidate-id)]
            (when-not source-mode
              (throw (ex-info "Manifest names an unknown candidate ID"
                              {:path (.getPath file)
                               :id id
                               :candidate-id candidate-id})))
            (when-not (or (= :mixed source-mode)
                          (and (= :dechunked source-mode)
                               (= "list" source-kind))
                          (and (= :chunked source-mode)
                               (= "vector" source-kind)))
              (throw (ex-info "Manifest contains an inapplicable candidate cell"
                              {:path (.getPath file)
                               :id id
                               :candidate-id candidate-id
                               :source-kind source-kind
                               :source-mode source-mode})))))

        (= class "xfseq.bench.Phase2BufferBenchmark")
        (when-not (every? (set buffer-policies) (get params "policy"))
          (throw (ex-info "Manifest contains an unknown buffer policy"
                          {:path (.getPath file) :id id
                           :policy (get params "policy")})))))
    (assoc manifest :phase phase :path (.getPath file))))

(defn- parameter-combinations
  [params]
  (reduce (fn [combinations [key values]]
            (for [combination combinations
                  value values]
              (assoc combination key value)))
          [{}]
          (sort-by key params)))

(defn manifest-identities
  "Expand explicit manifest cells into exact benchmark identities."
  [manifest]
  (set (mapcat (fn [{:keys [class method params]}]
                 (map (fn [combination]
                        [class method combination])
                      (parameter-combinations params)))
               (:cells manifest))))

(defn- manifest-profile
  "Map a result profile to the profile represented by its manifest.

  GC evidence reruns the decision cells, so `:decision-gc` intentionally
  validates against a `:decision` manifest while retaining its stricter
  result-profile validation."
  [profile]
  (if (= :decision-gc profile)
    :decision
    profile))

(defn- validate-manifest-profile!
  [manifest manifest-file profile]
  (let [supplied (manifest-profile profile)
        declared (:profile manifest)]
    (when-not (= declared supplied)
      (throw (ex-info "Benchmark result profile differs from manifest profile"
                      {:manifest (.getPath (io/file manifest-file))
                       :manifest-profile declared
                       :profile profile
                       :manifest-validation-profile supplied})))
    manifest))

(defn- benchmark-identity
  [row]
  (let [name (:benchmark row)
        split (.lastIndexOf ^String name ".")]
    (when (neg? split)
      (throw (ex-info "JMH benchmark name has no method separator"
                      {:benchmark name})))
    [(subs name 0 split)
     (subs name (inc split))
     (into {}
           (map (fn [[key value]]
                  [(if (keyword? key) (clojure.core/name key) key) value])
                (:params row)))]))

(defn- validate-manifest-rows!
  [rows expected path manifest]
  (let [identities (mapv benchmark-identity rows)
        duplicate-identities (->> (frequencies identities)
                                  (filter (fn [[_ occurrences]]
                                            (> occurrences 1)))
                                  (sort-by (comp pr-str key))
                                  (mapv (fn [[identity occurrences]]
                                          {:identity identity
                                           :occurrences occurrences})))
        actual (set identities)]
    (when-not (= (count rows) (count expected))
      (throw (ex-info "JMH result row count differs from checked-in manifest"
                      {:path path
                       :manifest (:path manifest)
                       :expected-count (count expected)
                       :actual-count (count rows)})))
    (when (seq duplicate-identities)
      (throw (ex-info "JMH result contains duplicate benchmark identities"
                      {:path path
                       :manifest (:path manifest)
                       :duplicates duplicate-identities})))
    (when-not (= expected actual)
      (throw (ex-info "JMH result cells differ from checked-in manifest"
                      {:path path
                       :manifest (:path manifest)
                       :missing (vec (sort-by pr-str
                                              (set/difference expected actual)))
                       :unexpected (vec (sort-by pr-str
                                                 (set/difference actual expected)))})))
    rows))

(defn validate-rows!
  "Validate JMH rows without requiring them to be in a durable file yet."
  [rows profile path]
  (when-not (and (vector? rows) (seq rows))
    (throw (ex-info "JMH JSON must contain a non-empty result array"
                    {:path path})))
  (doseq [row rows]
    (when-not (and (map? row)
                   (string? (:benchmark row))
                   (string? (:mode row))
                   (= jmh-version (:jmhVersion row))
                   (map? (:primaryMetric row))
                   (valid-number? (get-in row [:primaryMetric :score]))
                   (valid-score-error?
                     (get-in row [:primaryMetric :scoreError])
                     profile)
                   (valid-gc-allocation? row profile)
                   (map? (:params row)))
      (throw (ex-info "JMH result row is missing required metrics"
                      {:path path :row row}))))
  rows)

(defn validate-manifest!
  "Require a result to contain exactly the applicable manifest cells."
  ([file manifest-file]
   (validate-manifest! file manifest-file :decision))
  ([file manifest-file profile]
   (let [manifest (validate-manifest-profile!
                    (read-manifest manifest-file)
                    manifest-file
                    profile)
         summary (validate-result! file profile)
         expected (manifest-identities manifest)
         rows (read-json file)]
     (validate-manifest-rows! rows expected (.getPath (io/file file)) manifest)
     (assoc summary
            :manifest (:path manifest)
            :manifest-sha256 (sha256-file manifest-file)
            :manifest-cells (count expected)))))

(defn- valid-number?
  [value]
  (and (number? value)
       (not (Double/isNaN (double value)))
       (not (Double/isInfinite (double value)))))

(defn- valid-score-error?
  [value profile]
  ;; JMH serializes the error as the string "NaN" when a short smoke profile
  ;; has too few samples for a confidence interval.  Decision profiles must
  ;; use enough iterations to produce a numeric error, but the smoke still
  ;; needs to validate as a shape/identity check.
  (or (valid-number? value)
      (and (= :smoke profile) (= "NaN" value))))

(defn- valid-gc-allocation?
  [row profile]
  ;; A decision-GC receipt is useful only when JMH actually emitted the
  ;; normalized allocation metric.  Throughput-only JSON must not masquerade
  ;; as allocation evidence merely because its primary score is numeric.
  (or (not (= :decision-gc profile))
      (valid-number?
        (get-in row [:secondaryMetrics :gc.alloc.rate.norm :score]))))

(defn validate-result!
  "Validate JMH JSON shape and return a compact summary.

  This is intentionally strict about the fields used for comparison.  A
  malformed or empty result cannot silently become a decision input."
  ([file]
   (validate-result! file :decision))
  ([file profile]
   (let [file (io/file file)]
    (when-not (.isFile file)
      (throw (ex-info "Benchmark result does not exist" {:path (.getPath file)})))
    (let [rows (read-json file)]
      (when-not (and (vector? rows) (seq rows))
        (throw (ex-info "JMH JSON must contain a non-empty result array"
                        {:path (.getPath file)})))
      (doseq [row rows]
        (when-not (and (map? row)
                       (string? (:benchmark row))
                       (string? (:mode row))
                       (= jmh-version (:jmhVersion row))
                       (map? (:primaryMetric row))
                       (valid-number? (get-in row [:primaryMetric :score]))
                       (valid-score-error?
                         (get-in row [:primaryMetric :scoreError])
                         profile)
                       (valid-gc-allocation? row profile)
                       (map? (:params row)))
          (throw (ex-info "JMH result row is missing required metrics"
                          {:path (.getPath file) :row row}))))
      {:path (.getPath file)
       :sha256 (sha256-file file)
       :rows (count rows)
       :benchmarks (->> rows (map :benchmark) distinct sort vec)
       :candidate-ids (->> rows
                           (keep #(get-in % [:params :candidateId]))
                           distinct sort vec)}))))

(defn validate-smoke!
  "Apply the additional identity checks required by the tiny smoke profile."
  [file]
  (let [summary (validate-result! file :smoke)
        benchmark-names (:benchmarks summary)
        ids (set (:candidate-ids summary))]
    (doseq [required ["xfseq.bench.Phase2PublicBenchmark"
                     "xfseq.bench.Phase2JavaBenchmark"
                     "xfseq.bench.Phase2BufferBenchmark"]]
      (when-not (some #(str/starts-with? % required) benchmark-names)
        (throw (ex-info "Smoke result omitted a required benchmark group"
                        {:required required :benchmarks benchmark-names}))))
    (when-not (and (contains? ids "java-mixed-object-reduced-aware-v2")
                   (contains? ids "java-dechunked-object-reduced-aware-v2"))
      (throw (ex-info "Smoke result did not exercise distinct candidate IDs"
                      {:candidate-ids ids})))
    (assoc summary :smoke? true)))

(defn validate-phase3-smoke!
  "Validate the Phase 3 smoke's direct/control implementations and fixtures."
  [file]
  (let [summary (validate-result! file :smoke)
        rows (read-json file)
        implementations (set (keep #(get-in % [:params :implementation]) rows))
        operations (set (keep #(get-in % [:params :operation]) rows))
        source-kinds (set (keep #(get-in % [:params :sourceKind]) rows))
        methods (set (map (fn [row]
                            (let [name (:benchmark row)
                                  split (.lastIndexOf ^String name ".")]
                              (subs name (inc split))))
                          rows))
        required-implementations (set phase3-implementations)
        invalid-transduce-sinks
        (->> rows
             (filter (fn [row]
                      (and (= "transduce"
                              (get-in row [:params :implementation]))
                           (contains? #{"construct" "reduceRetained"}
                                      (let [name (:benchmark row)
                                            split (.lastIndexOf ^String name ".")]
                                        (subs name (inc split)))))))
             vec)]
    (when-not (some #(str/starts-with?
                       %
                       "xfseq.bench.Phase3UnaryBenchmark")
                    (:benchmarks summary))
      (throw (ex-info "Phase 3 smoke did not exercise its benchmark class"
                      {:benchmarks (:benchmarks summary)})))
    (when-not (= required-implementations implementations)
      (throw (ex-info "Phase 3 smoke omitted an implementation"
                      {:required required-implementations
                       :actual implementations})))
    (when-not (= (set phase3-operations) operations)
      (throw (ex-info "Phase 3 smoke must exercise exactly all four operations"
                      {:required (set phase3-operations)
                       :actual operations})))
    (when (seq invalid-transduce-sinks)
      (throw (ex-info "Phase 3 smoke contains an inapplicable transduce sink"
                      {:rows invalid-transduce-sinks})))
    (doseq [method ["first" "reduceUnretained" "reduceRetained"]]
      (when-not (contains? methods method)
        (throw (ex-info "Phase 3 smoke omitted a required sink"
                        {:required method :methods methods}))))
    (when-not (contains? source-kinds "iterator")
      (throw (ex-info "Phase 3 smoke omitted the fresh iterator fixture"
                      {:source-kinds source-kinds})))
    (assoc summary :phase3? true)))

(defn merge-results!
  "Merge independently executed JMH JSON arrays into one new durable result."
  ([target inputs]
   (merge-results! target inputs :decision))
  ([target inputs profile]
   (let [rows (vec (mapcat (fn [file]
                             (let [parsed (read-json file)]
                               (when-not (and (vector? parsed) (seq parsed))
                                 (throw (ex-info "Temporary JMH result is empty"
                                                 {:path file})))
                               parsed))
                           inputs))]
     ;; Validate each child before reserving/writing the durable path.  A
     ;; strict decision merge therefore rejects smoke-only NaN rows without
     ;; leaving an invalid artifact behind.
     (doseq [file inputs]
       (validate-result! file profile))
     (let [target (ensure-new-path! target)]
       (write-json-new! target rows)
       (validate-result! target profile)))))

(defn merge-smoke-results!
  "Merge a short smoke profile, whose two-sample error may be `\"NaN\"`."
  [target inputs]
  (merge-results! target inputs :smoke))

(defn merge-manifest-results!
  "Validate and merge child JMH results against an explicit cell manifest."
  [target inputs manifest-file profile]
  (let [manifest (validate-manifest-profile!
                   (read-manifest manifest-file)
                   manifest-file
                   profile)
        rows (vec (mapcat (fn [file]
                            (let [parsed (read-json file)]
                              (validate-rows! parsed profile file)
                              parsed))
                          inputs))
        expected (manifest-identities manifest)]
    ;; Check every identity and row count before reserving the durable target;
    ;; duplicate rows from repeated/overlapping child runs must never be
    ;; hidden by set comparison or leave a partial result behind.
    (validate-manifest-rows! rows expected target manifest)
    (let [target (ensure-new-path! target)]
      (write-json-new! target rows)
      (validate-manifest! target manifest-file profile))))
