(ns xfseq.phase-0-bench
  "Historical Phase 0 candidate registry and timing snapshot.

  This namespace is development tooling.  It names and constructor-smokes the
  preserved candidates without adding production dispatch, and benchmarks only
  the three top-level paths retained from the 2020 experiment."
  (:refer-clojure :exclude [sorted-map])
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.core :as clj]
    [criterium.core :as crit]
    [xfseq.core :as core]
    [xfseq.gen :as gen])
  (:import
    [java.lang ProcessHandle]
    [java.lang.management ManagementFactory]
    [java.security MessageDigest]
    [java.nio.file Files]
    [java.io StringWriter]
    [xfseq LongCons DoubleCons]
    [xfseq.buffer ObjectBuffer LongBuffer DoubleBuffer]))

(def preservation-sha
  "The immutable source identity used by every Phase 0 report."
  "168ce02f2dcb796045990fe1647205f4da20c1f5")

(def preservation-tag "research-2020-05-10")
(def size (long 10000))
(def candidate-paths ["core-direct" "legacy-clj-generated" "legacy-asm"])

(defn sorted-map
  "Build a map with stable key order for diffable EDN output."
  [& kvs]
  (into (clojure.core/sorted-map)
        (clojure.core/map vec (partition 2 kvs))))

(defn now-ns [] (System/nanoTime))

(defn class-name [value]
  (when (some? value)
    (.getName ^Class (class value))))

(defn exception-data [^Throwable error]
  (sorted-map
    :status :threw
    :class (class-name error)
    :message (.getMessage error)))

(defn outcome
  "Capture a constructor smoke observation without hiding a failure."
  [f]
  (try
    (sorted-map :status :ok :class (class-name (f)))
    (catch Throwable error
      (exception-data error))))

(defn parse-options
  [args]
  (loop [options {}
         remaining args]
    (if (empty? remaining)
      options
      (let [flag (first remaining)
            value (second remaining)]
        (if-not (and (string? flag)
                     (= \- (first flag))
                     (string? value))
          (throw (IllegalArgumentException.
                   (str "Expected --option value, got " (pr-str remaining))))
          (recur (assoc options
                   (keyword (subs flag 2))
                   value)
                 (nnext remaining)))))))

(defn sha256-bytes [^bytes bytes]
  (let [digest (.digest (doto (MessageDigest/getInstance "SHA-256")
                          (.update bytes)))]
    (apply str
      (clj/map #(format "%02x" (bit-and (int %) 0xff)) digest))))

(defn sha256-file [path]
  (sha256-bytes (Files/readAllBytes (.toPath (io/file path)))))

(defn write-new!
  "Write an artifact only when it does not already exist.

  Refusing an existing path makes reruns explicit and prevents one historical
  lane from silently replacing another."
  [path text]
  (let [file (io/file path)]
    (when (.exists file)
      (throw (ex-info "Refusing to overwrite existing Phase 0 artifact"
                      {:path path})))
    (io/make-parents file)
    (spit file text)
    path))

(defn edn-value
  "Convert Criterium's lazy/record values to reader-safe EDN data."
  [value]
  (cond
    (map? value)
    (into (clj/sorted-map)
      (clj/map (fn [[key nested]] [key (edn-value nested)]) value))

    (vector? value)
    (vec (clj/map edn-value value))

    (seq? value)
    (vec (clj/map edn-value value))

    (set? value)
    (vec (clj/map edn-value (sort value)))

    :else value))

(declare protocol-long-seq protocol-double-seq core-map)

(defn smoke-source
  "Return an actual ISeq with the input and chunk-mode required by a loop."
  [input-type mode]
  (case [input-type mode]
    [:object :chunked] (seq [(Long. 1) (Long. 2)])
    [:object :mixed] (seq [(Long. 1) (Long. 2)])
    [:object :dechunked] (seq (list (Long. 1) (Long. 2)))

    [:long :chunked] (protocol-long-seq (long-array [1 2]))
    [:long :mixed] (protocol-long-seq (long-array [1 2]))
    [:long :dechunked] (LongCons. (long 1) (LongCons. (long 2) nil))

    [:double :chunked] (protocol-double-seq (double-array [1.0 2.0]))
    [:double :mixed] (protocol-double-seq (double-array [1.0 2.0]))
    [:double :dechunked] (DoubleCons. (double 1.0) (DoubleCons. (double 2.0) nil))))

(defn buffer-for [argument-type]
  (case argument-type
    :object (ObjectBuffer.)
    :long (LongBuffer.)
    :double (DoubleBuffer.)))

(defn identity-function [argument-type]
  (case argument-type
    :object identity
    :long (fn ^long [^long value] value)
    :double (fn ^double [^double value] value)))

(defn identity-transducer [argument-type]
  (core-map (identity-function argument-type)))

(defn reducing-function [argument-type buffer]
  ((identity-transducer argument-type) buffer))

(defn argument-symbol [argument-type]
  (case argument-type
    :object 'Object
    :long 'long
    :double 'double))

(defn input-symbol [input-type]
  (argument-symbol input-type))

(defn generated-mode-keyword [mode]
  (keyword "xfseq.gen" (name mode)))

(defn resolve-var [namespace-sym var-sym]
  (or (ns-resolve namespace-sym var-sym)
      (throw (IllegalStateException.
               (str "Required namespace var is unavailable: "
                    namespace-sym "/" var-sym)))))

(defn core-map [& args]
  (apply (resolve-var 'xfseq.core 'map) args))

(defn core-xf-seq [& args]
  (apply (resolve-var 'xfseq.core 'xf-seq) args))

(defn protocol-long-seq [value]
  ((resolve-var 'xfseq.protocols 'long-seq) value))

(defn protocol-double-seq [value]
  ((resolve-var 'xfseq.protocols 'double-seq) value))

(defn type-letter [type]
  (case type
    :object "O"
    :long "L"
    :double "D"))

(defn asm-class-name [argument-type input-type check-identity? mode]
  (str "xfseq.gen.XFSeqStep_"
       (type-letter argument-type)
       (type-letter input-type)
       (if check-identity? "T" "F")
       (case mode :mixed "M" :chunked "C" :dechunked "D")))

(defn asm-id [argument-type input-type check-identity? mode]
  (format "legacy-asm-%s-%s-%s-%s"
          (name argument-type)
          (name input-type)
          (if check-identity? "identity-stop" "no-stop")
          (name mode)))

(defn asm-candidate [argument-type input-type check-identity? mode]
  (sorted-map
    :stable-id (asm-id argument-type input-type check-identity? mode)
    :kind :asm
    :source-class (asm-class-name argument-type input-type check-identity? mode)
    :argument-type argument-type
    :input-type input-type
    :identity-stop? check-identity?
    :source-mode mode
    :constructor-key [(str (argument-symbol argument-type))
                      (str (input-symbol input-type))
                      check-identity?
                      (str (generated-mode-keyword mode))]
    :semantic-status :semantically-non-equivalent
    :timed? false))

(def asm-candidates
  (vec
    (for [argument-type [:object :long :double]
          input-type [:object :long :double]
          check-identity? [true false]
          mode [:mixed :chunked :dechunked]]
      (asm-candidate argument-type input-type check-identity? mode))))

(def java-candidates
  [(sorted-map
     :stable-id "java-polymorphic-object-identity-stop"
     :kind :java
     :source-class "xfseq.XFSeqStep$ObjectStep"
     :argument-type :object :input-type :object :source-mode :mixed
     :constructor-order [:reducing-function :source :buffer]
     :semantic-status :semantically-non-equivalent :timed? false)
   (sorted-map
     :stable-id "java-polymorphic-long-identity-stop"
     :kind :java
     :source-class "xfseq.XFSeqStep$LongStep"
     :argument-type :long :input-type :long :source-mode :mixed
     :constructor-order [:reducing-function :source :buffer]
     :semantic-status :semantically-non-equivalent :timed? false)
   (sorted-map
     :stable-id "java-polymorphic-double-identity-stop"
     :kind :java
     :source-class "xfseq.XFSeqStep$DoubleStep"
     :argument-type :double :input-type :double :source-mode :mixed
     :constructor-order [:reducing-function :source :buffer]
     :semantic-status :semantically-non-equivalent :timed? false)
   (sorted-map
     :stable-id "java-mixed-object-identity-stop"
     :kind :java
     :source-class "xfseq.XFSeqStepSimple"
     :argument-type :object :input-type :object :source-mode :mixed
     :constructor-order [:buffer :reducing-function :source]
     :semantic-status :semantically-non-equivalent :timed? false)
   (sorted-map
     :stable-id "java-mixed-object-no-stop"
     :kind :java
     :source-class "xfseq.XFSeqStepSimpleNoReduced"
     :argument-type :object :input-type :object :source-mode :mixed
     :constructor-order [:buffer :reducing-function :source]
     :semantic-status :semantically-non-equivalent :timed? false)
   (sorted-map
     :stable-id "java-dechunked-object-identity-stop"
     :kind :java
     :source-class "xfseq.XFSeqStepSingleOnly"
     :argument-type :object :input-type :object :source-mode :dechunked
     :constructor-order [:buffer :reducing-function :source]
     :semantic-status :semantically-non-equivalent :timed? false)
   (sorted-map
     :stable-id "java-dechunked-object-no-stop"
     :kind :java
     :source-class "xfseq.XFSeqStepSingleOnlyNoReduced"
     :argument-type :object :input-type :object :source-mode :dechunked
     :constructor-order [:buffer :reducing-function :source]
     :semantic-status :semantically-non-equivalent :timed? false)
   (sorted-map
     :stable-id "java-chunked-object-identity-stop"
     :kind :java
     :source-class "xfseq.XFSeqStepChunkedOnly"
     :argument-type :object :input-type :object :source-mode :chunked
     :constructor-order [:buffer :reducing-function :source]
     :semantic-status :semantically-non-equivalent :timed? false)
   (sorted-map
     :stable-id "java-chunked-object-no-stop"
     :kind :java
     :source-class "xfseq.XFSeqStepChunkedOnlyNoReduced"
     :argument-type :object :input-type :object :source-mode :chunked
     :constructor-order [:buffer :reducing-function :source]
     :semantic-status :semantically-non-equivalent :timed? false)
   (sorted-map
     :stable-id "java-mixed-long-from-object"
     :kind :java
     :source-class "xfseq.XFSeqStepSimpleLong"
     :argument-type :long :input-type :object :source-mode :mixed
     :constructor-order [:buffer :reducing-function :source]
     :semantic-status :semantically-non-equivalent :timed? false)
   (sorted-map
     :stable-id "java-mixed-long-from-long"
     :kind :java
     :source-class "xfseq.XFSeqStepSimpleLongLong"
     :argument-type :long :input-type :long :source-mode :mixed
     :constructor-order [:buffer :reducing-function :source]
     :semantic-status :semantically-non-equivalent :timed? false)
   (sorted-map
     :stable-id "java-mixed-object-from-long"
     :kind :java
     :source-class "xfseq.XFSeqStepSimpleObjectLong"
     :argument-type :object :input-type :long :source-mode :mixed
     :constructor-order [:buffer :reducing-function :source]
     :semantic-status :semantically-non-equivalent :timed? false)
   (sorted-map
     :stable-id "java-mixed-double-from-long"
     :kind :java
     :source-class "xfseq.XFSeqStepSimpleDoubleLong"
     :argument-type :double :input-type :long :source-mode :mixed
     :constructor-order [:buffer :reducing-function :source]
     :semantic-status :semantically-non-equivalent :timed? false)])

(def top-level-candidates
  [(sorted-map
     :stable-id "core-direct"
     :kind :top-level
     :source-class "clojure.core/map"
     :semantic-status :oracle
     :timed? true)
   (sorted-map
     :stable-id "core-sequence"
     :kind :top-level
     :source-class "clojure.core/sequence"
     :semantic-status :oracle
     :timed? false)
   (sorted-map
     :stable-id "legacy-clj-generated"
     :kind :top-level
     :source-class "xfseq.core/xf-seq; XFSeqStep_<rf><input> deftypes"
     :semantic-status :semantically-non-equivalent
     :timed? true)
   (sorted-map
     :stable-id "legacy-asm"
     :kind :top-level
     :source-class "xfseq.gen/xf-seq"
     :semantic-status :semantically-non-equivalent
     :timed? true)])

(def candidate-registry
  (vec (concat top-level-candidates java-candidates asm-candidates)))

(defn registry-summary [registry]
  (sorted-map
    :total (count registry)
    :top-level-count (count (clj/filter #(= :top-level (:kind %)) registry))
    :java-count (count (clj/filter #(= :java (:kind %)) registry))
    :asm-count (count (clj/filter #(= :asm (:kind %)) registry))
    :asm-key-count (count (clj/filter #(= :asm (:kind %)) registry))
    :stable-id-count (count (set (clj/map :stable-id registry)))))

(defn constructor-for [class-name]
  (let [klass (Class/forName class-name)
        constructors (.getConstructors klass)]
    (when-not (= 1 (alength constructors))
      (throw (ex-info "Expected exactly one public candidate constructor"
                      {:class class-name
                       :constructor-count (alength constructors)})))
    (aget constructors 0)))

(defn invoke-constructor [class-name arguments]
  (.newInstance (constructor-for class-name) (object-array arguments)))

(defn smoke-java-candidate [candidate]
  (let [{:keys [argument-type input-type source-mode source-class
                constructor-order]} candidate
        buffer (buffer-for argument-type)
        reducing-function (reducing-function argument-type buffer)
        source (smoke-source input-type source-mode)
        by-name {:buffer buffer
                 :reducing-function reducing-function
                 :source source}]
    (outcome #(invoke-constructor source-class
                                  (clj/map by-name constructor-order)))))

(defn smoke-asm-candidate [candidate]
  (let [{:keys [argument-type input-type source-mode identity-stop?]} candidate
        gen-ns (find-ns 'xfseq.gen)
        ctors (var-get (ns-resolve gen-ns 'xf-seq-ctors))
        key [(argument-symbol argument-type)
             (input-symbol input-type)
             identity-stop?
             (generated-mode-keyword source-mode)]
        ctor (get ctors key)
        buffer (buffer-for argument-type)
        xf (identity-transducer argument-type)
        reducing-function (xf buffer)
        source (smoke-source input-type source-mode)]
    (if-not ctor
      (sorted-map :status :unreachable
                  :reason :missing-generated-constructor
                  :constructor-key (pr-str key))
      (outcome #(ctor buffer reducing-function source)))))

(defn smoke-candidate [candidate]
  (assoc candidate
    :constructor-smoke
    (case (:kind candidate)
      :java (smoke-java-candidate candidate)
      :asm (smoke-asm-candidate candidate)
      (sorted-map :status :not-applicable))))

(defn smoke-registry [registry]
  (let [smoked (vec (clj/map smoke-candidate registry))
        unsupported (vec
                     (clj/map #(sorted-map
                                 :stable-id (:stable-id %)
                                 :kind (:kind %)
                               :source-class (:source-class %)
                                 :smoke (:constructor-smoke %))
                               (clj/filter
                                 #(and (#{:java :asm} (:kind %))
                                       (not= :ok
                                             (get-in % [:constructor-smoke :status])))
                                 smoked)))]
    (sorted-map
      :candidates smoked
      :summary (registry-summary smoked)
      :unsupported-or-unreachable unsupported)))

(defn source-cases []
  (let [objs (repeat size (Long. 2))
        v-objs (vec objs)
        rang (range 0 size)
        v-rang (vec rang)
        s-rang (set rang)
        arr (object-array objs)
        l-arr (long-array size)
        d-arr (double-array size)]
    [{:source-id "objs" :source objs :argument-type :object
      :source-class (class-name objs) :source-mode :dechunked
      :transform identity :transform-kind :identity}
     {:source-id "v-objs" :source v-objs :argument-type :object
      :source-class (class-name v-objs) :source-mode :chunked
      :transform identity :transform-kind :identity}
     {:source-id "rang" :source rang :argument-type :long
      :source-class (class-name rang) :source-mode :chunked
      :transform (identity-function :long) :transform-kind :typed-identity}
     {:source-id "v-rang" :source v-rang :argument-type :long
      :source-class (class-name v-rang) :source-mode :chunked
      :transform (identity-function :long) :transform-kind :typed-identity}
     {:source-id "s-rang" :source s-rang :argument-type :long
      :source-class (class-name s-rang) :source-mode :dechunked
      :transform (identity-function :long) :transform-kind :typed-identity}
     {:source-id "arr" :source arr :argument-type :object
      :source-class (class-name arr) :source-mode :mixed
      :transform identity :transform-kind :identity}
     {:source-id "l-arr" :source l-arr :argument-type :long
      :source-class (class-name l-arr) :source-mode :mixed
      :transform (identity-function :long) :transform-kind :typed-identity}
     {:source-id "d-arr" :source d-arr :argument-type :double
      :source-class (class-name d-arr) :source-mode :mixed
      :transform (identity-function :double) :transform-kind :typed-identity}]))

(def nil-rf (fn [acc _value] acc))

(defn expression-for [candidate source-case]
  (let [coll (:source source-case)
        transform (:transform source-case)]
    (case candidate
      "core-direct"
      (fn [] (reduce nil-rf nil (map transform coll)))

      "legacy-clj-generated"
      (fn [] (reduce nil-rf nil
                     (core/xf-seq (core/map transform) coll)))

      "legacy-asm"
      (fn [] (reduce nil-rf nil
                     (gen/xf-seq (core/map transform) coll))))))

(def benchmark-options
  ;; Criterium's defaults are {:samples 60, :warmup-jit-period 10e9,
  ;; :target-execution-time 1e9, :bootstrap-size 1000}.  These bounded values
  ;; retain Criterium's sampling/statistics but are an explicit historical
  ;; snapshot deviation; the raw values and uncertainty remain in each result.
  {:samples 3
   :warmup-jit-period (long (* 100 1000000))
   :target-execution-time (long (* 25 1000000))
   :tail-quantile 0.025
   :bootstrap-size 100
   :max-gc-attempts 3
   :overhead 0
   :supress-jvm-option-warnings true})

(defn criterium-result [result]
  (edn-value
    (select-keys result
      [:execution-count :sample-count :samples :results :total-time
       :warmup-time :warmup-executions :final-gc-time :overhead :outliers
       :mean :sample-mean :variance :sample-variance :lower-q :upper-q
       :outlier-variance :tail-quantile :options :os-details
       :runtime-details])))

(defn run-timing-case [candidate source-case]
  (let [started (now-ns)
        result (crit/benchmark*
                 (expression-for candidate source-case)
                 benchmark-options)]
    (sorted-map
      :status :ok
      :implementation candidate
      :semantic-status (if (= "core-direct" candidate)
                         :oracle
                         :semantically-non-equivalent)
      :source-id (:source-id source-case)
      :source-class (:source-class source-case)
      :source-mode (:source-mode source-case)
      :transform-kind (:transform-kind source-case)
      :size size
      :sink :value-discarding-full-reduce
      :elapsed-ns (- (now-ns) started)
      :criterium (criterium-result result)
      :stdout-report
      (with-out-str
        (crit/report-result result :verbose)))))

(defn os-command [command]
  (try
    (let [builder (doto (ProcessBuilder. (into-array String command))
                    (.redirectErrorStream true))
          _ (.directory builder (io/file (System/getProperty "user.dir")))
          process (.start builder)
          output (slurp (.getInputStream process))]
      (str/trim output))
    (catch Throwable _ nil)))

(defn classpath-artifacts []
  (vec
    (clj/map
      (fn [entry]
        (let [file (io/file entry)]
          (sorted-map
            :path entry
            :kind (if (.isFile file) :file :directory)
            :sha256 (when (.isFile file) (sha256-file entry)))))
      (str/split (System/getProperty "java.class.path")
                 (re-pattern (java.util.regex.Pattern/quote
                               java.io.File/pathSeparator))))))

(defn inferred-command [options]
  "Reconstruct the exact no-wrapper Java invocation when --command is omitted."
  (let [java (str (io/file (System/getProperty "java.home") "bin" "java"))
        jvm-flags (vec (.getInputArguments
                         (ManagementFactory/getRuntimeMXBean)))
        tail (concat ["clojure.main" "-m" "xfseq.phase-0-bench"]
                     (:command-arguments options))]
    (str/join " "
              (concat [java]
                      jvm-flags
                      ["-cp" (System/getProperty "java.class.path")]
                      tail))))

(defn runtime-metadata [options]
  (let [runtime (ManagementFactory/getRuntimeMXBean)
        arguments (vec (.getInputArguments runtime))
        direct-linking (:direct-linking options)]
    (sorted-map
      :phase "phase-0"
      :runner "dev/xfseq/phase_0_bench.clj"
      :schema-version 1
      :lane (:lane options)
      :fork (:fork options)
      :process-id (try (.pid (ProcessHandle/current)) (catch Throwable _ nil))
      :process-start-time-ms (.getStartTime runtime)
      :clojure-version (clojure-version)
      :requested-clojure-version (:clojure-version options)
      :criterium-version "0.4.5"
      :java-version (System/getProperty "java.version")
      :java-runtime-version (System/getProperty "java.runtime.version")
      :java-vm-name (System/getProperty "java.vm.name")
      :java-vm-vendor (System/getProperty "java.vm.vendor")
      :java-home (System/getProperty "java.home")
      :jvm-flags arguments
      :heap-max-bytes (.maxMemory (Runtime/getRuntime))
      :available-processors (.availableProcessors (Runtime/getRuntime))
      :os-name (System/getProperty "os.name")
      :os-version (System/getProperty "os.version")
      :os-architecture (System/getProperty "os.arch")
      :os-build (sorted-map
                  :product-version (os-command ["/usr/bin/sw_vers" "-productVersion"])
                  :build-version (os-command ["/usr/bin/sw_vers" "-buildVersion"])
                  :uname (os-command ["/usr/bin/uname" "-a"])
                  :machine (os-command ["/usr/bin/uname" "-m"]))
      :gc "G1"
      :java-target-bytecode 8
      :direct-linking-mode direct-linking
      :direct-linking-explicit true
      :exit-status 0
      :linking (sorted-map
                 :source-caller-direct-linking (= "true" direct-linking)
                 :source-candidate-direct-linking (= "true" direct-linking)
                 :released-core-jar-direct-linking true
                 :symmetry (if (= "true" direct-linking)
                             :symmetric-on
                             :asymmetric-released-core-on-source-off))
      :direct-linking-property (System/getProperty
                                 "clojure.compiler.direct-linking")
      :working-directory (System/getProperty "user.dir")
      :classpath (System/getProperty "java.class.path")
      :classpath-artifacts (classpath-artifacts)
      :command (or (:command options) (inferred-command options))
      :command-arguments (vec (:command-arguments options))
      :candidate-commit preservation-sha
      :preservation-sha preservation-sha
      :preservation-tag preservation-tag
      :criterium-config benchmark-options
      :uncertainty-fields [:samples :sample-count :mean :sample-mean
                           :lower-q :upper-q :variance :sample-variance
                           :outlier-variance])))

(defn metadata-with-artifacts [options report-text stdout-text]
  (assoc (runtime-metadata options)
    :raw-artifact-sha256
    {:edn (sha256-bytes (.getBytes report-text "UTF-8"))
     :stdout (sha256-bytes (.getBytes stdout-text "UTF-8"))}
    :artifact-paths
    (sorted-map :edn (:output options)
                :stdout (:stdout-output options)
                :metadata (:meta-output options))))

(defn run-snapshot! [options]
  (let [started (now-ns)
        registry* (atom nil)
        timing* (atom nil)
        stdout-text
        (with-out-str
          (println "Phase 0 historical timing snapshot")
          (println "lane=" (:lane options) "fork=" (:fork options))
          ;; Keep namespace-load diagnostics inside the raw stdout artifact.
          (require 'xfseq.core)
          (require 'xfseq.gen)
          (let [registry (smoke-registry candidate-registry)
                cases (source-cases)
                timing
                (vec
                  (for [candidate candidate-paths
                        source-case cases]
                    (let [result (run-timing-case candidate source-case)]
                      (println "case=" candidate "/" (:source-id source-case))
                      (print (:stdout-report result))
                      (flush)
                      (dissoc result :stdout-report))))]
            (reset! registry* registry)
            (reset! timing* timing)
            (println "registry-summary=" (pr-str (:summary registry)))
            (println "unsupported-or-unreachable="
                     (pr-str (:unsupported-or-unreachable registry)))))
        report-data
        (sorted-map
          :schema-version 1
          :phase "phase-0"
          :status :ok
          :metadata (sorted-map
                      :lane (:lane options)
                      :fork (:fork options)
                      :candidate-paths candidate-paths
                      :sources (vec (clj/map #(dissoc % :source :transform)
                                             (source-cases)))
                      :size size
                      :sink :value-discarding-full-reduce
                      :matrix (sorted-map
                                :implementations candidate-paths
                                :source-count 8
                                :case-count (* 3 8))
                      :identity-transform-kinds [:identity :typed-identity]
                      :semantic-status
                      (sorted-map
                        :core-direct :oracle
                        :legacy-clj-generated :semantically-non-equivalent
                        :legacy-asm :semantically-non-equivalent)
                      :interpretation
                      (sorted-map
                        :historical-context-only true
                        :allocation-measured false
                        :semantically-non-equivalent-legacy-candidates true
                        :release-performance-evidence false
                        :upstream-adoption-evidence false
                        :overall-winner false
                        :pooled-speedup false
                        :limitation-text
                        "No allocation; legacy candidates are semantically non-equivalent; this is not release/adoption evidence; no overall winner or pooled speedup is claimed.")
                      :criterium-config benchmark-options
                      :criterium-defaults
                      (select-keys crit/*default-benchmark-opts*
                                   [:samples :warmup-jit-period
                                    :target-execution-time :tail-quantile
                                    :bootstrap-size :max-gc-attempts])
                      :deviation-from-default
                      (sorted-map
                        :samples "3 instead of 60"
                        :warmup-jit-period "100ms instead of 10s"
                        :target-execution-time "25ms instead of 1s"
                        :bootstrap-size "100 instead of 1000"
                        :max-gc-attempts "3 instead of 100"
                        :overhead "explicitly 0; default estimates overhead"
                        :progress "disabled")
                      :raw-measures
                      "Each case retains Criterium samples, execution count, mean, quantiles, variance, uncertainty, warmup and GC fields in the structured report."
                      :candidate-registry-summary (:summary @registry*)
                      :unsupported-or-unreachable
                      (:unsupported-or-unreachable @registry*))
          :candidate-registry (:candidates @registry*)
          :timings @timing*
          :elapsed-ns (- (now-ns) started))
        report-text (prn-str report-data)
        _ (write-new! (:output options) report-text)
        _ (write-new! (:stdout-output options) stdout-text)
        metadata (metadata-with-artifacts options report-text stdout-text)
        metadata-text (prn-str metadata)]
    (write-new! (:meta-output options) metadata-text)
    (println (str "phase-0 bench complete: " (:output options)))
    (flush)))

(defn -main [& args]
  (let [options (parse-options args)
        required [:lane :fork :clojure-version :direct-linking
                  :output :stdout-output :meta-output]
        missing (clj/filter #(nil? (get options %)) required)]
    (when (seq missing)
      (throw (IllegalArgumentException.
               (str "Missing required options: " (pr-str missing)))))
    (run-snapshot! (assoc options :command-arguments args))
    (shutdown-agents)))
