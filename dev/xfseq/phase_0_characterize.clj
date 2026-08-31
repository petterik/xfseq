(ns xfseq.phase-0-characterize
  "Data-oriented characterization of the preserved 2020 implementation.

  This namespace is development tooling.  It deliberately records the
  behavior of the current implementation, including known differences from
  Clojure, rather than turning those differences into product tests."
  (:refer-clojure :exclude [sorted-map])
  (:require
    [clojure.java.io :as io]
    [clojure.test :as test]
    [xfseq.core :as legacy]
    [xfseq.gen :as asm]
    [clojure.core :as clj]))

(def preservation-sha
  "168ce02f2dcb796045990fe1647205f4da20c1f5")

(def preservation-tag "research-2020-05-10")

(defn sorted-map
  "Build a map with stable key order for diffable EDN output."
  [& kvs]
  (into (clojure.core/sorted-map)
        (clojure.core/map vec (partition 2 kvs))))

(defn now-ns [] (System/nanoTime))

(defn class-name [x]
  (when (some? x)
    (.getName ^Class (class x))))

(defn exception-data [^Throwable error]
  (sorted-map
    :status :threw
    :class (class-name error)
    :message (.getMessage error)))

(defn outcome
  "Capture an observation without hiding a historical failure."
  [f]
  (try
    (sorted-map :status :ok :value (f))
    (catch Throwable error
      (exception-data error))))

(defn update-count [state key]
  (swap! state update key (fnil inc 0)))

(defn trace-source
  [events values]
  (reify clojure.lang.Seqable
    (seq [_]
      (swap! events conj (sorted-map :event :seq))
      (seq values))))

(defn traced-transducer
  "A transparent transducer that records application, step, and completion."
  [events started]
  (fn [rf]
    (swap! events update :applications
      (fnil conj [])
      (sorted-map
        :at-ns (- (now-ns) started)
        :thread (.getName (Thread/currentThread))))
    (fn
      ([]
       (update-count events :zero-arity)
       (rf))
      ([acc]
       (update-count events :completion)
       (rf acc))
      ([acc item]
       (update-count events :steps)
       (rf acc item)))))

(defn completion-transducer
  "Emit a marker from completion so empty-input behavior is visible."
  [events]
  (fn [rf]
    (update-count events :applications)
    (fn
      ([]
       (update-count events :zero-arity)
       (rf))
      ([acc]
       (update-count events :completion)
       (rf acc :completed))
      ([acc item]
       (update-count events :steps)
       (rf acc item)))))

(defn early-transducer
  "A small transducer which returns Reduced after the requested step count."
  [events limit]
  (fn [rf]
    (update-count events :applications)
    (let [remaining (volatile! limit)]
      (fn
        ([]
         (update-count events :zero-arity)
         (rf))
        ([acc]
         (update-count events :completion)
         (rf acc))
        ([acc item]
         (let [before @remaining
               _ (vswap! remaining dec)
               result (do
                        (update-count events :steps)
                        (rf acc item))]
           (if (<= before 1)
             (reduced result)
             result)))))))

(defn realize-values
  [value]
  (vec (seq value)))

(defn result-surface
  [build]
  (outcome
    (fn []
      (let [result (build)]
        (sorted-map
          :class (class-name result)
          :seq? (seq? result)
          :sequential? (sequential? result)
          :count (outcome (fn [] (count result)))
          :vec (outcome (fn [] (vec result))))))))

(defn source-construction-trace
  [build]
  (let [events (atom [])
        started (now-ns)
        built (outcome #(build (trace-source events [1 2])))
        after-build (now-ns)
        after-construction @events
        realized (when (= :ok (:status built))
                   (outcome #(first (seq (:value built)))))
        after-realization (now-ns)]
    (sorted-map
      :construction-ns (- after-build started)
      :first-realization-ns (- after-realization after-build)
      :construction-events after-construction
      :construction-seq-count (count after-construction)
      :after-first-realization-events @events
      :after-first-realization-seq-count (count @events)
      :result (if (= :ok (:status built))
                (sorted-map
                  :class (class-name (:value built))
                  :realization realized)
                built))))

(defn transducer-application-observation
  [build]
  (let [events (atom {})
        started (now-ns)
        built (outcome #(build (traced-transducer events started)))
        after-build (now-ns)
        after-construction @events
        first-value (when (= :ok (:status built))
                      (outcome #(first (seq (:value built)))))
        after-first-realization (now-ns)
        after-first @events
        all-values (when (= :ok (:status built))
                     (outcome #(realize-values (:value built))))
        after-full-realization (now-ns)
        after-full @events]
    (sorted-map
      :result (if (= :ok (:status built))
                (sorted-map :class (class-name (:value built)))
                built)
      :application-count-after-construction
      (count (:applications after-construction))
      :application-count-after-first
      (count (:applications after-first))
      :application-count-after-full
      (count (:applications after-full))
      :events-after-construction after-construction
      :events-after-first after-first
      :events-after-full after-full
      :first-realization first-value
      :full-realization all-values
      :timing-ns (sorted-map
                   :construction (- after-build started)
                   :first-realization (- after-first-realization after-build)
                   :full-realization (- after-full-realization
                                        after-first-realization)))))

(defn empty-completion-observation
  [build]
  (let [events (atom {})
        built (outcome #(build (completion-transducer events)))
        realized (when (= :ok (:status built))
                   (outcome #(realize-values (:value built))))]
    (sorted-map
      :result (if (= :ok (:status built))
                (sorted-map :class (class-name (:value built)))
                built)
      :events @events
      :values realized)))

(defn early-reduction-observation
  [build source-kind source]
  (let [events (atom {})
        built (outcome #(build (early-transducer events 2) source))
        realized (when (= :ok (:status built))
                   (outcome #(realize-values (:value built))))]
    (sorted-map
      :source source-kind
      :source-class (class-name source)
      :result (if (= :ok (:status built))
                (sorted-map :class (class-name (:value built)))
                built)
      :events @events
      :values realized)))

(defn consume-reducing-function
  [events]
  ;; Keep the reducing function generic.  The legacy consume implementation
  ;; only recognizes primitive IFn interfaces when deciding whether to call
  ;; one-arity completion, so this makes the missing completion observable.
  (fn
    ([]
     (swap! events conj :zero-arity)
     (long 700))
    ([acc]
     (swap! events conj [:completion acc])
     (+ acc 100))
    ([acc item]
     (swap! events conj [:step acc item])
     (+ acc item))))

(defn consume-observation []
  (let [oracle-events (atom [])
        oracle (outcome
                 #(clj/transduce (clj/map inc)
                    (consume-reducing-function oracle-events)
                    (long 0)
                    [1]))
        legacy-events (atom [])
        legacy-result (outcome
                        #(legacy/consume
                           (consume-reducing-function legacy-events)
                           (long 0)
                           (legacy/map inc [1])))]
    (sorted-map
      :oracle (sorted-map :result oracle :events @oracle-events)
      :legacy (sorted-map :result legacy-result :events @legacy-events)
      :classification :expected-difference
      :difference-label :missing-reducing-function-completion)))

(defn drain-observation []
  (let [oracle (outcome #(vec (clj/map inc (clj/map inc [1 2]))))
        legacy-result
        (outcome
          #(let [first-stage (legacy/map inc [1 2])
                 second-stage (legacy/map inc first-stage)
                 drained (legacy/drain second-stage)
                 values (vec (seq drained))
                 first-stage-after (outcome (fn [] (vec (seq first-stage))))]
             (sorted-map
               :values values
               :drained-class (class-name drained)
               :first-stage-after first-stage-after)))]
    (sorted-map
      :oracle oracle
      :legacy legacy-result
      :classification :expected-difference
      :difference-label :two-stage-drain-loses-transformation)))

(defn arity-data
  [v]
  (let [arglists (:arglists (meta v))]
    (sorted-map
      :arglists (pr-str arglists)
      :arity-counts (vec (clj/map count arglists)))))

(defn public-arities-observation []
  (sorted-map
    :map (sorted-map :core (arity-data #'clj/map)
                     :legacy (arity-data #'legacy/map))
    :filter (sorted-map :core (arity-data #'clj/filter)
                        :legacy (arity-data #'legacy/filter))
    :remove (sorted-map :core (arity-data #'clj/remove)
                        :legacy (arity-data #'legacy/remove))
    :take (sorted-map :core (arity-data #'clj/take)
                      :legacy (arity-data #'legacy/take))))

(defn clean-classpath-observation
  [clean-classpath source-root]
  (let [java (str (System/getProperty "java.home")
                  java.io.File/separator "bin" java.io.File/separator "java")
        expression "(require 'xfseq.core)"
        command [java "-cp" clean-classpath "clojure.main" "-e" expression]]
    (try
      (let [process (doto (ProcessBuilder. (into-array String command))
                      (.redirectErrorStream true))
            _ (.directory process (io/file (System/getProperty "user.dir")))
            started (now-ns)
            process (.start process)
            output (slurp (.getInputStream process))
            exit-status (.waitFor process)]
        (sorted-map
          :command (clj/pr-str command)
          :source-root source-root
          :classpath clean-classpath
          :exit-status exit-status
          :elapsed-ns (- (now-ns) started)
          :exception-class
          (when (re-find #"ClassNotFoundException" output)
            "java.lang.ClassNotFoundException")
          :missing-class
          (when (re-find #"xfseq\.ILongSeq" output)
            "xfseq.ILongSeq")
          :output output))
      (catch Throwable error
        (sorted-map
          :command (clj/pr-str command)
          :source-root source-root
          :classpath clean-classpath
          :process (exception-data error))))))

(defn legacy-suite-observation []
  (require 'xfseq.core-test)
  (let [report (java.io.StringWriter.)
        summary (binding [*out* report
                          test/*test-out* report]
                  (test/run-tests 'xfseq.core-test))]
    (sorted-map
      :namespace "xfseq.core-test"
      :summary (sorted-map
                 :test (:test summary)
                 :pass (:pass summary)
                 :fail (:fail summary)
                 :error (:error summary))
      :exact-output (str report))))

(defn metadata-observation
  [options]
  (let [direct-linking-property
        (System/getProperty "clojure.compiler.direct-linking")]
    (sorted-map
      :phase "phase-0"
      :runner "dev/xfseq/phase_0_characterize.clj"
      :clojure-version (clojure-version)
      :requested-clojure-version (:clojure-version options)
      :cli-version (:cli-version options)
      :java-version (System/getProperty "java.version")
      :java-runtime-version (System/getProperty "java.runtime.version")
      :java-vm-name (System/getProperty "java.vm.name")
      :java-vm-vendor (System/getProperty "java.vm.vendor")
      :java-home (System/getProperty "java.home")
      :os-name (System/getProperty "os.name")
      :os-version (System/getProperty "os.version")
      :architecture (System/getProperty "os.arch")
      :data-model (System/getProperty "sun.arch.data.model")
      :available-processors (.availableProcessors (Runtime/getRuntime))
      :direct-linking-property direct-linking-property
      :direct-linking-enabled (= "true" direct-linking-property)
      :timing-note "System/nanoTime diagnostics only; this report is not performance evidence"
      :working-directory (System/getProperty "user.dir")
      :classpath (System/getProperty "java.class.path")
      :command (:command options)
      :exit-status 0
      :preservation-sha preservation-sha
      :preservation-tag preservation-tag)))

(defn parse-options
  [args]
  (loop [options {}
         remaining args]
    (if (empty? remaining)
      options
      (let [flag (first remaining)
            value (second remaining)]
        (if-not (and (string? flag) (= \- (first flag))
                     (string? value))
          (throw (IllegalArgumentException.
                   (str "Expected --option value, got " (pr-str remaining))))
          (recur (assoc options
                   (keyword (subs flag 2))
                   value)
                 (nnext remaining)))))))

(defn report
  [options]
  (let [surface (sorted-map
                  :core-direct
                  (result-surface #(clj/map inc [1 2]))
                  :core-sequence
                  (result-surface #(clj/sequence (clj/map inc) [1 2]))
                  :xfseq-core
                  (result-surface #(legacy/map inc [1 2]))
                  :xfseq-gen
                  (result-surface #(asm/xf-seq (clj/map inc) [1 2])))
        source-trace (sorted-map
                       :core-direct
                       (source-construction-trace
                         #(clj/map inc %))
                       :xfseq-core
                       (source-construction-trace
                         #(legacy/xf-seq (clj/map inc) %))
                       :xfseq-gen
                       (source-construction-trace
                         #(asm/xf-seq (clj/map inc) %)))
        applications (sorted-map
                       :core-sequence
                       (transducer-application-observation
                         #(clj/sequence % [1 2 3]))
                       :xfseq-core
                       (transducer-application-observation
                         #(legacy/xf-seq % [1 2 3]))
                       :xfseq-gen
                       (transducer-application-observation
                         #(asm/xf-seq % [1 2 3])))
        empty-completion (sorted-map
                           :core-sequence
                           (empty-completion-observation
                             #(clj/sequence % []))
                           :xfseq-core
                           (empty-completion-observation
                             #(legacy/xf-seq % []))
                           :xfseq-gen
                           (empty-completion-observation
                             #(asm/xf-seq % [])))
        chunked (vec [0 1 2 3])
        dechunked (apply list chunked)
        early (sorted-map
                :chunked
                (sorted-map
                  :core-sequence
                  (early-reduction-observation
                    #(clj/sequence % %2) :chunked chunked)
                  :xfseq-core
                  (early-reduction-observation
                    #(legacy/xf-seq % %2) :chunked chunked)
                  :xfseq-gen
                  (early-reduction-observation
                    #(asm/xf-seq % %2) :chunked chunked))
                :dechunked
                (sorted-map
                  :core-sequence
                  (early-reduction-observation
                    #(clj/sequence % %2) :dechunked dechunked)
                  :xfseq-core
                  (early-reduction-observation
                    #(legacy/xf-seq % %2) :dechunked dechunked)
                  :xfseq-gen
                  (early-reduction-observation
                    #(asm/xf-seq % %2) :dechunked dechunked)))]
    (sorted-map
      :schema-version 1
      :metadata (metadata-observation options)
      :legacy-suite (legacy-suite-observation)
      :cases
      (sorted-map
        :result-surface
        (sorted-map
          :classification :expected-difference
          :difference-label :xfseq-head-incomplete-sequence-surface
          :observations surface)
        :construction-source-trace
        (sorted-map
          :classification :expected-difference
          :difference-label :asm-touches-source-during-construction
          :observations source-trace)
        :transducer-application-count-and-timing
        (sorted-map
          :classification :expected-difference
          :difference-label :legacy-paths-apply-transducer-more-than-once
          :observations applications)
        :empty-completion
        (sorted-map
          :classification :expected-difference
          :difference-label :legacy-loses-empty-completion-output
          :observations empty-completion)
        :early-reduction
        (sorted-map
          :classification :historical-probe
          :difference-label :direct-reduced-observation-by-input-shape
          :observations early)
        :consume-completion
        (consume-observation)
        :drain-two-stage
        (drain-observation)
        :public-arities
        (sorted-map
          :classification :expected-difference
          :difference-label :legacy-public-arities-incomplete
          :observations (public-arities-observation))
        :clean-declared-classpath-require
        (sorted-map
          :classification :expected-failure
          :difference-label :legacy-checkout-has-no-compiled-java-output
          :observation (clean-classpath-observation
                         (:clean-classpath options)
                         (:source-root options)))))))

(defn -main
  [& args]
  (let [options (parse-options args)
        output (:output options)
        data (report options)
        rendered (str (prn-str data))]
    (if output
      (do
        (io/make-parents output)
        (spit output rendered))
      (print rendered))
    (flush)
    (shutdown-agents)))
