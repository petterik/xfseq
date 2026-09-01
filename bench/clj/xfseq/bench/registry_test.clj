(ns xfseq.bench.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [xfseq.bench.registry :as registry]))

(set! *warn-on-reflection* true)

(defn- sample-row
  [score-error]
  {:benchmark "xfseq.bench.Sample"
   :mode "thrpt"
   :jmhVersion "1.37"
   :primaryMetric {:score 1.0
                   :scoreError score-error}
   :params {}})

(defn- temporary-path
  [suffix]
  (let [file (java.io.File/createTempFile "xfseq-bench-registry-" suffix)]
    (.delete file)
    (.getPath file)))

(defn- test-manifest
  [profile implementations]
  {:schema-version 1
   :profile profile
   :cells [{:id "test-public-construct"
            :class "xfseq.bench.Phase2PublicBenchmark"
            :method "construct"
            :params {"implementation" (vec implementations)
                     "sourceKind" ["list"]
                     "size" ["1"]
                     "workload" ["identity"]}}]})

(defn- test-row
  [implementation]
  {:benchmark "xfseq.bench.Phase2PublicBenchmark.construct"
   :mode "thrpt"
   :jmhVersion "1.37"
   :primaryMetric {:score 1.0
                   :scoreError 0.1}
   :params {:implementation implementation
            :sourceKind "list"
            :size "1"
            :workload "identity"}})

(deftest profile-aware-score-error-validation
  (testing "smoke accepts JMH's two-sample NaN marker"
    (let [input (java.io.File/createTempFile "xfseq-bench-registry-" ".json")
          smoke-output (str (.getPath input) ".smoke")
          decision-output (str (.getPath input) ".decision")]
      (.delete input)
      (try
        (registry/write-json-new! (.getPath input) [(sample-row "NaN")])
        (is (map? (registry/validate-result! (.getPath input) :smoke)))
        (is (thrown? clojure.lang.ExceptionInfo
                     (registry/validate-result! (.getPath input))))
        (is (map? (registry/merge-smoke-results!
                    smoke-output
                    [(.getPath input)])))
        (is (thrown? clojure.lang.ExceptionInfo
                     (registry/merge-results!
                       decision-output
                       [(.getPath input)])))
        (is (not (.exists (java.io.File. decision-output))))
        (finally
          (doseq [file [input
                        (java.io.File. smoke-output)
                        (java.io.File. decision-output)]]
            (.delete ^java.io.File file)))))))

(deftest checked-in-manifests-are-explicit-and-applicable
  (let [screen (registry/read-manifest "bench/manifests/phase2-screen.edn")
        decision (registry/read-manifest "bench/manifests/phase2-decision.edn")]
    (is (= :screen (:profile screen)))
    (is (= :decision (:profile decision)))
    ;; These counts are intentionally small subsets of the full vocabulary,
    ;; not an accidental Cartesian expansion.  Applicability is checked while
    ;; each manifest is read (including specialized source-shape candidates).
    (is (= 98 (count (registry/manifest-identities screen))))
    (is (= 93 (count (registry/manifest-identities decision))))
    (is (every? #(contains? #{"xfseq.bench.Phase2PublicBenchmark"
                              "xfseq.bench.Phase2JavaBenchmark"
                              "xfseq.bench.Phase2BufferBenchmark"}
                         (:class %))
                (:cells screen)))))

(deftest phase2-profile-settings-are-explicit
  (let [decision (:decision registry/profiles)
        decision-gc (:decision-gc registry/profiles)
        expected-jvm-opts ["-Xms2g" "-Xmx2g" "-XX:+UseG1GC"]]
    (testing "unprofiled decision evidence has no allocation profiler"
      (is (= 3 (:forks decision)))
      (is (= 5 (:warmups decision)))
      (is (= 5 (:measurements decision)))
      (is (= expected-jvm-opts (:jvm-opts decision)))
      (is (nil? (:allocation-profiler decision))))
    (testing "GC evidence is a separate, explicitly profiled lane"
      (is (= 3 (:forks decision-gc)))
      (is (= 5 (:warmups decision-gc)))
      (is (= 5 (:measurements decision-gc)))
      (is (= expected-jvm-opts (:jvm-opts decision-gc)))
      (is (= "gc" (:allocation-profiler decision-gc))))))

(deftest manifest-profile-and-exactness-validation
  (let [manifest-file (temporary-path ".edn")
        result-file (temporary-path ".json")
        gc-result-file (temporary-path ".gc.json")
        two-manifest-file (temporary-path ".two.edn")
        duplicate-file (temporary-path ".duplicate.json")
        short-file (temporary-path ".short.json")
        duplicate-target (temporary-path ".duplicate-merged.json")
        short-target (temporary-path ".short-merged.json")]
    (try
      ;; A one-row decision manifest gives a compact fixture for profile
      ;; compatibility, including the intentional decision-GC alias.
      (registry/write-edn-new!
        manifest-file
        (test-manifest :decision ["xfseq"]))
      (registry/write-json-new! result-file [(test-row "xfseq")])
      (let [error (try
                    (registry/validate-manifest!
                      result-file manifest-file :screen)
                    nil
                    (catch clojure.lang.ExceptionInfo error
                      error))]
        (is (instance? clojure.lang.ExceptionInfo error))
        (is (= :screen (:profile (ex-data error))))
        (is (= :decision (:manifest-profile (ex-data error)))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (registry/validate-manifest!
                     result-file manifest-file :decision-gc)))
      (registry/write-json-new!
        gc-result-file
        [(assoc (test-row "xfseq")
                :secondaryMetrics
                {:gc.alloc.rate.norm {:score 1.0
                                      :scoreError 0.1}})])
      (is (map? (registry/validate-manifest!
                  gc-result-file manifest-file :decision-gc)))

      ;; Two expanded identities make the duplicate and row-count failures
      ;; independently observable before a durable merge target is reserved.
      (registry/write-edn-new!
        two-manifest-file
        (test-manifest :decision ["xfseq" "sequence"]))
      (registry/write-json-new!
        duplicate-file
        [(test-row "xfseq") (test-row "xfseq")])
      (registry/write-json-new! short-file [(test-row "xfseq")])
      (let [duplicate-error (try
                              (registry/merge-manifest-results!
                                duplicate-target
                                [duplicate-file]
                                two-manifest-file
                                :decision)
                              nil
                              (catch clojure.lang.ExceptionInfo error
                                error))
            short-error (try
                          (registry/merge-manifest-results!
                            short-target
                            [short-file]
                            two-manifest-file
                            :decision)
                          nil
                          (catch clojure.lang.ExceptionInfo error
                            error))]
        (is (instance? clojure.lang.ExceptionInfo duplicate-error))
        (is (= [{:identity ["xfseq.bench.Phase2PublicBenchmark"
                            "construct"
                            {"implementation" "xfseq"
                             "sourceKind" "list"
                             "size" "1"
                             "workload" "identity"}]
                     :occurrences 2}]
               (:duplicates (ex-data duplicate-error))))
        (is (instance? clojure.lang.ExceptionInfo short-error))
        (is (= 2 (:expected-count (ex-data short-error))))
        (is (= 1 (:actual-count (ex-data short-error))))
        (is (not (.exists ^java.io.File (io/file duplicate-target))))
        (is (not (.exists ^java.io.File (io/file short-target)))))
      (finally
        (doseq [file [manifest-file two-manifest-file result-file gc-result-file
                      duplicate-file short-file
                      duplicate-target short-target]]
          (.delete ^java.io.File (io/file file)))))))
