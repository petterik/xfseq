(ns xfseq.bench.registry-test
  (:require [clojure.test :refer [deftest is testing]]
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
