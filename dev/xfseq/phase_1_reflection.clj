(ns xfseq.phase-1-reflection
  (:import [java.io StringWriter]))

(set! *warn-on-reflection* true)

(def namespaces
  '[xfseq.protocols
    xfseq.analyze
    xfseq.core
    xfseq.gen
    xfseq.phase-0-characterize
    xfseq.phase-0-bench
    xfseq.core-test
    xfseq.bench])

(defn -main [& _]
  (let [warnings (StringWriter.)]
    (binding [*err* warnings
              *warn-on-reflection* true]
      (doseq [namespace-sym namespaces]
        (require namespace-sym :reload)))
    (let [output (str warnings)]
      (print output)
      (if (seq output)
        (throw (ex-info "Compiler reflection warnings found"
                        {:warnings output}))
        (println "Compiler reflection check passed.")))))
