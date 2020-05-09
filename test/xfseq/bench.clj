(ns xfseq.bench
  (:require
    [criterium.core :as crit]
    [xfseq.core :as core]
    [xfseq.gen :as gen]))

(def size (long 1e4))

(defn run-bench []
  (let [objs (repeat size (Long. 2))
        v-objs (vec objs)

        rang (range 0 (long size))
        v-rang (vec rang)
        s-rang (set rang)

        arr (object-array objs)
        l-arr (long-array size)
        d-arr (double-array size)

        nil-rf (fn [a _] a)
        long-identity (fn ^long [^long l] l)
        double-identity (fn ^double [^double l] l)

        bench [[:objs objs identity]
               [:v-objs v-objs identity]
               [:rang rang long-identity]
               [:v-rang v-rang long-identity]
               [:s-rang s-rang long-identity]
               [:arr arr identity]
               [:l-arr l-arr long-identity]
               [:d-arr d-arr double-identity]]]


    #_(do
      (prn "Start baseline")
      (prn "")

      ;; baseline
      (doseq [[id coll f] bench]
        (prn "Start: " id)
        (crit/bench
          (reduce nil-rf nil (map f coll)))
        (prn ""))

      (prn "End baseline")
      (prn "")
      (prn "==================")
      (prn ""))

    #_(do
      (prn "Start xfseq.core")
      (prn "")

      ;; xfseq clojure macro with xfseq transducers
      (doseq [[id coll f] bench]
        (prn "Start: " id)
        (crit/bench
          (reduce nil-rf nil (core/xf-seq (core/map f) coll)))
        (prn ""))

      (prn "End xfseq.core")
      (prn "")
      (prn "=================="))

    (do
      (prn "Start xfseq.gen")
      (prn "")

      ;; xfseq asm gen with xfseq transducers
     (doseq [[id coll f] bench]
       (prn "Start: " id)
       (crit/bench
         (reduce nil-rf nil (gen/xf-seq (core/map f) coll)))
       (prn ""))

      (prn "End xfseq.gen")
      (prn "")
      (prn "=================="))))

(defn -main [& args]
  (run-bench))

(comment

  (run-bench))
