(ns xfseq.phase-1-lint
  (:require [clj-kondo.core :as kondo]))

(def lint-paths ["src" "dev" "test" "build.clj" "deps.edn"])

(defn -main [& _]
  (let [result (kondo/run! {:lint lint-paths
                             :config ".clj-kondo/config.edn"
                             :cache false
                             :repro true})]
    (kondo/print! result)
    (when-some [findings (seq (:findings result))]
      (throw (ex-info "clj-kondo found active findings"
                      {:finding-count (count findings)})))))
