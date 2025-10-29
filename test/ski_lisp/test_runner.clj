(ns ski-lisp.test-runner
  (:require [clojure.test :as t]
            [ski-lisp.ski-test]
            [ski-lisp.rewrite-test]
            [ski-lisp.bracket-test])
  (:gen-class))

(defn -main [& _]
  (let [summary (t/run-all-tests #"^ski-lisp\..*")]
    (let [fail (or (:fail summary) 0)
          err  (or (:error summary) 0)]
      (shutdown-agents)
      (when (pos? (+ fail err))
        (System/exit 1)))))
