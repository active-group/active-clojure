(ns active.clojure.figwheel-test-runner
  (:require [figwheel.main.testing :refer-macros [run-tests-async]]
            [active.clojure.test-runner]))

(defn -main [& args]
  (run-tests-async 10000))
