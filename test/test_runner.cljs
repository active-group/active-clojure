(ns test-runner
  (:require
   [cljs.test :refer-macros [run-all-tests]]
   [active.clojure.debug-test]
   [active.clojure.lens-test]
   [active.clojure.record-test]
   [active.clojure.condition-test]
   [active.clojure.monad-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful? (run-all-tests))
    0
    1))
