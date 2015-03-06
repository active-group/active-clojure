(ns test-runner
  (:require
   #+cljs [cljs.test :refer-macros [run-all-tests]]
   [active.clojure.debug-test]
   [active.clojure.lens-test]
   [active.clojure.record-test]
   [active.clojure.condition-test]))

#+cljs (enable-console-print!)

#+cljs 
(defn runner []
  (if (cljs.test/successful? (run-all-tests))
    0
    1))
