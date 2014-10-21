(ns active.clojure.debug-test
  (:require #+clj [active.clojure.debug :as dbg]
            #+clj [clojure.test :refer :all]
            #+cljs [cemerick.cljs.test])
  #+cljs 
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [active.clojure.debug :as dbg]))

#+cljs
(enable-console-print!)

(deftest pret
  (let [v  [4 8 15 16 23 42 108]]
    (is (= v (dbg/pret v)))))
