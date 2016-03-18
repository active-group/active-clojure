(ns active.clojure.test-condition-test
  #?(:cljs (:require-macros [active.clojure.test-condition]
                            [cljs.test :refer (is deftest run-tests testing)]))
  (:require #?(:clj [active.clojure.test-condition])
            #?(:clj [clojure.test :refer :all])
            #?(:cljs [cljs.test])
            [active.clojure.condition :as c]))

(deftest is-test
  (is
   (raised? c/error?
            (c/raise (c/make-error) "random error")))

  ;; failure is success
  (is
   (raised? c/error? 23))

  (is
   (raised? c/error? (c/raise (c/make-warning) "not an error"))))
