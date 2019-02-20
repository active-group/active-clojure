(ns active.clojure.macro-test
  (:require #?(:clj [clojure.test :refer :all])
            #?(:clj [active.clojure.macro :refer [if-cljs]])
            #?(:cljs [cljs.test]))
  #?(:cljs
     (:require-macros [active.clojure.macro :refer [if-cljs]]
                      [cljs.test :refer (is deftest run-tests testing)])))

(deftest if-cljs-test
  #?(:cljs
     (is (= 1
            (if-cljs 1 0)))
     :clj
     (is (= 0
            (if-cljs 1 0)))))
