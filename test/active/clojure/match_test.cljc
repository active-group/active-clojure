(ns active.clojure.match-test
  (:require [active.clojure.match :as match]
            #?(:clj [clojure.test :refer :all])
            #?(:cljs [cljs.test :as t]))
  #?(:cljs
  (:require-macros [cljs.test
                    :refer (is deftest run-tests testing)])))

#?(:cljs
(enable-console-print!))

(def one-data
  {:kind "one" :x "x" :y "y" :z "z" :w "w"})

(match/defpattern one
  [(:kind #"one")
   (:x "x" :as x)
   (:y "y")
   (:z :as z)
   :w])

(def two-data
  {:kind "two" :a "a" :b "b" :c "c"
   :d {"Z" 42 "Y" 23 "X" 65
       "W" {"foo" "bar"}}})

(match/defpattern two
  [(:kind #"two")
   (:a "a" :as a)
   (:b "b")
   (:c :as c)
   ([:d Z] 42 :as Z)
   ([:d Y] :as Y)
   ([:d X] 65)
   [:d W foo]])

(def example-matcher
  (match/map-matcher
   one [x (str y) z w]
   two [a b c Z Y X foo]
   :else false))

(deftest t-map-matcher
  (is (= ["x" "y" "z" "w"]
         (example-matcher one-data)))
  (is (= ["a" "b" "c" 42 23 65 "bar"]
         (example-matcher two-data)))
  (is (= false (example-matcher {:kind "none"}))))
