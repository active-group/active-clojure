(ns active.clojure.match-test
  (:require #?(:clj [active.clojure.match :refer (defpattern map-matcher)])
            #?(:clj [clojure.test :refer :all])
            #?(:cljs [cljs.test :as t]))
  #?(:cljs
  (:require-macros [cljs.test
                    :refer (is deftest run-tests testing)]
                   [active.clojure.match :refer (map-matcher)])))

#?(:cljs
(enable-console-print!))

(def one-data
  {:kind "one" :x "x" :y "y" :z "z" :w "w"})

(def two-data
  {:kind "two" :a "a" :b "b" :c "c"
   :d {"Z" 42 "Y" 23 "X" 65
       "W" {"foo" "bar"}}})

#?(:clj
(defpattern one
  [(:kind #"one")
   (:x "x" :as x)
   (:y "y")
   (:z :as z)
   :w]))

#?(:clj
(defpattern two
  [(:kind #"two")
   (:a "a" :as a)
   (:b "b")
   (:c :as c)
   ([:d Z] 42 :as Z)
   ([:d Y] :as Y)
   ([:d X] 65)
   [:d W foo]]))

#?(:clj
(def example-matcher
  (map-matcher
   one [x (str y) z w]
   two [a b c Z Y X foo]
   :else false)))

#?(:clj
(deftest t-map-matcher
  (is (= ["x" "y" "z" "w"]
         (example-matcher one-data)))
  (is (= ["a" "b" "c" 42 23 65 "bar"]
         (example-matcher two-data)))
  (is (= false (example-matcher {:kind "none"})))))

#?(:clj
(deftest t-map-matcher-optional
  (is (= ["a" "b" "c" "C" 42 23 nil]
         ((map-matcher
           [(:kind #"two")
            (? :a :as a)
            (? :b)
            (? :c "C" :as c)
            (? :C "C" :as C)
            (? [:d Z] :as Z)
            (? [:d Y] "y")
            (? [:d U])]
           [a b c C Z Y U])
          two-data)))))
