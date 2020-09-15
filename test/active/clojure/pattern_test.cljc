(ns active.clojure.match-test
  (:require [active.clojure.pattern :as p]
            #?(:clj  [clojure.test :as t :refer :all]
               :cljs [cljs.test :as t :include-macros true])))

;; Tests from `active.clojure.match-test`

(def one-data {:kind "one"
               :x    "x"
               :y    "y"
               :z    "z"
               :w    "w"})

(def two-data
  {:kind "two"
   :a    "a"
   :b    "b"
   :c    "c"
   :d    {"Z" 42
          "Y" 23
          "X" 65
          "W" {"foo"
               "bar"}}})

(def three-data
  {:different-kind
   "one"
   :x "x"
   :y "y"
   :z "z"
   :w "w"})

;; (defpattern one
;;   [(:kind #"one")
;;    (:x "x" :as x)
;;    (:y "y")
;;    (:z :as z)
;;    :w])

(def one
  (p/pattern 'one
             (p/key-matches-clause :kind (p/match-regex #"one"))
             (-> (p/key-matches-clause :x (p/match-const "x"))
                 (p/bind-match 'x))
             (p/key-matches-clause :y (p/match-const "y"))
             (-> (p/key-exists-clause :z)
                 (p/bind-match 'z))
             (p/key-exists-clause :w)))

;; (defpattern two
;;   [(:kind #"two")
;;    (:a "a" :as a)
;;    (:b "b")
;;    (:c :as c)
;;    ([:d Z] 42 :as Z)
;;    ([:d Y] :as Y)
;;    ([:d X] 65)
;;    [:d W foo]])
(def two
  (p/pattern 'two
             (p/key-matches-clause :kind (p/match-regex #"two"))
             (-> (p/key-matches-clause :a (p/match-const "a"))
                 (p/bind-match 'a))
             (p/key-matches-clause :b (p/match-const "b"))
             (-> (p/key-exists-clause :c)
                 (p/bind-match 'c))
             (-> (p/path-matches-clause [:d 'Z] (p/match-const 42))
                 (p/bind-match 'Z))
             (-> (p/path-exists-clause [:d 'Y])
                 (p/bind-match 'Y))
             (p/path-matches-clause [:d 'X] (p/match-const 65))
             (p/path-exists-clause [:d 'W 'foo])))
