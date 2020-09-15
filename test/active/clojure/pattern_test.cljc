(ns active.clojure.pattern-test
  (:require [active.clojure.pattern :as p]
            #?(:clj  [clojure.test :as t :refer :all]
               :cljs [cljs.test :as t :include-macros true])))

;; Tests from `active.clojure.match-test`

(def one-data (quote {:kind "one"
                      :x    "x"
                      :y    "y"
                      :z    "z"
                      :w    "w"}))

(def two-data
  (quote {:kind "two"
          :a    "a"
          :b    "b"
          :c    "c"
          :d    {"Z" 42
                 "Y" 23
                 "X" 65
                 "W" {"foo"
                      "bar"}}}))

(def three-data
  (quote {:different-kind
          "one"
          :x "x"
          :y "y"
          :z "z"
          :w "w"}))

(def one
  (quote [(:kind #"one")
          (:x "x" :as x)
          (:y "y")
          (:z :as z)
          :w]))

(def one-pattern
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

(t/deftest parse-clause-test
  (t/testing "key exists clause"
    (t/is (= (p/make-key-exists-clause :k 'k)
             (p/parse-clause :k))))

  (t/testing "key exists with binding clause"
    (t/is (= (p/make-key-exists-clause :k 'Binding)
             (p/parse-clause (list :k :as 'Binding)))))

  (t/testing "path exists clause"
    (t/is (= (p/make-path-exists-clause [:k 'V] 'V)
             (p/parse-clause [:k 'V]))))

  (t/testing "path exists clause with binding"
    (t/is (= (p/make-path-exists-clause [:k 'V] 'Binding)
             (p/parse-clause (list [:k 'V] :as 'Binding)))))

  (t/testing "key matches clause"
    (t/testing "with regex"
      (let [c (p/parse-clause (list :k #"foo"))]
        (t/is (= :k (p/key-matches-clause-key c)))
        (t/is (= "foo" #?(:clj (.pattern ^java.util.regex.Pattern (p/regex-matcher-regex (p/key-matches-clause-matcher c)))
                          :cljs (.-source (p/regex-matcher-regex (p/key-matches-clause-matcher c))))))
        (t/is (= 'k  (p/key-matches-clause-binding c)))))
    (t/testing "with any other value"
      (t/is (= (p/make-key-matches-clause :k (p/make-constant-matcher "foo") 'k)
               (p/parse-clause (list :k "foo"))))))

  (t/testing "key matches clause with binding"
    (t/is (= (p/make-key-matches-clause :k (p/make-constant-matcher "foo") 'Binding)
             (p/parse-clause (list :k "foo" :as 'Binding)))))

  (t/testing "path matches clause"
    (t/is (= (p/make-path-matches-clause [:k 'bar 'baz] (p/make-constant-matcher "foo") 'baz)
             (p/parse-clause (list [:k 'bar 'baz] "foo")))))

  (t/testing "path matches clause with binding"
    (t/is (= (p/make-path-matches-clause [:k 'bar 'baz] (p/make-constant-matcher "foo") 'Binding)
             (p/parse-clause (list [:k 'bar 'baz] "foo" :as 'Binding))))))

(def three
  (quote [(:kind "three")
          (:x "x" :as x)
          (:y "y")
          (:z :as z)
          :w]))

(def three-pattern
  (p/pattern 'three
             (p/key-matches-clause :kind (p/match-const "three"))
             (-> (p/key-matches-clause :x (p/match-const "x"))
                 (p/bind-match 'x))
             (p/key-matches-clause :y (p/match-const "y"))
             (-> (p/key-exists-clause :z)
                 (p/bind-match 'z))
             (p/key-exists-clause :w)))

(t/deftest parse-pattern-test
  (t/is (= three-pattern (p/parse-pattern 'three three))))
