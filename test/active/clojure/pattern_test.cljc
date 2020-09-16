(ns active.clojure.pattern-test
  (:require [active.clojure.pattern :as p]
            #?(:clj  [clojure.test :as t :refer :all]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest parse-clause-test
  (t/testing "key exists clause"
    (t/is (= (p/make-key-exists-clause :k p/the-existence-matcher 'k)
             (p/parse-clause :k))))

  (t/testing "key exists with binding clause"
    (t/is (= (p/make-key-exists-clause :k p/the-existence-matcher 'Binding)
             (p/parse-clause (list :k :as 'Binding)))))

  (t/testing "path exists clause"
    (t/is (= (p/make-path-exists-clause [:k 'V] p/the-existence-matcher 'V)
             (p/parse-clause [:k 'V]))))

  (t/testing "path exists clause with binding"
    (t/is (= (p/make-path-exists-clause [:k 'V] p/the-existence-matcher 'Binding)
             (p/parse-clause (list [:k 'V] :as 'Binding)))))

  (t/testing "key matches clause"
    (t/testing "with regex"
      (let [c (p/parse-clause (list :k #"foo"))]
        (t/is (= :k (p/key-matches-clause-key c)))
        (t/is (= "foo" #?(:clj (.pattern ^java.util.regex.Pattern (p/regex-matcher-regex (p/key-matches-clause-matcher c)))
                          :cljs (.-source (p/regex-matcher-regex (p/key-matches-clause-matcher c))))))
        (t/is (= 'k (p/key-matches-clause-binding c)))))
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
             (p/parse-clause (list [:k 'bar 'baz] "foo" :as 'Binding)))))

  (t/testing "optional clauses"
    (t/is (= (p/make-optional-clause (p/make-key-exists-clause :k p/the-existence-matcher 'k))
             (p/parse-clause (list '? :k))))
    (t/is (= (p/make-optional-clause (p/make-key-exists-clause :k p/the-existence-matcher 'Binding))
             (p/parse-clause (list '? :k :as 'Binding))))
    (t/is (= (p/make-optional-clause (p/make-path-matches-clause [:k 'bar 'baz] (p/make-constant-matcher "foo") 'Binding))
             (p/parse-clause (list '? [:k 'bar 'baz] "foo" :as 'Binding))))))

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

(t/deftest key-exists-clause->match-test
  (t/is (= {:x 'x} (p/key-exists-clause->match (p/key-exists-clause :x))))
  (t/is (= {:x 'rebind}
           (p/key-exists-clause->match (-> (p/key-exists-clause :x)
                                           (p/bind-match 'rebind))))))

(t/deftest path-exists-clause->match-test
  (t/is (= {:x {"b" {"c" 'c}}}
           (p/path-exists-clause->match (p/path-exists-clause [:x "b" "c"]))))
  (t/is (= {:x {"b" {"c" 'rebind}}}
           (p/path-exists-clause->match (-> (p/path-exists-clause [:x "b" "c"])
                                            (p/bind-match 'rebind))))))

(t/deftest key-matches-clause->lhs-match-test
  (t/is (= {:x "b"} (p/key-matches-clause->lhs-match (p/key-matches-clause :x (p/match-const "b")))))
  (t/testing "lhs doesn't care abound bindings"
    (t/is (= {:x "b"} (p/key-matches-clause->lhs-match (-> (p/key-matches-clause :x (p/match-const "b"))
                                                           (p/bind-match 'rebind)))))))

(t/deftest key-matches-clause->rhs-match-test
  (t/is (= `[~(symbol "x") (get-in {:x "b"} [:x] "b")]
           (p/key-matches-clause->rhs-match {:x "b"}
                                            (p/key-matches-clause :x (p/match-const "b"))
                                            'x)))
  (t/is (= `[~(symbol "rebind") (get-in {:x "b"} [:x] "b")]
           (p/key-matches-clause->rhs-match {:x "b"}
                                            (-> (p/key-matches-clause :x (p/match-const "b"))
                                                (p/bind-match 'rebind))
                                            'rebind))))

(t/deftest path-matches-clause->lhs-match-test
  (t/is (= {:x {:y {:z "b"}}} (p/path-matches-clause->lhs-match (p/path-matches-clause [:x :y :z] (p/match-const "b")))))
  (t/testing "lhs doesn't care abound bindings"
    (t/is (= {:x {:y {:z "b"}}}
             (p/path-matches-clause->lhs-match (-> (p/path-matches-clause [:x :y :z] (p/match-const "b"))
                                                   (p/bind-match 'rebind)))))))

(t/deftest path-matches-clause->rhs-match-test
  (t/is (= `[~(symbol "z") (get-in {:x {:y {:z "b"}}} [:x :y :z] "b")]
           (p/path-matches-clause->rhs-match {:x {:y {:z "b"}}}
                                             (p/path-matches-clause [:x :y :z] (p/match-const "b"))
                                             'z)))
  (t/is (= `[~(symbol "rebind") (get-in {:x {:y {:z "b"}}} [:x :y :z] "b")]
           (p/path-matches-clause->rhs-match {:x {:y {:z "b"}}}
                                             (-> (p/path-matches-clause [:x :y :z] (p/match-const "b"))
                                                 (p/bind-match 'rebind))
                                             'rebind))))

;; Imported test from [[active.clojure.match-test]
(def one-data
  {:kind "one" :x "x" :y "y" :z "z" :w "w"})

(def two-data
  {:kind "two" :a "a" :b "b" :c "c"
   :d {"Z" 42 "Y" 23 "X" 65
       "W" {"foo" "bar"}}})

(def three-data
  {:different-kind "one" :x "x" :y "y" :z "z" :w "w"})

(def one
  '[(:kind #"one")
    (:x "x" :as x)
    (:y "y")
    (:z :as z)
    :w])

(def two
  '[(:kind #"two")
    (:a "a" :as a)
    (:b "b")
    (:c :as c)
    ([:d Z] 42 :as Z)
    ([:d Y] :as Y)
    ([:d X] 65)
    [:d W foo]])

(def example-matcher
  (p/map-matcher
   one [x y z w]
   two [a b c Z Y X foo]
   :else false))

(t/deftest map-matcher-test
  (t/is (= ["x" "y" "z" "w"]
           (example-matcher one-data)))
  (t/is (= ["a" "b" "c" 42 23 65 "bar"]
           (example-matcher two-data)))
  (t/is (= false (example-matcher {:kind "none"}))))
