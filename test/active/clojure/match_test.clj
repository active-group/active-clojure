(ns active.clojure.match-test
  (:require [active.clojure.match :as p]
            [active.clojure.functions :as f]
            [clojure.core.match.regex]
            [clojure.test :as t]))

(t/deftest parse-clause-test
  (t/testing "key exists clause"
    (t/is (= (p/make-key-exists-clause :k p/the-existence-matcher 'k)
             (p/parse-clause :k))))

  (t/testing "key exists with binding clause"
    (t/is (= (p/make-key-exists-clause :k p/the-existence-matcher 'Binding)
             (p/parse-clause (list :k :as 'Binding)))))

  (t/testing "path exists clause"
    (t/is (= (p/make-path-exists-clause [:k "V"] p/the-existence-matcher 'V)
             (p/parse-clause [:k 'V]))))

  (t/testing "path exists clause with binding"
    (t/is (= (p/make-path-exists-clause [:k "V"] p/the-existence-matcher 'Binding)
             (p/parse-clause (list [:k 'V] :as 'Binding)))))

  (t/testing "key matches clause"
    (t/testing "with regex"
      (let [c (p/parse-clause (list :k #"foo"))]
        (t/is (= :k (p/key-matches-clause-key c)))
        (t/is (= "foo" (.pattern ^java.util.regex.Pattern (p/regex-matcher-regex (p/key-matches-clause-matcher c)))))
        (t/is (= 'k (p/key-matches-clause-binding c)))))
    (t/testing "with any other value"
      (t/is (= (p/make-key-matches-clause :k (p/make-constant-matcher "foo") 'k)
               (p/parse-clause (list :k "foo"))))))

  (t/testing "key matches clause with binding"
    (t/is (= (p/make-key-matches-clause :k (p/make-constant-matcher "foo") 'Binding)
             (p/parse-clause (list :k "foo" :as 'Binding)))))

  (t/testing "path matches clause"
    (t/is (= (p/make-path-matches-clause [:k "bar" "baz"] (p/make-constant-matcher "foo") 'baz)
             (p/parse-clause (list [:k 'bar 'baz] "foo")))))

  (t/testing "path matches clause with binding"
    (t/is (= (p/make-path-matches-clause [:k "bar" "baz"] (p/make-constant-matcher "foo") 'Binding)
             (p/parse-clause (list [:k 'bar 'baz] "foo" :as 'Binding)))))

  (t/testing "with an options matcher"
    (t/is (= (p/make-key-matches-clause :k (p/make-options-matcher [1 2 3]) 'Binding)
             (p/parse-clause (list :k (list :or 1 2 3) :as 'Binding)))))

  (t/testing "with a predicate matcher"
    (t/is (= (p/make-key-matches-clause :k (p/make-predicate-matcher even?) 'Binding)
             (p/parse-clause (list :k (list 'k :compare-fn even?) :as 'Binding))))
    (t/is (= (p/make-key-matches-clause :k (p/make-predicate-matcher (f/partial = 42)) 'Binding)
             (p/parse-clause (list :k (list 'k :compare-fn (f/partial = 42)) :as 'Binding)))))

  (t/testing "optional clauses"
    (t/is (= (p/make-optional-clause (p/make-key-exists-clause :k p/the-existence-matcher 'k))
             (p/parse-clause (list '? :k))))
    (t/is (= (p/make-optional-clause (p/make-key-exists-clause :k p/the-existence-matcher 'Binding))
             (p/parse-clause (list '? :k :as 'Binding))))
    (t/is (= (p/make-optional-clause (p/make-path-matches-clause [:k "bar" "baz"] (p/make-constant-matcher "foo") 'Binding))
             (p/parse-clause (list '? [:k 'bar 'baz] "foo" :as 'Binding))))))

(t/deftest parse-pattern-test
  (let [three '[(:kind "three")
                (:x "x" :as x)
                (:y "y")
                (:z :as z)
                :w]
        three-pattern (p/pattern 'three
                                 (p/key-matches-clause :kind (p/match-const "three"))
                                 (-> (p/key-matches-clause :x (p/match-const "x"))
                                     (p/bind-match 'x))
                                 (p/key-matches-clause :y (p/match-const "y"))
                                 (-> (p/key-exists-clause :z)
                                     (p/bind-match 'z))
                                 (p/key-exists-clause :w))]
    (t/is (= three-pattern (p/parse-pattern 'three three)))))

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
                                            (p/key-matches-clause :x (p/match-const "b")))))
  (t/is (= `[~(symbol "rebind") (get-in {:x "b"} [:x] "b")]
           (p/key-matches-clause->rhs-match {:x "b"}
                                            (-> (p/key-matches-clause :x (p/match-const "b"))
                                                (p/bind-match 'rebind))))))

(t/deftest path-matches-clause->lhs-match-test
  (t/is (= {:x {:y {:z "b"}}} (p/path-matches-clause->lhs-match (p/path-matches-clause [:x :y :z] (p/match-const "b")))))
  (t/testing "lhs doesn't care abound bindings"
    (t/is (= {:x {:y {:z "b"}}}
             (p/path-matches-clause->lhs-match (-> (p/path-matches-clause [:x :y :z] (p/match-const "b"))
                                                   (p/bind-match 'rebind)))))))

(t/deftest path-matches-clause->rhs-match-test
  (t/is (= `[~(symbol "z") (get-in {:x {:y {:z "b"}}} [:x :y :z] "b")]
           (p/path-matches-clause->rhs-match {:x {:y {:z "b"}}}
                                             (p/path-matches-clause [:x :y :z] (p/match-const "b")))))
  (t/is (= `[~(symbol "rebind") (get-in {:x {:y {:z "b"}}} [:x :y :z] "b")]
           (p/path-matches-clause->rhs-match {:x {:y {:z "b"}}}
                                             (-> (p/path-matches-clause [:x :y :z] (p/match-const "b"))
                                                 (p/bind-match 'rebind))))))

;; Imported test from active.clojure.match-test
(def one-data
  {:kind "one" :x "x" :y "y" :z "z" :w "w"})

(def two-data
  {:kind "two" :a "a" :b "b" :c "c"
   :d {"Z" 42 "Y" 23 "X" 65
       "W" {"foo" "bar"}}})

(def three-data
  {:different-kind "one" :x "x" :y "y" :z "z" :w "w"})

(p/defpattern one
  [(:kind #"one")
   (:x "x" :as x)
   (:y "y")
   (:z :as z)
   :w])

(p/defpattern two
  [(:kind #"two")
   (:a "a" :as a)
   (:b "b")
   (:c :as c)
   ([:d Z] 42 :as Z)
   ([:d Y] :as Y)
   ([:d X] 65)
   [:d W foo]])

(def example-matcher
  (p/map-matcher
   one   [x y z w]
   two   [a b c Z Y X foo]
   :else false))

(t/deftest map-matcher-test
  (t/is (= ["x" "y" "z" "w"]
           (example-matcher one-data)))
  (t/is (= ["a" "b" "c" 42 23 65 "bar"]
           (example-matcher two-data)))
  (t/is (= false (example-matcher {:kind "none"}))))

(t/deftest map-matcher-optional-test
  (t/is (= ["a" "b" "c" "C" 42 23 nil]
           ((p/map-matcher [(:kind #"two")
                            (? :a :as a)
                            (? :b)
                            (? :c "C" :as c)
                            (? :C "C" :as C)
                            (? [:d Z] :as Z)
                            (? [:d Y] "y")
                            (? [:d U])]
                           [a b c C Z Y U])
            two-data))))

(t/deftest map-matcher-regex-key-not-found-t
  (t/is (= false
           (example-matcher three-data))))

(p/defpattern one-or
  [(:kind #"one")
   (:x (:or "a" "b" "c" "x") :as x)
   (:y (:or "x" "y" "z"))
   (:z :as z)
   :w])

(def example-or-matcher
  (p/map-matcher
   one-or [x y z w]
   two [a b c Z Y X foo]
   :else false))

(t/deftest map-matcher-or-test
  (t/is (= ["x" "y" "z" "w"]
           (example-or-matcher one-data)))
  (t/is (= ["a" "b" "c" 42 23 65 "bar"]
           (example-or-matcher two-data)))
  (t/is (= false (example-matcher {:kind "none"}))))

(def predicate-matcher
  (p/map-matcher
   [(:x (x :compare-fn even?))] ::even
   [(:x (x :compare-fn odd?))] ::odd))

(t/deftest map-matcher-predicate-test
  (t/is (= ::even (predicate-matcher {:x 42})))
  (t/is (= ::odd (predicate-matcher {:x 41}))))

(p/defpattern predicate-pattern
  [(:x (x :compare-fn even?))])

(t/deftest map-matcher-polymorphism-test
  (t/testing "works with a pattern record"
    (t/is (= ::even
             ((p/map-matcher predicate-pattern ::even)
              {:x 42}))))

  (t/testing "works with pattern syntax"
    (t/is (= ::even
             ((p/map-matcher [(:x (x :compare-fn even?))] ::even)
              {:x 42})))))
