(ns active.clojure.match-test
  (:require [active.clojure.match :as p]
            [active.clojure.functions :as f]
            [clojure.core.match.regex]
            [clojure.test :as t]))

(t/deftest parse-clause-test
  (t/testing "key exists clause"
    (t/testing "flat"
      (t/is (= (p/make-key-exists-clause :k p/the-existence-matcher "k")
               (p/parse-clause :k)))
      (t/is (= (p/make-key-exists-clause "k" p/the-existence-matcher "k")
               (p/parse-clause k))))
    (t/testing "list"
      (t/is (= (p/make-key-exists-clause :k p/the-existence-matcher "k")
               (p/parse-clause (:k))))))

  (t/testing "key exists with binding clause"
    (t/is (= (p/make-key-exists-clause :k p/the-existence-matcher "Binding")
             (p/parse-clause (:k :as Binding)))))

  (t/testing "path exists clause"
    (t/testing "flat"
      (t/is (= (p/make-path-exists-clause [:k "V"] p/the-existence-matcher "V")
               (p/parse-clause [:k V]))))
    (t/testing "list"
      (t/is (= (p/make-path-exists-clause [:k "V"] p/the-existence-matcher "V")
               (p/parse-clause [:k V])))))

  (t/testing "path exists clause with binding"
    (t/is (= (p/make-path-exists-clause [:k "V"] p/the-existence-matcher "Binding")
             (p/parse-clause ([:k V] :as Binding)))))

  (t/testing "key matches clause"
    (t/testing "with regex"
      (let [c (p/parse-clause (:k #"foo"))]
        (t/is (= :k (p/key-matches-clause-key c)))
        (t/is (= "foo" (.pattern ^java.util.regex.Pattern (p/regex-matcher-regex (p/key-matches-clause-matcher c)))))
        (t/is (= "k" (p/key-matches-clause-binding c)))))
    (t/testing "with any other value"
      (t/is (= (p/make-key-matches-clause :k (p/make-constant-matcher "foo") "k")
               (p/parse-clause (:k "foo"))))))

  (t/testing "key matches clause with binding"
    (t/is (= (p/make-key-matches-clause :k (p/make-constant-matcher "foo") "Binding")
             (p/parse-clause (:k "foo" :as Binding)))))

  (t/testing "path matches clause"
    (t/is (= (p/make-path-matches-clause [:k "bar" "baz"] (p/make-constant-matcher "foo") "baz")
             (p/parse-clause ([:k bar baz] "foo")))))

  (t/testing "path matches clause with binding"
    (t/is (= (p/make-path-matches-clause [:k "bar" "baz"] (p/make-constant-matcher "foo") "Binding")
             (p/parse-clause ([:k bar baz] "foo" :as Binding)))))

  (t/testing "with an options matcher"
    (t/is (= (p/make-key-matches-clause :k (p/make-options-matcher [1 2 3]) "Binding")
             (p/parse-clause (:k (:or 1 2 3) :as Binding)))))

  (t/testing "with a predicate matcher"
    (t/is (= (p/make-key-matches-clause :k (p/make-predicate-matcher even?) "Binding")
             (p/parse-clause (:k (:compare-fn even?) :as Binding))))
    (t/is (= (p/make-key-matches-clause :k (p/make-predicate-matcher (f/partial = 42)) "Binding")
             (p/parse-clause (:k (:compare-fn (f/partial = 42)) :as Binding))))

    ;; What about "regular" anonymous functions?
    (t/is (p/predicate-matcher?
           (p/key-matches-clause-matcher
            (p/parse-clause (:k (:compare-fn #(= % 42)) :as Binding))))))

  (t/testing "optional clauses"
    (t/is (= (p/make-optional-clause (p/make-key-exists-clause :k p/the-existence-matcher "k"))
             (p/parse-clause (? :k))))
    (t/is (= (p/make-optional-clause (p/make-key-exists-clause :k p/the-existence-matcher "Binding"))
             (p/parse-clause (? :k :as Binding))))
    (t/is (= (p/make-optional-clause (p/make-path-matches-clause [:k "bar" "baz"] (p/make-constant-matcher "foo") "Binding"))
             (p/parse-clause (? [:k bar baz] "foo" :as Binding)))))

  (t/testing "with local binding as match value"
    (t/is (= (p/make-key-matches-clause :k (p/make-constant-matcher "foo") "k")
             (let [foo "foo"]
               (p/parse-clause (:k foo)))))))

(t/deftest parse-pattern-test
  (let [three-pattern
        (p/pattern (p/key-matches-clause :kind (p/match-const "three") "kind")
                   (p/key-matches-clause :x (p/match-const "x") "x")
                   (p/key-matches-clause :y (p/match-const "y") "y")
                   (p/key-exists-clause :z "z")
                   (p/key-exists-clause :w "w"))]
    (t/is (= (p/pattern-clauses three-pattern)
             (p/pattern-clauses (p/parse-pattern [(:kind "three")
                                                  (:x "x" :as x)
                                                  (:y "y")
                                                  (:z :as z)
                                                  :w]))))))

(t/deftest clause->lhs

  (t/testing "key exists"
    (t/is (= {:x 'x} (p/clause->lhs {} (p/key-exists-clause :x "x"))))
    (t/is (= {:x 'rebind} (p/clause->lhs {} (p/key-exists-clause :x "rebind")))))

  (t/testing "path exists"
    (t/is (= {:x {"b" {"c" 'c}}}
             (p/clause->lhs {} (p/path-exists-clause [:x "b" "c"] "c"))))
    (t/is (= {:x {"b" {"c" 'rebind}}}
             (p/clause->lhs {} (p/path-exists-clause [:x "b" "c"] "rebind")))))

  (t/testing "key matches"
    (t/is (= {:x "b"} (p/clause->lhs {} (p/key-matches-clause :x (p/match-const "b") "x"))))
    (t/testing "lhs doesn't care abound bindings"
      (t/is (= {:x "b"} (p/clause->lhs {} (p/key-matches-clause :x (p/match-const "b") "rebind")))))
    #_(t/is (= `({:x ~(quote _)} :guard [(constantly (~even? (get-in {} [:x])))])
             (p/clause->lhs {} (p/key-matches-clause :x (p/match-predicate even?))))))

  (t/testing "path matches"
    (t/is (= {:x {:y {:z "b"}}} (p/clause->lhs {} (p/path-matches-clause [:x :y :z] (p/match-const "b") "z"))))
    (t/is (= {:x {:y {:z "b"}}}
             (p/clause->lhs {} (-> (p/path-matches-clause [:x :y :z] (p/match-const "b") "rebind")))))
    #_(t/is (= `({:x {:y {"Z" ~(quote _)}}} :guard [(constantly (~even? (get-in {} [:x :y "Z"])))])
             (p/clause->lhs {} (p/path-matches-clause [:x :y "Z"] (p/match-predicate even?)))))))

(t/deftest path-matches-clause->rhs-match-test
  (t/is (= `[~(symbol "z") (get-in {:x {:y {:z "b"}}} [:x :y :z] "b")]
           (p/path-matches-clause->rhs-match {:x {:y {:z "b"}}}
                                             (p/path-matches-clause [:x :y :z] (p/match-const "b") "z"))))
  (t/is (= `[~(symbol "rebind") (get-in {:x {:y {:z "b"}}} [:x :y :z] "b")]
           (p/path-matches-clause->rhs-match {:x {:y {:z "b"}}}
                                             (p/path-matches-clause [:x :y :z] (p/match-const "b") "rebind")))))

(t/deftest reduce-lhs-test
  (t/is (empty? (p/reduce-lhs [])))
  (t/is (= {:x "x"} (p/reduce-lhs [{:x "x"}])))
  (t/is (= {:x "x" :y 'y} (p/reduce-lhs [{:x "x"} {:y 'y}])))
  (t/is (= {:x "other"} (p/reduce-lhs [{:x "X"} {:x "other"}])))
  (t/is (= (list {:x "x" :y "y"} :guard [:some-guard])
           (p/reduce-lhs [(list {:x "x"} :guard [:some-guard])
                          {:y "y"}])))
  (t/is (= (list {:x "x" :y "y"})
           (p/reduce-lhs [{:x "x"} (list {:y "y"})])))

  (t/is (= (list {:x "x" :y '_ :a 42 :z '_} :guard [:guard-1 :guard-2 :guard-3])
           (p/reduce-lhs [{:x "x"}
                          (list {:y '_} :guard [:guard-1])
                          (list {:y '_} :guard [:guard-2])
                          {:a 42}
                          (list {:z '_} :guard [:guard-3])]))))


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


;;; FIXME these tests all fail because compare-fn patterns dont work.
(p/defpattern one-guard
  [(:kind #"one")
   (:x (:compare-fn #(= % (last ["a" "b" "c" "x"]))) :as x)
   (:y (:compare-fn #(= % (:y {:x "x" :y "y" :z "z"}))))
   (:z :as z)
   :w])

(macroexpand-1 '(old-match/map-matcher [(:x (:compare-fn even?))
                                        :y] x))

(def example-guard-matcher
  (p/map-matcher
   one-guard [x y z w]
   two [a b c Z Y X foo]
   :else false))

(def predicate-matcher
  (p/map-matcher
   ;; The order is important
   [(:x (:compare-fn #(string? %)))] ::string
   [(:x (:compare-fn (fn [x] (boolean? x))))] ::boolean
   [(:x (:compare-fn even?))] ::even
   [(:x (:compare-fn odd?))] ::odd))

(t/deftest map-matcher-predicate-test
  (t/is (= ::even (predicate-matcher {:x 42})))
  (t/is (= ::odd (predicate-matcher {:x 41})))
  (t/is (= ::string (predicate-matcher {:x "string"})))
  (t/is (= ::boolean (predicate-matcher {:x true}))))

(p/defpattern predicate-pattern
  [(:x (:compare-fn even?))])

(t/deftest map-matcher-polymorphism-test
  (t/testing "works with a pattern record"
    (t/is (= ::even
             ((p/map-matcher predicate-pattern ::even)
              {:x 42}))))

  (t/testing "works with pattern syntax"
    (t/is (= ::even
             ((p/map-matcher [(:x (:compare-fn even?))] ::even)
              {:x 42})))))

(t/deftest closes-over-outer-variables-test
  (t/testing "with compare-fn"
    (let [evt {:x "x"}]
      (t/is (= "x"
               ((p/map-matcher [(:x (:compare-fn #(= % (:x evt))))] x)
                evt)))))
  (t/testing "as local constant"
    (let [x "x"
          evt {:x x}]
      (t/is (= x
               ((p/map-matcher [(:x x)] x)
                evt)))))
  (t/testing "as global constant"
    (let [evt {:x x}]
      (t/is (= x
               ((p/map-matcher [(:x x)] x)
                evt)))))
  (t/testing "as constant with global defpattern"
    (let [x "x"
          evt {:x x}]
      (t/is (= x
               ((p/map-matcher constant-pattern x)
                evt)))))
  (t/testing "as constant with global parse-pattern"
    (let [evt {:x x}]
      (t/is (= x
               ((p/map-matcher p x)
                evt)))))
  (t/testing "as constant with global defpattern"
    (let [x "x"
          evt {:x x}]
      (p/defpattern p [(:x x)])
      (t/is (= x
               ((p/map-matcher p x)
                evt)))))
  (t/testing "as constant with local parse-pattern"
    (let [x "x"
          evt {:x x}
          p (p/parse-pattern [(:x x)])]
      (t/is (= x
               ((p/map-matcher p x)
                evt))))))
