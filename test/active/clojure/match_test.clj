(ns active.clojure.match-test
  (:require [active.clojure.match :as p]
            [active.clojure.functions :as f]
            [clojure.core.match.regex]
            [clojure.test :as t]))

(t/deftest parse-clause-test
  (t/testing "key exists without binding clause"
    (t/testing "flat"
      (t/is (= (p/make-key-exists-without-binding-clause :k)
               (p/parse-clause :k)))
      (t/is (= (p/make-key-exists-without-binding-clause "k")
               (p/parse-clause k))))
    (t/testing "list"
      (t/is (= (p/make-key-exists-without-binding-clause :k)
               (p/parse-clause (:k))))
      (t/is (= (p/make-path-exists-without-binding-clause [:k])
               (p/parse-clause [:k])))
      (t/is (= (p/make-path-exists-without-binding-clause [:k])
               (p/parse-clause ([:k]))))))

  (t/testing "key exists with binding clause"
    (t/is (= (p/make-key-exists-with-binding-clause :k "Binding")
             (p/parse-clause (:k :as Binding)))))

  (t/testing "path exists without binding clause"
    (t/testing "flat"
      (t/is (= (p/make-path-exists-without-binding-clause [:k "V"])
               (p/parse-clause [:k V]))))
    (t/testing "list"
      (t/is (= (p/make-path-exists-without-binding-clause [:k "V"])
               (p/parse-clause ([:k V]))))))

  (t/testing "path exists with binding clause"
    (t/is (= (p/make-path-exists-with-binding-clause [:k "V"] "Binding")
             (p/parse-clause ([:k V] :as Binding)))))

  (t/testing "key matches without binding clause"
    (t/testing "with regex"
      (let [c (p/parse-clause (:k #"foo"))]
        (t/is (= :k (p/key-matches-without-binding-clause-key c)))
        (t/is (= "foo" (.pattern ^java.util.regex.Pattern (p/regex-matcher-regex (p/key-matches-without-binding-clause-matcher c)))))
        ))
    (t/testing "with any other value"
      (t/is (= (p/make-key-matches-without-binding-clause :k (p/make-constant-matcher "foo"))
               (p/parse-clause (:k "foo"))))))

  (t/testing "key matches with binding clause"
    (t/is (= (p/make-key-matches-with-binding-clause :k (p/make-constant-matcher "foo") "Binding")
             (p/parse-clause (:k "foo" :as Binding)))))

  (t/testing "path matches without binding clause"
    (t/is (= (p/make-path-matches-without-binding-clause [:k "bar" "baz"] (p/make-constant-matcher "foo"))
             (p/parse-clause ([:k bar baz] "foo")))))

  (t/testing "path matches with binding clause"
    (t/is (= (p/make-path-matches-with-binding-clause [:k "bar" "baz"] (p/make-constant-matcher "foo") "Binding")
             (p/parse-clause ([:k bar baz] "foo" :as Binding)))))

  (t/testing "with an options matcher"
    (t/is (= (p/make-key-matches-with-binding-clause :k (p/make-options-matcher [1 2 3]) "Binding")
             (p/parse-clause (:k (:or 1 2 3) :as Binding)))))

  (t/testing "with a predicate matcher"
    (t/is (= (p/make-key-matches-with-binding-clause :k (p/make-predicate-matcher even?) "Binding")
             (p/parse-clause (:k (:compare-fn even?) :as Binding))))
    (t/is (= (p/make-key-matches-with-binding-clause :k (p/make-predicate-matcher (f/partial = 42)) "Binding")
             (p/parse-clause (:k (:compare-fn (f/partial = 42)) :as Binding))))

    ;; What about "regular" anonymous functions?
    (t/is (p/predicate-matcher?
           (p/key-matches-with-binding-clause-matcher
            (p/parse-clause (:k (:compare-fn #(= % 42)) :as Binding))))))

  (t/testing "optional clauses"

    (t/is (= (p/make-optional-key-exists-with-binding-clause :k "Binding")
             (p/parse-clause (? :k :as Binding))))

    (t/is (= (p/make-optional-path-exists-with-binding-clause [:k "bar" "baz"] "Binding")
             (p/parse-clause (? [:k bar baz] :as Binding))))

    (t/is (= (p/make-optional-key-with-default-binding-clause :k "foo" "Binding")
             (p/parse-clause (? :k "foo" :as Binding))))

    (t/is (= (p/make-optional-path-with-default-binding-clause [:k "bar" "baz"] "foo" "Binding")
             (p/parse-clause (? [:k bar baz] "foo" :as Binding))))))

(t/deftest parse-pattern-test
  (let [three-pattern
        (p/pattern (p/key-matches-without-binding-clause :kind (p/match-const "three"))
                   (p/key-matches-with-binding-clause :x (p/match-const "x") "x")
                   (p/key-matches-without-binding-clause :y (p/match-const "y"))
                   (p/key-exists-with-binding-clause :z "z")
                   (p/key-exists-without-binding-clause :w))]
    (t/is (= (p/pattern-clauses three-pattern)
             (p/pattern-clauses (p/parse-pattern [(:kind "three")
                                                  (:x "x" :as x)
                                                  (:y "y")
                                                  (:z :as z)
                                                  :w]))))))

(t/deftest clause->lhs
  ;; lhs doesn't care about bindings

  (t/testing "key exists with binding"
    (t/is (= {:x 'x} (p/clause->lhs {} (p/key-exists-with-binding-clause :x "x"))))
    (t/is (= {:x 'rebind} (p/clause->lhs {} (p/key-exists-with-binding-clause :x "rebind")))))

  (t/testing "key exists without binding"
    (t/is (= {:x '_} (p/clause->lhs {} (p/key-exists-without-binding-clause :x)))))

  (t/testing "path exists with binding"
    (t/is (= {:x {"b" {"c" 'c}}}
             (p/clause->lhs {} (p/path-exists-with-binding-clause [:x "b" "c"] "c"))))
    (t/is (= {:x {"b" {"c" 'rebind}}}
             (p/clause->lhs {} (p/path-exists-with-binding-clause [:x "b" "c"] "rebind")))))

  (t/testing "path exists without binding"
    (t/is (= {:x {"b" {"c" '_}}}
             (p/clause->lhs {} (p/path-exists-without-binding-clause [:x "b" "c"])))))

  (t/testing "key matches with binding"
    (t/is (= {:x "b"} (p/clause->lhs {} (p/key-matches-with-binding-clause :x (p/match-const "b") "x"))))
    (t/is (= {:x "b"} (p/clause->lhs {} (p/key-matches-with-binding-clause :x (p/match-const "b") "rebind"))))
    (t/is (= `({:x ~(quote _)} :guard [(constantly (~even? (get-in {} [:x])))])
             (p/clause->lhs {} (p/key-matches-with-binding-clause :x (p/match-predicate even?) "x")))))

  (t/testing "key matches without binding"
    (t/is (= {:x "b"} (p/clause->lhs {} (p/key-matches-without-binding-clause :x (p/match-const "b"))))))

  (t/testing "path matches with binding"
    (t/is (= {:x {:y {:z "b"}}}
             (p/clause->lhs {} (p/path-matches-with-binding-clause [:x :y :z] (p/match-const "b") "z"))))
    (t/is (= {:x {:y {:z "b"}}}
             (p/clause->lhs {} (p/path-matches-with-binding-clause [:x :y :z] (p/match-const "b") "rebind"))))
    (t/is (= `({:x {:y {"Z" ~(quote _)}}} :guard [(constantly (~even? (get-in {} [:x :y "Z"])))])
             (p/clause->lhs {} (p/path-matches-with-binding-clause [:x :y "Z"] (p/match-predicate even?) "Z")))))

  (t/testing "path matches without binding"
    (t/is (= {:x {:y {:z "b"}}}
             (p/clause->lhs {} (p/path-matches-without-binding-clause [:x :y :z] (p/match-const "b")))))))

(t/deftest path-matches-with-binding-clause->rhs-match-test
  (t/is (= `[~(symbol "z") (get-in {:x {:y {:z "b"}}} [:x :y :z] "b")]
           (p/path-matches-with-binding-clause->rhs-match {:x {:y {:z "b"}}}
                                             (p/path-matches-with-binding-clause [:x :y :z] (p/match-const "b") "z"))))
  (t/is (= `[~(symbol "rebind") (get-in {:x {:y {:z "b"}}} [:x :y :z] "b")]
           (p/path-matches-with-binding-clause->rhs-match {:x {:y {:z "b"}}}
                                             (p/path-matches-with-binding-clause [:x :y :z] (p/match-const "b") "rebind")))))

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


;; Testing Macro

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
   ([:d] :as d)
   ([:d Z] 42 :as Z)
   ([:d Y] :as Y)
   ([:d X] 65)
   [:d W foo]])

(def example-matcher
  (p/map-matcher
   one   [x z]
   two   [a c d Z Y]
   :else false))

(t/deftest map-matcher-test
  ;; FIXME
  ;; compiler exception: not a valid pattern
  ;; (t/is (= [] ((p/map-matcher [] []) {:a "a"})))
  ;; compiler exception: Wrong number of args (0) passed to: active.clojure.match/deep-merge
  ;; (t/is (= [] ((p/map-matcher [()] []) {:a "a"})))
  ;; compiler exception: class clojure.lang.PersistentVector cannot be cast to class clojure.lang.Named
  ;; (t/is (= [] ((p/map-matcher [([])] []) {:a "a"})))

  (t/testing "Very simple example without bindings."
    ;; we thought that empty lets in the macro-expand would create problems
    (t/is (= [] ((p/map-matcher [(:a)] []) {:a "a"}))))
  (t/testing "Very simple examples with binding."
    (t/is (= [] ((p/map-matcher [(:a :as a)] []) {:a "a"})))
    (t/is (= ["a"] ((p/map-matcher [(:a :as a)] [a]) {:a "a"}))))
  (t/testing "Example-matcher"
    (t/is (= ["x" "z"]
             (example-matcher one-data)))
    (t/is (= ["a" "c" {"Z" 42 "Y" 23 "X" 65 "W" {"foo" "bar"}} 42 23]
             (example-matcher two-data)))
    (t/is (= false (example-matcher {:kind "none"})))
    (t/testing "map-matcher-regex-key-not-found"
      (t/is (= false (example-matcher three-data)))))
  (t/testing "Paths"
    (t/is (= [{}] ((p/map-matcher [([:a] :as a)] [a]) {:a {}})))
    (t/is (= [] ((p/map-matcher [([:a b] "c")] []) {:a { "b" "c"}})))
    (t/is (= [] ((p/map-matcher [([:a b c] "d")] []) {:a { "b" {"c" "d"}}})))
    (t/is (= ["d"] ((p/map-matcher [([:a b c] "d" :as z)] [z]) {:a { "b" {"c" "d"}}})))))

(t/deftest map-matcher-optional-test
  (t/testing "Optional values"
    (t/is (= ["a" "c" "C" 42]
             ((p/map-matcher [(:kind #"two")
                              (? :a :as a)
                              (? :c "C" :as c)
                              (? :C "C" :as C)
                              (? [:d Z] :as Z)]
                             [a c C Z])
              two-data))))
  (t/testing "Fall back to given default value."
    (t/is (= ["a" "C" "C" 42]
             ((p/map-matcher [(:kind #"two")
                              (? :a :as a)
                              (? :c "C" :as c)
                              (? :C "C" :as C)
                              (? [:d Z] :as Z)]
                             [a c C Z])
              {:kind "two" :a "a" :b "b"
               :d {"Z" 42 "X" 65
                   "W" {"foo" "bar"}}}))))
  (t/testing "More optionals"
    ;; key match with binding
    (t/is (= [nil] ((p/map-matcher [(? :a :as a)] [a]) {})))
    (t/is (= ["a"] ((p/map-matcher [(? :a :as a)] [a]) {:a "a"})))
    ;; key match with default value with binding
    (t/is (= ["A"] ((p/map-matcher [(? :a "A" :as a)] [a]) {})))
    (t/is (= ["a"] ((p/map-matcher [(? :a "A" :as a)] [a]) {:a "a"})))
    ;; path match with binding
    (t/is (= [nil] ((p/map-matcher [(? [:a _] :as a)] [a]) {})))
    (t/is (= [nil] ((p/map-matcher [(? [:a _] :as a)] [a]) {:a "a"})))
    (t/is (= [nil] ((p/map-matcher [(? [:a _] :as a)] [a]) {:a {"b" {"c" "d"}}})))
    (t/is (= [nil] ((p/map-matcher [(? [:a a] :as a)] [a]) {})))
    (t/is (= [nil] ((p/map-matcher [(? [:a a] :as a)] [a]) {:a "a"})))
    (t/is (= [nil] ((p/map-matcher [(? [:a a] :as a)] [a]) {:a {"b" {"c" "d"}}})))
    (t/is (= [{"c" "d"}] ((p/map-matcher [(? [:a b] :as a)] [a]) {:a {"b" {"c" "d"}}})))
    (t/is (= [2] ((p/map-matcher [(? [:a _] :as a)] [a]) {:a {"_" 2}})))
    (t/is (= [nil] ((p/map-matcher [(? [:a c] :as a)] [a]) {:a {"b" {"c" "d"}}})))
    (t/is (= ["d"] ((p/map-matcher [(? [:a b c] :as a)] [a]) {:a {"b" {"c" "d"}}})))
    ;; path match with binding with default value
    (t/is (= ["D"] ((p/map-matcher [(? [:a b c] "D" :as a)] [a]) {:a "b"})))
    (t/is (= ["D"] ((p/map-matcher [(? [:a b c] "D" :as a)] [a]) {:a {"b" {}}})))
    (t/is (= ["D"] ((p/map-matcher [(? [:a b c] "D" :as a)] [a]) {:a {"b" {"e" "d"}}})))
    (t/is (= ["d"] ((p/map-matcher [(? [:a b c] "D" :as a)] [a]) {:a {"b" {"c" "d"}}})))
    ))

(p/defpattern one-or
  [(:kind #"one")
   (:x (:or "a" "b" "c" "x") :as x)
   (:y (:or "x" "y" "z"))
   (:z :as z)
   :w])

(def example-or-matcher
  (p/map-matcher
   one-or [x z]
   two [a c Z Y]
   :else false))

(t/deftest map-matcher-or-test
  (t/testing "Alternative values to match on"
    (t/is (= ["x" "z"]
             (example-or-matcher one-data)))
    (t/is (= ["a" "c" 42 23]
             (example-or-matcher two-data)))
    (t/is (= false (example-or-matcher {:kind "none"})))))

;; FIXME: one-guard and example-guard-matcher not used
#_(p/defpattern one-guard
  [(:kind #"one")
   (:x (:compare-fn #(= % (last ["a" "b" "c" "x"]))) :as x)
   (:y (:compare-fn #(= % (:y {:x "x" :y "y" :z "z"}))))
   (:z :as z)
   :w])

#_(def example-guard-matcher
  (p/map-matcher
   one-guard [x z]
   two [a c Z Y]
   :else false))

(p/defpattern predicate-pattern
  [(:x (:compare-fn even?))])

(def predicate-matcher
  (p/map-matcher
   ;; The order is important
   [(:x (:compare-fn #(string? %)))] ::string
   [(:x (:compare-fn (fn [x] (boolean? x))))] ::boolean
   [(:x (:compare-fn even?))] ::even
   [(:x (:compare-fn odd?))] ::odd))

(t/deftest map-matcher-predicate-test
  (t/testing "Predicates"
    (t/is (= ::even (predicate-matcher {:x 42})))
    (t/is (= ::odd (predicate-matcher {:x 41})))
    (t/is (= ::string (predicate-matcher {:x "string"})))
    (t/is (= ::boolean (predicate-matcher {:x true}))))
  (t/testing "Polymorphism: works with a pattern record"
    (t/is (= ::even
             ((p/map-matcher predicate-pattern ::even)
              {:x 42}))))
  (t/testing "Polymorphism: works with pattern syntax"
    (t/is (= ::even
             ((p/map-matcher [(:x (:compare-fn even?))] ::even)
              {:x 42})))))

(def x "x")
(p/defpattern constant-pattern [(X x)])
(def p (p/parse-pattern '[(X x)]))

(t/deftest map-matcher-scope-test

  (t/testing "let-variable is not overwritten if there is no binding"
    (let [k "my-value"]
      ((p/map-matcher [(:k 23)] (t/is (= "my-value" k))) {:k 23})))
  (t/testing "let-variable is overwritten if there is a binding"
    (let [k "my-value"]
      ((p/map-matcher [(:k 23 :as k)] (t/is (= 23 k))) {:k 23})))
  (t/testing "let-variable used within compare-fn"
    (let [evt {"X" "x"}]
      (t/is (= "x"
               ((p/map-matcher [(X (:compare-fn #(= % (get evt "X"))))] x)
                evt)))))
  (t/testing "local: let-variable used as constant"
    (let [x   "x"
          evt {"X" x}]
      (t/is (= x
               ((p/map-matcher [(X x)] x)
                evt)))))
  (t/testing "global: def-variable used as constant"
    (let [evt {"X" x}]
      (t/is (= x
               ((p/map-matcher [(X x)] x)
                evt)))))
  (t/testing "global: defpattern used as constant"
    (let [x   "x"
          evt {"X" x}]
      (t/is (= x
               ((p/map-matcher constant-pattern x)
                evt)))))
  (t/testing "global: def-parse-pattern as constant"
    (let [evt {"X" x}]
      (t/is (= x
               ((p/map-matcher p x)
                evt)))))
  (t/testing "local: defpattern as constant"
    (let [x   "x"
          evt {"X" x}]
      (p/defpattern p [("X" x)])
      (t/is (= x
               ((p/map-matcher p x)
                evt)))))
  #_(t/testing "as constant with local parse-pattern"
    (let [x   "x"
          evt {"X" x}
          p   (p/parse-pattern [(X x)])]
      (t/is (= x
               ;; Caused by java.lang.UnsupportedOperationException Can't eval locals
               ((p/map-matcher p x)
                evt))))))
