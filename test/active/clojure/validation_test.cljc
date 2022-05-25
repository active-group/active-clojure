(ns active.clojure.validation-test
  (:require #?(:clj [active.clojure.record :refer [define-record-type]]
               :cljs [active.clojure.cljs.record :refer-macros [define-record-type]])
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [active.clojure.validation :as v]))

(t/deftest fmap-success-test
  (let [failure (v/make-validation-failure
                 [(v/make-validation-error "candidate" "message" nil)])
        success (v/make-validation-success "candidate")]
    (t/is (= failure (v/fmap-success identity failure)))
    (t/is (= success (v/fmap-success identity success)))))

(t/deftest seq-validation-test
  (let [failure (v/make-validation-failure
                 [(v/make-validation-error "candidate" "message" nil)])
        failure-2 (v/make-validation-failure
                   [(v/make-validation-error "candidate-2" "message-2" nil)])
        flat-success (v/make-validation-success "candidate")]
    (t/testing "failures are concatenated"
      (t/is (= (v/make-validation-failure
                [(v/make-validation-error "candidate" "message" nil)
                 (v/make-validation-error "candidate-2" "message-2" nil)])
               (v/seq-validation failure failure-2))))
    (t/testing "one failure leads to failure"
      (t/is (= failure
               (v/seq-validation (v/pure-validation identity) failure))))
    (t/testing "two successes lead to success"
      (t/is (= flat-success
               (v/seq-validation (v/pure-validation identity) flat-success))))))

(t/deftest curry-n-test
  (t/is (= 42 ((v/curry-n (fn [] 42) 0))))
  (t/is (= 42 (((v/curry-n (fn [a b] (+ a b)) 2) 1) 41)))
  (t/is (= 42 ((((v/curry-n (fn [a b c] (+ a b c)) 3) 1) 40) 1))))

(define-record-type Person {:rtd-record? true}
  make-person person?
  [name person-name
   age person-age])

(defn- validate-person
  [[name age]]
  (v/validation make-person
                (v/validate-string name)
                (v/validate-pos-int age)))

(defn- validate-person-with-labels
  [[name age]]
  (v/validation make-person
                (v/validate-string name :name)
                (v/validate-pos-int age :age)))

(t/deftest validation-test
  (t/is (= (v/make-validation-success (make-person "Mimi" 1))
           (validate-person ["Mimi" 1])))
  (t/is (= (v/make-validation-success (make-person "Mimi" 1))
           (validate-person-with-labels ["Mimi" 1])))
  (t/testing "every failure is collected in the result"
    (t/is (= (v/make-validation-failure
              [(v/make-validation-error 1 ::v/string nil)
               (v/make-validation-error "Mimi" ::v/pos-int nil)])
             (validate-person [1 "Mimi"])))
    (t/is (= (v/make-validation-failure
              [(v/make-validation-error 1 ::v/string :name)
               (v/make-validation-error "Mimi" ::v/pos-int :age)])
             (validate-person-with-labels [1 "Mimi"])))))

(t/deftest validate-string-test
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error 42 ::v/string nil)])
           (v/validate-string 42)))
  (t/is (= (v/make-validation-success "string")
           (v/validate-string "string"))))

(t/deftest validate-non-empty-string-test
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error 42 ::v/non-empty-string nil)])
           (v/validate-non-empty-string 42)))
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error "" ::v/non-empty-string nil)])
           (v/validate-non-empty-string "")))
  (t/is (= (v/make-validation-success "string")
           (v/validate-non-empty-string "string"))))

(t/deftest validate-int-test
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error "string" ::v/int nil)])
           (v/validate-int "string")))
  (t/is (= (v/make-validation-success 42)
           (v/validate-int 42))))

(t/deftest pos-int-validation-test
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error "string" ::v/pos-int nil)])
           (v/validate-pos-int "string")))
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error -23 ::v/pos-int nil)])
           (v/validate-pos-int -23)))
  (t/is (= (v/make-validation-success 42)
           (v/validate-pos-int 42))))

(t/deftest validate-boolean-test
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error "string" ::v/boolean nil)])
           (v/validate-boolean "string")))
  (t/is (= (v/make-validation-success true)
           (v/validate-boolean true)))
  (t/is (= (v/make-validation-success false)
           (v/validate-boolean false))))

(t/deftest validate-keyword-test
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error 42 ::v/keyword nil)])
           (v/validate-keyword 42)))
  (t/is (= (v/make-validation-success :keyword)
           (v/validate-keyword :keyword))))

(t/deftest validate-one-of-test
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error 42 [::v/one-of #{:a :b :c}] nil)])
           (v/validate-one-of [:a :b :c] 42)))
  (t/is (= (v/make-validation-success :a)
           (v/validate-one-of [:a :b :c] :a)))
  (t/is (= (v/make-validation-success :c)
           (v/validate-one-of [:a :b :c] :c))))

(t/deftest validate-list-test
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error 42 ::v/list nil)])
           (v/validate-list 42)))
  (t/testing "vectors are not lists"
    (t/is (= (v/make-validation-failure
              [(v/make-validation-error [1 2 3] ::v/list nil)])
             (v/validate-list [1 2 3]))))
  (t/is (= (v/make-validation-success (list 1 2 3))
           (v/validate-list (list 1 2 3)))))

(t/deftest validate-vector-test
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error 42 ::v/vector nil)])
           (v/validate-vector 42)))
  (t/testing "lists are not vectors"
    (t/is (= (v/make-validation-failure
              [(v/make-validation-error (list 1 2 3) ::v/vector nil)])
             (v/validate-vector (list 1 2 3)))))
  (t/is (= (v/make-validation-success [1 2 3])
           (v/validate-vector [1 2 3]))))

(t/deftest validate-map-test
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error 42 ::v/map nil)])
           (v/validate-map 42)))
  (t/is (= (v/make-validation-success {:a "b"})
           (v/validate-map {:a "b"}))))

(t/deftest validate-set-test
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error 42 ::v/set nil)])
           (v/validate-set 42)))
  (t/is (= (v/make-validation-success #{:a :b :c})
           (v/validate-set #{:a :b :c}))))

(t/deftest validate-sequential-test
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error 42 ::v/sequential nil)])
           (v/validate-sequential 42)))
  (t/is (= (v/make-validation-success (list 1 2 3))
           (v/validate-sequential (list 1 2 3))))
  (t/is (= (v/make-validation-success [1 2 3])
           (v/validate-sequential [1 2 3]))))

(t/deftest optional-test
  (let [validate-optional-string (v/optional v/validate-string)]
    (t/is (= (v/make-validation-success "string")
             (validate-optional-string "string")))
    (t/is (= (v/make-validation-success nil)
             (validate-optional-string nil)))
    (t/is (= (v/make-validation-failure
              [(v/make-validation-error 42 [::v/optional ::v/string] nil)])
             (validate-optional-string 42)))))

(define-record-type Node {:rtd-record? true}
  make-node node?
  [label node-label
   neighbors node-neighbors])

(defn- validate-node
  [[label neighbors]]
  (v/validation make-node
                (v/validate-non-empty-string label :label)
                (v/sequence-of validate-node neighbors :neighbors)))

(t/deftest sequence-of-test
  (t/is (= (v/make-validation-success ["a" "b" "c"])
           (v/sequence-of v/validate-non-empty-string ["a" "b" "c"])))
  (t/is (= (v/make-validation-failure
            [(v/make-validation-error nil ::v/non-empty-string [::v/seq 0])
             (v/make-validation-error 32 ::v/non-empty-string [::v/seq 2])])
           (v/sequence-of v/validate-non-empty-string [nil "b" 32])))
  (t/testing "labels are used correctly"
    (t/is (= (v/make-validation-failure
              [(v/make-validation-error nil ::v/non-empty-string [::some-name 0])
               (v/make-validation-error 32 ::v/non-empty-string [::some-name 2])])
             (v/sequence-of v/validate-non-empty-string [nil "b" 32] ::some-name))))
  (t/testing "sequential validations can be nested"
    (t/testing "one level deep"
      (t/is (= (v/make-validation-success
                (make-node "a" []))
               (validate-node ["a" []])))
      (t/is (= (v/make-validation-success
                (make-node "a" [(make-node "b" []) (make-node "c" [])]))
               (validate-node ["a" [["b" []] ["c" []]]])))
      (t/is (= (v/make-validation-failure
                [(v/make-validation-error "" ::v/non-empty-string [[:neighbors 1] :label])])
               (validate-node ["a" [["b" []] ["" []]]]))))
    (t/testing "multiple levels deep"
      (t/is (= (v/make-validation-success
                (make-node "a" [(make-node "b" [(make-node "c" [])])
                                (make-node "d" [(make-node "e" [(make-node "f" [])])])]))
               (validate-node ["a" [["b" [["c" []]]]
                                    ["d" [["e" [["f" []]]]]]]])))
      (t/is (= (v/make-validation-failure
                [(v/make-validation-error :e ::v/non-empty-string [[:neighbors 1] [[:neighbors 0] :label]])
                 (v/make-validation-error :f ::v/non-empty-string [[:neighbors 1] [[:neighbors 0] [[:neighbors 0] :label]]])])
               (validate-node ["a" [["b" [["c" []]]]
                                    ["d" [[:e [[:f []]]]]]]]))))))

(t/deftest validate-choice-test
  (let [validate-string-or-int (fn [candidate & [label]]
                                 (v/validate-choice [v/validate-string
                                                     v/validate-int]
                                                    candidate
                                                    label))]
    (t/is (= (v/make-validation-success "string")
             (validate-string-or-int "string")))
    (t/is (= (v/make-validation-success 42)
             (validate-string-or-int 42)))

    (t/is (= (v/make-validation-failure
              [(v/make-validation-error :key ::v/string nil)
               (v/make-validation-error :key ::v/int nil)])
             (validate-string-or-int :key)))))

(t/deftest validate-all-test
  (let [v (fn [c]
            (v/validate-all [v/validate-non-empty-string
                             (fn [candidate & [label]]
                               (if (= candidate "clojure")
                                 (v/make-validation-success candidate)
                                 (v/make-validation-failure [(v/make-validation-error candidate ::not-clojure label)])))]
                          c
                          :non-empty-and-clojure))]
    (t/is (= (v/make-validation-failure
              [(v/make-validation-error "" ::v/non-empty-string :non-empty-and-clojure)
               (v/make-validation-error "" ::not-clojure :non-empty-and-clojure)])
             (v "")))
    (t/is (= (v/make-validation-failure
              [(v/make-validation-error "clj" ::not-clojure :non-empty-and-clojure)])
             (v "clj")))
    (t/is (= (v/make-validation-success "clojure") (v "clojure")))))
