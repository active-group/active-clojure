(ns active.clojure.condition-test
  #?(:clj (:require [clojure.test :refer [deftest is]]
                    [active.clojure.condition :as c])
     :cljs (:require [cljs.test :refer-macros [is deftest]]
                     [active.clojure.condition :as c :include-macros true])))

#?(:cljs
(enable-console-print!))

(c/define-condition-type &c c/&condition
  make-c c?
  [x c-x])

(c/define-condition-type &c1 &c
  make-c1 c1?
  [a c1-a])

(c/define-condition-type &c2 &c
  make-c2 c2?
  [b c2-b])

(def v1 (make-c1 "V1" "a1"))
  
(deftest c1
  (is (c? v1))
  (is (c1? v1))
  (is (not (c2? v1)))
  (is (= "V1" (c-x v1)))
  (is (= "a1" (c1-a v1))))

(def v2 (make-c2 "V2" "b2"))

(deftest c2
  (is (c? v2))
  (is (not (c1? v2)))
  (is (c2? v2))
  (is (= "V2"
         (c-x v2)))
  (is (= "b2"
         (c2-b v2))))

(def v3 (c/combine-conditions
         (make-c1 "V3/1" "a3")
         (make-c2 "V3/2" "b3")))

(deftest compound1
  (is (c? v3))
  (is (c1? v3))
  (is (c2? v3))
  (is (= "V3/1"
         (c-x v3)))
  (is (= "a3"
         (c1-a v3)))
  (is (= "b3"
         (c2-b v3))))

(def v4 (c/combine-conditions v1 v2))

(deftest compound2
  (is (c? v4))
  (is (c1? v4))
  (is (c2? v4))
  (is (= "V1"
         (c-x v4)))
  (is (= "a1"
         (c1-a v4)))
  (is (= "b2"
         (c2-b v4))))

(def v5 (c/combine-conditions v2 v3))

(deftest compound3
  (is (c? v5))
  (is (c1? v5))
  (is (c2? v5))
  (is (= "V2"
         (c-x v5)))
  (is (= "a3"
         (c1-a v5)))
  (is (= "b2"
         (c2-b v5))))

(deftest condition-types
  (mapv
   (fn [[e pred?]]
     (is (pred? e))
     (try (throw e)
          (catch Throwable caught
            (is (= e caught)))))
   [[(c/make-message-condition "the message") c/message-condition?]
    [(c/make-warning) c/warning?]
    [(c/make-serious-condition) c/serious-condition?]
    [(c/make-error) c/error?]
    [(c/make-violation) c/violation?]
    [(c/make-assertion-violation) c/assertion-violation?]
    [(c/make-irritants-condition ["eins" "zwei"]) c/irritants-condition?]
    [(c/make-who-condition "them") c/who-condition?]]))

(deftest guard-test
  (is (= :error
         (c/guard [con
                 (c/error? con) :error
                 (c/violation? con) :violation]
                (throw (c/make-error)))))
  (is (thrown? Throwable
               (c/guard [con
                       (c/error? con) :error]
                      (throw (c/make-violation)))))
  (is (= :something-else
         (c/guard [con
                 (c/violation? con) :violation
                 :else :something-else]
                 (throw (c/make-error))))))

#?(:clj
(deftest java-throwables
  (let [c (c/->condition (Throwable. "foo"))]
    (is (c/throwable? c))
    (is (not (c/error? c)))
    (is (not (c/assertion-violation? c))))

  (let [c (c/->condition (Error. "foo"))]
    (is (c/throwable? c))
    (is (not (c/error? c)))
    (is (c/assertion-violation? c)))

  (let [c (c/->condition (Exception. "foo"))]
    (is (c/throwable? c))
    (is (c/error? c))
    (is (not (c/assertion-violation? c))))))

(deftest combine-nil
  (is (c/error? (c/combine-conditions false (c/make-error) nil))))

(deftest exception-in-macro
  (try (or false (c/assertion-violation `exception-in-macro "I should throw."))
       #?(:clj
          (catch Exception ^Exception e
            (is (= "I should throw." (.getMessage e))))
          :cljs
          (catch js/Error e
            (is (= "I should throw." (.-message e)))))))
