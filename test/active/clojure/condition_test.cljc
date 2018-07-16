(ns active.clojure.condition-test
  (:require [active.clojure.condition :refer (&condition combine-conditions #?(:clj define-condition-type) #?(:clj guard) ->condition
                                              make-error
                                              assertion-violation
                                              throwable? error? assertion-violation?
                                              #?(:cljs Throwable))]
            [active.clojure.condition :as c]
            #?(:clj [clojure.test :refer :all])
            #?(:cljs [cljs.test]))
  #?(:cljs 
  (:require-macros [cljs.test
                    :refer (is deftest run-tests testing)]
                   [active.clojure.condition :refer (define-condition-type assertion-violation guard)])))

#?(:cljs
(enable-console-print!))

(define-condition-type &c &condition
  make-c c?
  [x c-x])

(define-condition-type &c1 &c
  make-c1 c1?
  [a c1-a])

(define-condition-type &c2 &c
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

(def v3 (combine-conditions
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

(def v4 (combine-conditions v1 v2))

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

(def v5 (combine-conditions v2 v3))

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
  (map
   #(let [e ((first %1))]
      (is ((second %1) e))

      (try (throw e)
           (catch Throwable caught
             (is (= e caught)))))
   [[c/make-message-condition c/message-condition?]
    [c/make-warning c/warning?]
    [c/make-serious-condition c/serious-condition?]
    [c/make-error c/error?]
    [c/make-violation c/violation?]
    [c/make-assertion-violation c/assertion-violation?]
    [c/make-irritants-condition c/irritants-condition?]
    [c/make-who-condition c/who-condition?]]))

(deftest guard-test
  (is (= :error
         (guard [con
                 (c/error? con) :error
                 (c/violation? con) :violation]
                (throw (c/make-error)))))
  (is (thrown? Throwable
               (guard [con
                       (c/error? con) :error]
                      (throw (c/make-violation)))))
  (is (= :something-else
         (guard [con
                 (c/violation? con) :violation
                 :else :something-else]
                 (throw (c/make-error))))))

#?(:clj
(deftest java-throwables
  (let [c (->condition (Throwable. "foo"))]
    (is (throwable? c))
    (is (not (error? c)))
    (is (not (assertion-violation? c))))

  (let [c (->condition (Error. "foo"))]
    (is (throwable? c))
    (is (not (error? c)))
    (is (assertion-violation? c)))

  (let [c (->condition (Exception. "foo"))]
    (is (throwable? c))
    (is (error? c))
    (is (not (assertion-violation? c))))))

(deftest combine-nil
  (is (error? (combine-conditions false (make-error) nil))))

(deftest exception-in-macro
  (try (or false (assertion-violation `exception-in-macro "I should throw."))
       #?(:clj
          (catch Exception ^Exception e
            (is (= "I should throw." (.getMessage e))))
          :cljs
          (catch js/Error e
            (is (= "I should throw." (.-message e)))))))
