(ns active.clojure.record-spec-test
  (:require [active.clojure.record-spec :refer (define-record-type)]
            [active.clojure.lens :as lens]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer :all]))


(s/def ::k int?)
(s/def ::v string?)


(define-record-type kv
  (make-kv k v) kv?
  [^{:spec ::k} k kv-k
   (v kv-v kv-v-lens)])

(define-record-type kv-store
  (make-kv-store store) kv-store?
  [(^{:doc "Set of all kvs stored."
      :spec (s/coll-of ::kv :into #{})}
    store kv-store-store kv-store-lens)])

(defrecord FakeKV [k v])

(deftest simple
  (let [kv-1 (make-kv 1 "foo")
        kv-2 (make-kv 2 "bar")
        kv-store (make-kv-store #{kv-1 kv-2})
        kv-fake (FakeKV. 1 "foo")]
    (is (kv? kv-1))
    (is (kv? kv-2))
    (is (kv-store? kv-store))
    (is (= 1 (kv-k kv-1)))
    (is (= "foo" (kv-v kv-1)))
    (is (= #{kv-1 kv-2} (kv-store-store kv-store)))
    (is (not= kv-1 kv-fake))))

(deftest with-instrumentation
  (testing "without instrumentation, spec errors are not detected"
    (let [kv (make-kv "foo" :bar)]
      (is (= "foo" (kv-k kv)))
      (is (= :bar (kv-v kv)))))
  (testing "after instrumentation, this throws an error"
    (stest/instrument)
    (try (make-kv "foo" :bar)
         (catch Exception e
           (is (= "Call to #'active.clojure.record-spec-test/make-kv did not conform to spec:
In: [0] val: \"foo\" fails spec: :active.clojure.record-spec-test/k at: [:args :k] predicate: int?\n"
                  (.getMessage e)))))))
