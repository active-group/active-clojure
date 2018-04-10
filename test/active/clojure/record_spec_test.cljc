(ns active.clojure.record-spec-test
  #?@
   (:clj
    [(:require
      [active.clojure.record-spec :refer [define-record-type]]
      [clojure.spec.alpha :as s]
      [clojure.spec.test.alpha :as stest]
      [clojure.test :as t])]
    :cljs
    [(:require
      [active.clojure.record-spec :refer-macros [define-record-type]]
      [cljs.spec.alpha :as s :include-macros true]
      [cljs.spec.test.alpha :as stest :include-macros true]
      [cljs.test :as t :include-macros true])]))

(s/def ::k int?)
(s/def ::v string?)


(define-record-type kv
  (make-kv k v) kv?
  [^{:spec ::k} k kv-k
   (^{:spec ::v} v kv-v kv-v-lens)])

(define-record-type kv-store
  (make-kv-store store) kv-store?
  [(^{:doc "Set of all kvs stored."
      :spec (s/coll-of ::kv :into #{})}
    store kv-store-store kv-store-lens)])

(defrecord FakeKV [k v])

(t/deftest simple
  (let [kv-1 (make-kv 1 "foo")
        kv-2 (make-kv 2 "bar")
        kv-store (make-kv-store #{kv-1 kv-2})
        kv-fake (FakeKV. 1 "foo")]
    (t/is (kv? kv-1))
    (t/is (kv? kv-2))
    (t/is (kv-store? kv-store))
    (t/is (= 1 (kv-k kv-1)))
    (t/is (= "foo" (kv-v kv-1)))
    (t/is (= #{kv-1 kv-2} (kv-store-store kv-store)))
    (t/is (not= kv-1 kv-fake))))

(t/deftest with-instrumentation
  (t/testing "without instrumentation, spec errors are not detected"
    (let [kv (make-kv "foo" :bar)]
      (t/is (= "foo" (kv-k kv)))
      (t/is (= :bar (kv-v kv)))))
  (t/testing "after instrumentation, this throws an error"
    (stest/instrument)
    (try (make-kv "foo" :bar)
         (catch Exception e
           (t/is (= "Call to #'active.clojure.record-spec-test/make-kv did not conform to spec:
In: [0] val: \"foo\" fails spec: :active.clojure.record-spec-test/k at: [:args :k] predicate: int?\n"
                  (.getMessage e)))))))
