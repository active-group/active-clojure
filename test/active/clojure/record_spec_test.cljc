(ns active.clojure.record-spec-test
  #?@
   (:clj
    [(:require
      [active.clojure.lens :as lens]
      [active.clojure.record-spec :refer [define-record-type]]
      [clojure.spec.alpha :as s]
      [clojure.spec.test.alpha :as stest]
      [clojure.test :as t])]
    :cljs
    [(:require
      [active.clojure.lens :as lens]
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

(t/deftest simple-kv-tests
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
  #?(:clj
     (t/testing "after instrumentation, this throws an error"
       (stest/instrument)
       (try (make-kv "foo" :bar)
            (catch Exception e
              (t/is (= "Call to #'active.clojure.record-spec-test/make-kv did not conform to spec:
In: [0] val: \"foo\" fails spec: :active.clojure.record-spec-test/k at: [:args :k] predicate: int?\n"
                       (.getMessage e))))))))

(define-record-type Dith
  (make-dith tso)
  dith?
  [^{:spec string?} tso dith-tso])

(define-record-type Xom
  (make-xom baz dith)
  xom?
  [^{:spec integer?} baz xom-baz
   ^{:spec ::Dith} dith xom-dith])

(t/deftest record-spec-tests
  (t/testing "Selector spec validity"
    (t/is (s/valid? ::Xom-dith (make-dith "dith")))
    (t/is (not (s/valid? ::Dith-tso 31947))))
  (t/testing "Record spec validity"
    (t/is (s/valid? ::Dith (make-dith 23)))
    (t/is (s/valid?
           ::Xom
           (make-xom 1000 (make-dith "five"))))
    #_(t/is (not (s/valid? ::Dith (make-dith "abc")))) ; not yet working as intended
    #_(t/is (not (s/valid?
                ::Xom
                (make-xom 23 (make-dith 128)))))))

;; taken from record-test

(define-record-type Pare
  (kons a b)
  pare?
  [a kar
   b kdr])

(defrecord FakePare [a b])

(t/deftest simple
  (let [r (kons 1 2)]
    (t/is (pare? r))
    (t/is (= 1 (kar r)))
    (t/is (= 2 (kdr r)))))

#?(:clj
   (t/deftest unique
     (t/is (thrown? Throwable
                    (kar (FakePare. 1 2))))))

(define-record-type Pu
  (make-pu c a)
  pu?
  [a pua
   b pub
   c puc])

(t/deftest constructor
  (let [p (make-pu 1 2)]
    (t/is (pu? p))
    (t/is (not (pare? p)))
    (t/is (= 2 (pua p)))
    (t/is (= 1 (puc p)))
    (t/is (nil? (pub p)))))

;; Records with lenses

(defn law-1-holds [l data v]
  ;; you get back what you put in
  (t/is (= v
         (lens/yank (lens/shove data l v) l))))

(defn law-2-holds [l data]
  ;; putting back what you got doesn't change anything
  (t/is (= data
         (lens/shove data l (lens/yank data l)))))

(defn law-3-holds [l data v1 v2]
  ;; second set wins, or setting once is the same as setting twice
  (t/is (= (lens/shove data l v1)
         (lens/shove (lens/shove data l v2) l v1))))

(defn lens-laws-hold [l data v1 v2]
  (and (law-1-holds l data v1)
       (law-2-holds l data)
       (law-3-holds l data v1 v2)))

(define-record-type ^{:doc "Lens example"} LensPare
  (lens-kons a b)
  lens-pare?
  [(^{:doc "a field"} a lens-kar lens-kar-lens)
   (^{:doc "b field"} b lens-kdr lens-kdr-lens)])

(t/deftest pare-lens
  (lens-laws-hold lens-kar-lens (lens-kons 1 2) 23 42)
  (lens-laws-hold lens-kdr-lens (lens-kons 1 2) 23 42)
  (t/is (= (lens-kons "a" 42)
         (lens/shove (lens-kons 23 42) lens-kar-lens "a")))
  (t/is (= (lens-kons 23 "b")
         (lens/shove (lens-kons 23 42) lens-kdr-lens "b"))))

(define-record-type Quadruple
  (quadruple a b c d)
  quadruple?
  [(a quadruple-one quadruple-one-lens)
   b quadruple-two
   (c quadruple-three quadruple-three-lens)
   d quadruple-four])

(t/deftest quadruple-lens
  (lens-laws-hold quadruple-one-lens (quadruple 'a 'b 'c 'd) 12 78)
  (lens-laws-hold quadruple-three-lens (quadruple 'a 'b 'c 'd) 12 78)
  (t/is (= (quadruple 4 8 15 16)
         (lens/shove (quadruple 108 8 15 16) quadruple-one-lens 4))))
