(ns active.clojure.struct-test
  (:require [active.clojure.struct :as sut #?@(:cljs [:include-macros true])]
            [active.clojure.struct.validator :as validator]
            #?(:clj [clojure.test :as t]
                  :cljs [cljs.test :as t :include-macros true])))

(sut/def-struct T [t-a t-b])

(defn throws [t]
  #?(:clj (try (t) false (catch Exception e e))
     :cljs (try (t) false (catch :default e e))))

(t/deftest construction-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/is (some? v) "Can construct with all fields")

    (t/is (= v (sut/struct-map T
                               t-b :foo
                               t-a 42)) "order does not matter"))

  (t/is (some? (throws #(sut/struct-map T t-a 42)))
        "Cannot construct with partial fields")
  
  (t/is (some? (throws #(sut/struct-map T :foo 42)))
        "Cannot construct with foreign fields")

  ;; Note: not sure if we want to allow this; but it could be an easy
  ;; way to write 'smart constructors' that define default values.
  (t/is (= (sut/struct-map T
                           t-b :foo
                           t-a 42)
           (sut/struct-map T
                           t-b :foo
                           t-a nil
                           t-a 42))
        "Later values 'override' previous values in struct-map")
  )

(t/deftest printer-test
  ;; (also indirectly tests key printer)
  (let [s (pr-str (sut/struct-map T
                                  t-a 42
                                  t-b :foo))]
    (t/is (#{"{~t-b :foo, ~t-a 42}"
             "{~t-a 42, ~t-b :foo}"} s)))
  (let [s (str (sut/struct-map T
                               t-a "42"
                               t-b :foo))]
    (t/is #{"{~t-b :foo, ~t-a 42}"
            "{~t-a 42, ~t-b :foo}"} s))
  )

(t/deftest access-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/testing "via keys"
      (t/is (= 42 (t-a v))))
    
    (t/testing "via get"
      (t/is (= :foo (get v t-b)))
      (t/is (= :foo (get-in {:test v} [:test t-b]))))
    
    (t/testing "via destructuring"
      (let [{a t-a b t-b} v]
        (t/is (= 42 a))
        (t/is (= :foo b))))

    (t/testing "access fails if keys not in struct-map"
      (t/is (throws #(:foo v)))
      (t/is (throws #(get v :foo)))
      (t/is (throws #(let [{foo :foo} v]))))
    ))

(t/deftest empty-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/is (= #{t-a t-b}
             (set (keys (empty v)))))
    (t/is (= (sut/struct-map T t-a nil t-b nil)
             (empty v)))))

(t/deftest modify-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/testing "via assoc"
      (t/is (= (sut/struct-map T
                               t-a 42
                               t-b :bar)
               (assoc v t-b :bar))))

    (t/testing "via key fn"
      (t/is (= (sut/struct-map T
                               t-a 42
                               t-b :bar)
               (t-b v :bar))))
    
    (t/testing "via update"
      (t/is (= (sut/struct-map T
                               t-a -42
                               t-b :foo)
               (update v t-a -))))

    (t/testing "update fails if key not in struct-map"
      (t/is (throws #(assoc v :foo))))
    ))

(t/deftest keyed-map-test
  (t/testing "keys also work for hash-maps"
    (let [v {t-a 42
             t-b :foo}]
      (t/testing "access"
        (t/is (= 42 (t-a v)))
        (t/is (= :foo (get v t-b)))

        (t/is (= nil (t-b {})))
        (t/is (= nil (get {} t-b)))
        )

      (t/testing "update"
       (t/is (= {t-a 42 t-b :bar}
                (t-b v :bar)))
    
       (t/is (= {t-a 42 t-b :bar}
                (assoc v t-b :bar))))))
  )

(t/deftest compare-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)
        v2 (sut/struct-map T
                           t-a 42
                           t-b :foo)
        v3 {t-a 42
            t-b :foo}]

    (t/is (= t-a t-a) "Key equal itself")

    (t/is (= T T) "Types equal itself")

    (t/is (= v v) "Equals itself")

    (t/is (and (= v v2) (= v2 v)) "Equals other struct-map")

    (t/is (= v v3) "Equals other maps with same keys")
    (t/is (= v3 v) "Other maps with same keys are equal")

    (t/is (not (= v {:foo 42})) "Does not equals maps with other keys")
    (t/is (not (= {:foo 42} v)) "Maps with other keys are not equal")
    ))

(t/deftest type-test-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]

    (t/testing "struct-maps are maps"
      (t/is (map? v)))
    
    (t/testing "instance?"
      (t/is (sut/instance? T v))

      (t/is (not (sut/instance? T 42)))
      (t/is (not (sut/instance? T {}))))

    (t/testing "satisfies?"
      (t/is (not (sut/satisfies? T 42)))
      
      (t/is (sut/satisfies? T {t-a nil
                               t-b :foo
                               :x :y}))

      (t/is (not (sut/satisfies? T {t-a 42}))))

    #_(t/testing "normal maps can be ok too"
      ;; option: allow this or not?
      (t/is (sut/instance? T {t-a 42
                              t-b :foo}))

      (t/is (not (sut/instance? T {t-a 42})))
      )
    ))

(t/deftest reflection-test
  ;; requires iterator to be implemented
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/testing "reduce and reduce-kv"
      (t/is (= #{[t-a 42] [t-b :foo]}
               (reduce (fn [r [k v]]
                         (conj r [k v]))
                       #{}
                       v)))
      (t/is (= #{[t-a 42] [t-b :foo]}
               (reduce-kv (fn [r k v]
                            (conj r [k v]))
                          #{}
                          v))))

    (t/testing "keys"
      (t/is #{t-a t-b}
            (set (keys v))))
    (t/testing "vals"
      (t/is #{42 :foo}
            (set (vals v))))))

(t/deftest into-test
  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    (t/is (= #{[t-a 42] [t-b :foo]}
             (into #{} v))))

  (let [v (sut/struct-map T
                          t-a 42
                          t-b :foo)]
    ;; requires cons to be implemented
    (t/is (= v
             (into (sut/struct-map T t-a nil t-b nil)
                   #{[t-a 42] [t-b :foo]})))

    (t/is (throws
           #(into (sut/struct-map T t-a nil t-b nil)
                  #{[:bar :baz]}))
          "cannot add other keys")))


#?(:clj
   (t/deftest non-generative-clj-test
     (let [[t1 v1] (eval '(vector (active.clojure.struct/def-struct A [a])
                                  (active.clojure.struct/struct-map A a :foo)))
           [t2 v2] (eval '(vector (active.clojure.struct/def-struct A [a])
                                  (active.clojure.struct/struct-map A a :foo)))]
       (t/is (= t1 t2))
       
       (t/is (= v1 v2))

       (t/is (sut/instance? t1 v2))
       (t/is (sut/instance? t2 v1))
       )))

(defrecord AllInt []
  validator/IMapValidator
  (-validate-field! [this changed-key changed-value]
    (when-not (int? changed-value)
      (throw (ex-info "Not an int" {:v changed-value}))))
  (-validate-map! [this m changed-keys]
    (doseq [k changed-keys]
      (let [v (get m k)]
        (when-not (int? v)
          (throw (ex-info "Not an int" {:v v})))))))

(t/deftest validator-test

  (sut/def-struct ValidatedT [t-a t-b])
  (sut/set-validator! ValidatedT (AllInt.))
  
  (let [valid (sut/struct-map ValidatedT t-a 42 t-b 21)]
    (t/testing "contruction checks for validity"
      (t/is (throws #(sut/struct-map ValidatedT t-a :foo t-b 21))))

    (t/testing "modification checks for validity"
      (t/is (throws #(assoc valid t-a :foo)))
      (t/is (throws #(into valid {t-a :foo})))
      (t/is (throws #(empty valid))))

    (t/testing "satisfies? checks for validity"  
      (t/is (sut/satisfies? ValidatedT {t-a 11 t-b 22}))
      (t/is (not (sut/satisfies? ValidatedT {t-a :foo t-b :bar})))))
  )

(t/deftest number-of-fields-test
  ;; more than 21 fields works.
  (sut/def-struct BigT [t-a0 t-a1 t-a2 t-a3 t-a4 t-a5 t-a6 t-a7 t-a8 t-a9
                        t-a10 t-a11 t-a12 t-a13 t-a14 t-a15 t-a16 t-a17 t-a18 t-a19
                        t-a20 t-a21 t-a22 t-a23 t-a24 t-a25 t-a26 t-a27 t-a28 t-a29])
  (t/is (sut/instance? BigT
                       (sut/struct-map BigT
                                       t-a0 nil t-a1 nil t-a2 nil t-a3 nil t-a4 nil t-a5 nil t-a6 nil t-a7 nil t-a8 nil t-a9 nil
                                       t-a10 nil t-a11 nil t-a12 nil t-a13 nil t-a14 nil t-a15 nil t-a16 nil t-a17 nil t-a18 nil t-a19 nil
                                       t-a20 nil t-a21 nil t-a22 nil t-a23 nil t-a24 nil t-a25 nil t-a26 nil t-a27 nil t-a28 nil t-a29 nil))))
