(ns active.clojure.record-test
  (:require [active.clojure.lens :as lens]
            [clojure.spec.test.alpha :as spec-test]
            #?(:clj [clojure.spec.alpha :as spec])
            #?(:clj [active.clojure.clj.record :refer [define-record-type]])
            #?(:clj [active.clojure.record-data-test :as r-data])
            #?(:clj [active.clojure.record-nongenerative-test])
            #?(:clj [active.clojure.record-runtime :as rrun])
            #?(:clj [clojure.test :refer :all])
            ;; The following is needed because the unique test
            ;; below contains `Throwable`.
            #?(:cljs [active.clojure.condition :refer (Throwable)])
            #?(:cljs [active.clojure.cljs.record])
            #?(:cljs [active.clojure.record-nongenerative-test])
            #?(:cljs [cljs.test])
            #?(:cljs [cljs.spec.alpha :as spec])
            #?(:cljs [active.clojure.record-runtime :as rrun]))
  #?(:cljs
     (:require-macros [cljs.test :refer (is deftest run-tests testing)]
                      [active.clojure.cljs.record :refer [define-record-type]]
                      [active.clojure.record-test :refer [throws-exception? is-in-registry?]])))

#?(:cljs
(enable-console-print!))

(define-record-type Pare
  (kons a b)
  pare?
  [a kar
   b kdr])

(defrecord FakePare [a b])

(deftest simple
  (let [r (kons 1 2)]
    (is (pare? r))
    (is (= 1 (kar r)))
    (is (= 2 (kdr r)))))

(deftest unique
  (is (thrown? Throwable
               (kar (FakePare. 1 2)))))


;; Omit constructor args

(define-record-type Schmare
  schmons
  schmare?
  [a schmar
   b schmdr])

(deftest simple-omit
  (let [r (schmons 1 2)]
    (is (schmare? r))
    (is (= 1 (schmar r)))
    (is (= 2 (schmdr r)))))


;; Uninstantiated fields and custom constructor args order

(define-record-type Pu
  (make-pu c a)
  pu?
  [a pua
   b pub
   c puc])

(deftest constructor
  (let [p (make-pu 1 2)]
    (is (pu? p))
    (is (not (pare? p)))
    (is (= 2 (pua p)))
    (is (= 1 (puc p)))
    (is (nil? (pub p)))))

(define-record-type Mu
  (make-mu a b)
  mu?
  [a mua
   b mub
   c muc])

(define-record-type MuRTD
  {:java-class? false ; CLJ
   :rtd-record? true} ; CLJS
  (make-murtd a b)
  murtd?
  [a murtda
   b murtdb
   c murtdc])

(deftest lens-shove-works-with-omitted-constructor-args-test
  ;; Lens Law 1
  (let [m (make-mu 1 2)]
    (is (= 3
           (muc (lens/shove m muc 3)))))
  (let [m (make-murtd 1 2)]
    (is (= 3
           (murtdc (lens/shove m murtdc 3))))))

(define-record-type Nu
  (make-nu a c b)
  nu?
  [a nua
   b nub
   c nuc])

(define-record-type NuRTD
  (make-nurtd a c b)
  nurtd?
  [a nurtda
   b nurtdb
   c nurtdc])

(deftest lens-shove-works-with-swapped-constructor-args-test
  ;; Lens Law 1
  (let [n (make-nu 1 2 3)]
    (is (= (make-nu  1 3 3)
           (lens/shove n nuc 3))))
  (let [n (make-nurtd 1 2 3)]
    (is (= (make-nurtd  1 3 3)
           (lens/shove n nurtdc 3)))))


;; Records with lenses

(defn law-1-holds [l data v]
  ;; you get back what you put in
  (is (= v
         (lens/yank (lens/shove data l v) l))))

(defn law-2-holds [l data]
  ;; putting back what you got doesn't change anything
  (is (= data
         (lens/shove data l (lens/yank data l)))))

(defn law-3-holds [l data v1 v2]
  ;; second set wins, or setting once is the same as setting twice
  (is (= (lens/shove data l v1)
         (lens/shove (lens/shove data l v2) l v1))))

(defn lens-laws-hold [l data v1 v2]
  (and (law-1-holds l data v1)
       (law-2-holds l data)
       (law-3-holds l data v1 v2)))

(define-record-type ^{:doc "Lens example"} LensPare
  (lens-kons a b)
  lens-pare?
  [^{:doc "a field"} a lens-kar
   ^{:doc "b field"} b lens-kdr])

(deftest pare-lens
  (lens-laws-hold lens-kar (lens-kons 1 2) 23 42)
  (lens-laws-hold lens-kdr (lens-kons 1 2) 23 42)
  (is (= (lens-kons "a" 42)
         (lens/shove (lens-kons 23 42) lens-kar "a")))
  (is (= (lens-kons 23 "b")
         (lens/shove (lens-kons 23 42) lens-kdr "b"))))

(define-record-type Quadruple
  (quadruple a b c d)
  quadruple?
  [a quadruple-one
   b quadruple-two
   c quadruple-three
   d quadruple-four])

(deftest quadruple-lens
  (lens-laws-hold quadruple-one (quadruple 'a 'b 'c 'd) 12 78)
  (lens-laws-hold quadruple-three (quadruple 'a 'b 'c 'd) 12 78)
  (is (= (quadruple 4 8 15 16)
         (lens/shove (quadruple 108 8 15 16) quadruple-one 4))))


;;; Test Records with Specs
(define-record-type IntString
  {:spec ::IntString}
  (make-int-string i s)
  int-string?
  [^{:spec int?} i int-string-int
   ^{:spec string?} s int-string-string])

(define-record-type EvenAny
   {:spec ::EvenAny}
   (make-even-any even any)
   even-any?
   [^{:spec (and #(int? %) #(even? %))} even even-any-even
    any even-any-any])

(define-record-type Container
   {:spec ::Container}
   (make-container value)
   container?
   [^{:spec ::IntString} value container-value])

(defn includes?
   [^Exception e & subs]
  #?(:clj (every? #(clojure.string/includes? (.getMessage e) %)
                  subs)
     :cljs (every? #(clojure.string/includes? (.toString e) %)
                   subs)))

(deftest constructor-spec-test
  ;; Needs to be called, so that function spec errors are given
  ;; "Instrument takes a fully-qualified symbol to resolve it in
  ;; the context of the current namespace."
  (spec-test/instrument `constructor-ns)
  (try (make-int-string 2.2 "a")
       (catch #?(:clj Exception :cljs js/Error) e
         (is (includes? e "2.2" "int?"))))
  (try (make-int-string 3 3)
       (catch #?(:clj Exception :cljs js/Error) e
         (is (includes? e "3" "string?"))))
  (try (make-even-any 3 :anything)
       (catch #?(:clj Exception :cljs js/Error) e
         (is (includes? e ":even"))))
  (testing "spec'd record as field"
    (try (make-container 5)
         (catch #?(:clj Exception :cljs js/Error) e
           (is (includes? e "5" "IntString")))))
  (spec-test/unstrument))

(deftest record-spec-test
   (is (spec/valid? ::IntString (make-int-string 3 "H")))
   (is (not (spec/valid? ::IntString 3)))
   (is (not (spec/valid? ::IntString (make-pu 3 "H"))))
   (testing "spec'd record as field"
     (is (spec/valid? ::Container (make-container (make-int-string 3 "H"))))
     (is (not (spec/valid? ::Container (make-container 5))))))

;; Record with other spec name
(define-record-type RWOSN
  {:spec ::MySpecName}
  (make-rwosn value)
  rwosn?
  [^{:spec int?} value rwosn-value])

(deftest record-with-other-spec-name-test
  (testing "record-spec-test"
    (spec-test/unstrument)
    (is (spec/valid? ::MySpecName (make-rwosn 5)))
    (is (not (spec/valid? ::MySpecName (make-rwosn "H"))))))

;;; Providing an options map should still work
(define-record-type RecordWithOptions
   {:bla 3}
   (make-record-with-options value)
   record-with-options?
   [value record-with-options-value])

(deftest providing-options-map-test
   (let [rwo (make-record-with-options 5)]
     (is (record-with-options? rwo))
     (is (= 5 (record-with-options-value rwo)))))

;;;; Nongenerative Records tests

;;; Generative option, one should be allowed to define two types with
;;; different arguments but same type name
(define-record-type GenerativeRecord
   (make-generative-record field)
   generative-record?
   [field generative-record-field])

;;; CLJS only:
;;; After evaluating the following expression, it is expected, that you get warnings like these
;; [Figwheel:WARNING] Compile Warning: map->GenerativeRecord at line 212 is being replaced  /Users/kaan/projekte/active-clojure/test/active/clojure/record_test.cljc   line:217  column:1
;; [Figwheel:WARNING] Compile Warning: generative-record? at line 212 is being replaced  /Users/kaan/projekte/active-clojure/test/active/clojure/record_test.cljc   line:217  column:1
;; [Figwheel:WARNING] Compile Warning: make-generative-record at line 212 is being replaced  /Users/kaan/projekte/active-clojure/test/active/clojure/record_test.cljc   line:217  column:1

(define-record-type GenerativeRecord
   (make-generative-record)
   generative-record?
   [])

;;; Nongenerative
(define-record-type NonGenerativeRecord
   {:nongenerative "NonGenerativeRecord"}
   (make-non-generative-record field)
   non-generative-record?
   [field non-generative-record-field])

;; Helper macro that allows to test a macro.
#?(:clj (defmacro throws-exception?
          [form]
          (try
            (eval form)
            (catch Exception e
              (= "This record type definition already exists with different arguments."
                 (.getMessage e))))))

(deftest nongenerative-record-test
   ;; Other implementation should throw an error
   (is (throws-exception?
        (define-record-type NonGenerativeRecord
          {:nongenerative "NonGenerativeRecord"}
          (make-non-generative-record)
          non-generative-record?
          [])))
   ;; The exact same definition should not throw an exception
   (is (nil? (define-record-type NonGenerativeRecord
               {:nongenerative "NonGenerativeRecord"}
               (make-non-generative-record field)
               non-generative-record?
               [field non-generative-record-field])))

   ;; same record-type-definition of other namespace should fail
   (is (throws-exception?
        (define-record-type NonGROtherNS
          {:nongenerative "NonGROtherNS"}
          (make-ngrons field)
          ngrons?
          [field ngrons-field]))))

;;; Test automatic generation of nongenerative id
(define-record-type NonGenerativeRecordAuto
   {:nongenerative true}
   (make-non-generative-record-auto a)
   non-generative-record-auto?
   [a non-generative-record-auto-a])

;;; Testing auto-id needs a macro in CLJS case to access the registry
#?(:clj (defmacro is-in-registry? []
          (contains? @active.clojure.record-helper/global-record-type-registry
                     "active.clojure.record-test/NonGenerativeRecordAuto")))

(deftest nongenerative-record-auto-id-test
  #?(:clj (is (contains? @active.clojure.record-helper/global-record-type-registry
                         "active.clojure.record-test/NonGenerativeRecordAuto"))
     :cljs (is (is-in-registry?))))

;;; Test record type without arrow constructor (ie ->TypeName)
(define-record-type RecordWithoutArrowConstructor
   {:arrow-constructor? false}
   (make-rwac a)
   rwac?
   [a rwac-a])

(define-record-type RecordWithArrowConstructor
   (make-rac a)
   rac?
   [a rac-a])

(deftest record-without-arrow-constructor-test
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (->RecordWithoutArrowConstructor 5)))
  (is (->RecordWithArrowConstructor 5)))

;;; Test record type without map protocol
(define-record-type RecordWithoutMapProtocol
  {:map-protocol? false}
  (make-rwmp a b)
  rwim?
  [a rwmp-a
   b rwmp-b])

(deftest record-without-map-protocol-test
  (is (= false
         (map? (make-rwmp 3 4))))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (assoc (make-rwmp 3 4) :c 4)))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (dissoc (make-rwmp 3 4) :a))))



;;;; Providing own `equality` implementation
;;; (CLJ) In Clojure, our records defaultly implement `IPersistentMap` which is an
;;; instance of `IPersistentCollection`,
;;; this entails that equality testing is done by the `equiv` method and not `equals`.
;;; Therefore you have to provide an implementation of `IPersistentMap` and not
;;; `clojure.lang.IHashEq`
;;; ----
;;; (CLJS) In CLJS you have to provide an `IEquiv` implementation
(define-record-type FirstImportant
   (make-first-important a b)
   s?
   [a first-important-a
    b first-important-b]
  #?@(:clj
      [clojure.lang.IPersistentMap
       (equiv [this other] (= (first-important-a this) (first-important-a other)))]
      :cljs
      [IEquiv
       (-equiv [this other] (= (first-important-a this) (first-important-a other)))]))

(deftest providing-own-equality-implementation-test
  #?(:clj
     (is (= (make-first-important 1 1) (make-first-important 1 1)))
     :cljs
     (is (= (make-first-important 1 1) (make-first-important 1 1))))
  #?(:clj
     (is (= (make-first-important 1 1) (make-first-important 1 0)))
     :cljs
     (is (= (make-first-important 1 1) (make-first-important 1 0))))
  #?(:clj
     (is (not (= (make-first-important 1 1) (make-first-important 0 1))))
     :cljs
     (is (not (= (make-first-important 1 1) (make-first-important 0 1))))))

;;; Remove a (default) interface from record
;;; (This example, yields the same result as providing `:no-map-protocol? true` in the options map)
(define-record-type RecordWithoutInterfaces
  {:remove-interfaces [#?(:clj java.util.Map :cljs IMap)
                       #?(:clj clojure.lang.IPersistentMap :cljs IAssociative)]}
   (make-rwi a)
   rwi?
   [a rwi-a])

(deftest remove-interfaces-test
   (is (= false
          (map? (make-rwi 3))))
   (is (thrown? #?(:clj Exception :cljs js/Error)
                (assoc (make-rwi 3) :c 4)))
   (is (thrown? #?(:clj Exception :cljs js/Error)
                (dissoc (make-rwi 3) :a))))

;;; implement user made protocol

(defprotocol SaySomething
  (say [this]))

(define-record-type SayIt
  (make-say-it a)
  say-it?
  [a say-it-a]
  SaySomething
  (say [this] (str "Hello, my field value is " (say-it-a this))))

(deftest implement-own-protocol-test
  (is (= "Hello, my field value is 3"
         (say (make-say-it 3)))))

;;;; No java-class option tests
(define-record-type ICreateNoJavaClass
  {:java-class? false}
  (make-icnjc a b)
  icnjc?
  [a icnjc-a
   b icnjc-b])
(define-record-type ICreateAJavaClass
  {:java-class? true}
  (make-icajc a b)
  icajc?
  [a icajc-a
   b icajc-b])

#?(:clj
   (deftest no-java-class-test
     (let [without (make-icnjc 5 "foo")
           with (make-icajc 5 "foo")]
       (is (not (class? ICreateNoJavaClass)))
       (is (not (record? without)))
       (is (not (map? without)))
       (is (rrun/record? without))

       (is (class? ICreateAJavaClass))
       (is (record? with))
       (is (map? with))
       (is (not (rrun/record? with))))))

#?(:clj
   (deftest rtd-records-functions-test
     (let [r (make-icnjc 1 2)]
       (is (icnjc? r))
       (is (not (icajc? r)))
       (is (= 1 (icnjc-a r)))
       (is (= 2 (icnjc-b r))))))

;;; :meta option (works only with :java-class? false)
(define-record-type ^{:foo "I am meta information"} MetaInfo
  {:java-class? false}
  meta-info
  meta-info?
  [])

#?(:clj
   (deftest meta-info-test
     (is (= "I am meta information"
            (:foo (MetaInfo :meta))))))


;;;; rtd-records
#?(:cljs
   (define-record-type ICreateRtd
     {:rtd-record? true}
     (make-icrtd a b)
     icrtd?
     [a icrtd-a
      b icrtd-b]))
#?(:cljs
   (define-record-type ICreateNoRtd
     {:rtd-record? false}
     (make-icnrtd a b)
     icnrtd?
     [a icnrtd-a
      b icnrtd-b]))


#?(:cljs
   (deftest rtd-record-test
     (let [without (make-icnrtd 5 "foo")
           with (make-icrtd 5 "foo")]
       (is (record? without))
       (is (map? without))
       (is (not (rrun/record? without)))

       (is (not (record? with)))
       (is (not (map? with)))
       (is (rrun/record? with)))))

#?(:cljs
   (deftest rtd-records-functions-cljs-test
     (let [r (make-icrtd 1 2)]
       (is (icrtd? r))
       (is (not (icnrtd? r)))
       (is (= 1 (icrtd-a r)))
       (is (= 2 (icrtd-b r))))))

;;; :meta option (works only with :rtd-record? true)
#?(:cljs
   (define-record-type ^{:foo "I am meta information"} MetaInfoCljs
     {:rtd-record? true}
     meta-infoc
     meta-infoc?
     []))

#?(:cljs
   (deftest meta-info-CLJS-test
     (is (= "I am meta information"
            (:foo (MetaInfoCljs :meta))))))



;;; test meta-info inheritance

(define-record-type ^{:foo "bar" :private true} MetaInheritance
  ^{:floo "blar" :doc "Alternative documentation"} make-mi
  ^{:floo "blar" :doc "Alternative documentation"} mi?
  [a ^{:floo "blar" :doc "Alternative documentation"} mi-a])

(deftest meta-inheritance-test
  (is (= "bar" (:foo (meta #'->MetaInheritance))))
  (is (= true (:private (meta #'->MetaInheritance))))
  (is (= "bar" (:foo (meta #'map->MetaInheritance))))
  (is (= true (:private (meta #'map->MetaInheritance))))
  (is (= "bar" (:foo (meta #'mi-a))))
  (is (= "blar" (:floo (meta #'mi-a))))
  (is (= "Alternative documentation" (:doc (meta #'mi-a))))
  (is (= true (:private (meta #'mi-a))))
  (is (= "bar" (:foo (meta #'make-mi))))
  (is (= "blar" (:floo (meta #'make-mi))))
  (is (= "Alternative documentation" (:doc (meta #'make-mi))))
  (is (= true (:private (meta #'make-mi))))
  (is (= "bar" (:foo (meta #'mi?))))
  (is (= "blar" (:floo (meta #'mi?))))
  (is (= "Alternative documentation" (:doc (meta #'mi?))))
  (is (= true (:private (meta #'mi?)))))

(define-record-type ^{:foo "bar" :private true} MetaInheritanceRTD
  {:java-class? false ; CLJ
   :rtd-record? true  ; CLJS
   }
  ^{:floo "blar" :doc "Alternative documentation"} make-mi-rtd
  ^{:floo "blar" :doc "Alternative documentation"} mi-rtd?
  [a ^{:floo "blar" :doc "Alternative documentation"} mi-rtd-a])

(deftest meta-inheritance-rtd-test
  (is (= "bar" (:foo (meta #'mi-rtd-a))))
  (is (= "blar" (:floo (meta #'mi-rtd-a))))
  (is (= "Alternative documentation" (:doc (meta #'mi-rtd-a))))
  (is (= true (:private (meta #'mi-rtd-a))))
  (is (= "bar" (:foo (meta #'make-mi-rtd))))
  (is (= "blar" (:floo (meta #'make-mi-rtd))))
  (is (= "Alternative documentation" (:doc (meta #'make-mi-rtd))))
  (is (= true (:private (meta #'make-mi-rtd))))
  (is (= "bar" (:foo (meta #'mi-rtd?))))
  (is (= "blar" (:floo (meta #'mi-rtd?))))
  (is (= "Alternative documentation" (:doc (meta #'mi-rtd?))))
  (is (= true (:private (meta #'mi-rtd?)))))
