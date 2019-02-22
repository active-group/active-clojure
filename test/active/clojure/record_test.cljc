(ns active.clojure.record-test
  (:require #?(:clj [active.clojure.record :refer (define-record-type)])
            [active.clojure.lens :as lens]
            [clojure.spec.test.alpha :as spec-test]
            #?(:clj [active.clojure.record-data-test :as r-data])
            #?(:clj [active.clojure.record-nongenerative-test])
            #?(:clj [clojure.test :refer :all])
            ;; The following is needed because the unique test
            ;; below contains `Throwable`.
            #?(:cljs [active.clojure.condition :refer (Throwable)])
            #?(:cljs [cljs.test])
            [clojure.spec.alpha :as spec])
  #?(:cljs
     (:require-macros [cljs.test :refer (is deftest run-tests testing)]
                      [active.clojure.record :refer [define-record-type]])))

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


;; Uninstantiated fields

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
#?(:clj (define-record-type IntString
          {:spec ::IntString}
          (make-int-string int string)
          int-string?
          [^{:spec int?} int int-string-int
           ^{:spec string?} string int-string-string]))

#?(:clj (define-record-type EvenAny
          {:spec ::EvenAny}
          (make-even-any even any)
          even-any?
          [^{:spec (and #(int? %) #(even? %))} even even-any-even
           any even-any-any]))

#?(:clj (define-record-type Container
          {:spec ::Container}
          (make-container value)
          container?
          [^{:spec ::IntString} value container-value]))

#?(:clj (defn includes?
          [^Exception e & subs]
          (every? #(clojure.string/includes? (.getMessage e) %)
                  subs)))

#?(:clj (deftest constructor-spec-test
          ;; Needs to be called, so that function spec errors are given:
          (spec-test/instrument)
          (try (make-int-string 2.2 "a")
               (catch Exception e
                 (is (includes? e ":int" "int?"))))
          (try (make-int-string 3 3)
               (catch Exception e
                 (is (includes? e ":string" "string?"))))
          (try (make-even-any 3 :anything)
               (catch Exception e
                 (is (includes? e ":even"))))
          (testing "spec'd record as field"
            (try (make-container 5)
                 (catch Exception e
                   (is (includes? e ":value" "IntString")))))
          (spec-test/unstrument)))

#?(:clj (deftest record-spec-test
          (is (spec/valid? ::IntString (make-int-string 3 "H")))
          (is (not (spec/valid? ::IntString 3)))
          (is (not (spec/valid? ::IntString (make-pu 3 "H"))))
          (testing "spec'd record as field"
            (is (spec/valid? ::Container (make-container (make-int-string 3 "H"))))
            (is (not (spec/valid? ::Container (make-container 5)))))))

;; Record with other spec name
#?(:clj (define-record-type RWOSN
          {:spec ::MySpecName}
          (make-rwosn value)
          rwosn?
          [^{:spec int?} value rwosn-value]))

#?(:clj (deftest record-with-other-spec-name-test
          (testing "record-spec-test"
            (spec-test/unstrument)
            (is (spec/valid? ::MySpecName (make-rwosn 5)))
            (is (not (spec/valid? ::MySpecName (make-rwosn "H")))))))

;;; Providing own `clojure.lang.IHashEq` implementation
#?(:clj (define-record-type FirstImportant
          (make-first-important a b)
          s?
          [a first-important-a
           b first-important-b]
          clojure.lang.IHashEq
          (hasheq [this] 3)
          (hashCode [this] 3)
          (equals [this other] (= (first-important-a this) (first-important-a other)))))

#?(:clj (defn equals [^FirstImportant a ^FirstImportant b]
          (.equals a b)))

#?(:clj (deftest providing-own-ihasheq-implementation-test
          (is (equals (make-first-important 1 1) (make-first-important 1 0)))
          (is (not (equals (make-first-important 1 1) (make-first-important 0 1))))))

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
#?(:clj (define-record-type GenerativeRecord
          (make-generative-record field)
          generative-record?
          [field generative-record-field]))

#?(:clj (define-record-type GenerativeRecord
          (make-generative-record)
          generative-record?
          []))

;;; Nongenerative
#?(:clj (define-record-type NonGenerativeRecord
          {:nongenerative "NonGenerativeRecord"}
          (make-non-generative-record field)
          non-generative-record?
          [field non-generative-record-field]))

;; Helper macro that allows to test a macro.
#?(:clj (defmacro throws-exception?
          [form]
          (try
            (eval form)
            (catch Exception e
              (= "This record type definition already exists with different arguments."
                 (.getMessage e))))))

#?(:clj (deftest nongenerative-record-test
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
                 [field ngrons-field])))))

;;; Test automatic generation of nongenerative id
#?(:clj (define-record-type NonGenerativeRecordAuto
          {:nongenerative true}
          (make-non-generative-record-auto a)
          non-generative-record-auto?
          [a non-generative-record-auto-a]))

#?(:clj (deftest nongenerative-record-auto-id-test
          (is (contains? @active.clojure.record/global-record-type-registry
                         "active.clojure.record-test/NonGenerativeRecordAuto"))))

;;; Test record type without arrow constructor (ie ->TypeName)
#?(:clj (define-record-type RecordWithoutArrowConstructor
          {:no-arrow-constructor? true}
          (make-rwac a)
          rwac?
          [a rwac-a]))

#?(:clj (define-record-type RecordWithArrowConstructor
          (make-rac a)
          rac?
          [a rac-a]))

#?(:clj (deftest record-without-arrow-constructor-test
          (is (thrown? Exception (->RecordWithoutArrowConstructor 5)))
          (is (->RecordWithArrowConstructor 5))))

;;; Test record type without map protocol
#?(:clj (define-record-type RecordWithoutMapProtocol
          {:no-map-protocol? true}
          (make-rwmp a b)
          rwim?
          [a rwmp-a
           b rwmp-b]))

#?(:clj (deftest record-without-map-protocol-test
          (is (= false
                 (map? (make-rwmp 3 4))))))
