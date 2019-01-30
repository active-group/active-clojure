(ns active.clojure.record-test
  (:require #?(:clj [active.clojure.record :refer (define-record-type)])
            [active.clojure.lens :as lens]
            [clojure.spec.test.alpha :as spec-test]
            [active.clojure.record-data-test :as r-data]
            #?(:clj [clojure.test :refer :all])
            ;; The following is needed because the unique test
            ;; below contains `Throwable`.
            #?(:cljs [active.clojure.condition :refer (Throwable)])
            #?(:cljs [cljs.test])
            [clojure.spec.alpha :as spec])
  #?(:cljs
     (:require-macros [cljs.test
                       :refer (is deftest run-tests testing)]
                      [active.clojure.record :refer (define-record-type)])))

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
(define-record-type IntString
  (make-int-string int string)
  int-string?
  [^{:spec int?} int int-string-int
   ^{:spec string?} string int-string-string])

(define-record-type EvenAny
  (make-even-any even any)
  even-any?
  [^{:spec (and #(int? %) #(even? %))} even even-any-even
   any even-any-any])

(define-record-type Container
  (make-container value)
  container?
  [^{:spec ::IntString} value container-value])

(defn includes?
  [^Exception e & subs]
  (every? #(clojure.string/includes? (.getMessage e) %)
          subs))

(deftest constructor-spec-test
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
  (spec-test/unstrument))

(deftest record-spec-test
  (is (spec/valid? ::IntString (make-int-string 3 "H")))
  (is (not (spec/valid? ::IntString 3)))
  (is (not (spec/valid? ::IntString (make-pu 3 "H"))))
  (testing "spec'd record as field"
    (is (spec/valid? ::Container (make-container (make-int-string 3 "H"))))
    (is (not (spec/valid? ::Container (make-container 5))))))


(deftest test-specs-from-separate-namespace-test
  (testing "constructor spec tests"
    (spec-test/instrument)
    (try (r-data/make-int-int 2.2 3)
         (catch Exception e
           (is (includes? e ":fst" "int?"))))
    (try (r-data/make-int-int 4 2.5)
         (catch Exception e
           (is (includes? e ":snd" "int?"))))
    (try (r-data/make-container 5)
         (catch Exception e
           (is (includes? e ":value" "IntInt"))))
    (spec-test/unstrument))
  (testing "record-spec-test"
    (is (spec/valid? ::r-data/IntInt (r-data/make-int-int 3 4)))
    (is (not (spec/valid? ::r-data/IntInt (r-data/make-int-int 3 "h"))))
    (is (not (spec/valid? ::r-data/Container (r-data/make-container 5))))))

;;; Providing own `clojure.lang.IHashEq` implementation
(define-record-type FirstImportant
  (make-first-important a b)
  s?
  [a first-important-a
   b first-important-b]
  clojure.lang.IHashEq
  (hasheq [this] 3)
  (hashCode [this] 3)
  (equals [this other] (= (first-important-a this) (first-important-a other))))

(defn equals [^FirstImportant a ^FirstImportant b]
  (.equals a b))

(deftest providing-own-ihasheq-implementation-test
  (is (equals (make-first-important 1 1) (make-first-important 1 0)))
  (is (not (equals (make-first-important 1 1) (make-first-important 0 1)))))
