(ns active.clojure.record-test
  (:require #+clj [active.clojure.record :refer (define-record-type)]
            #+clj [clojure.test :refer :all]
            #+cljs ;; The following is needed because the unique test
		   ;; below contains `Throwable`.
            #+cljs [active.clojure.condition :refer (Throwable)]
            #+cljs [cemerick.cljs.test])
  #+cljs 
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [active.clojure.record :refer (define-record-type)]))

#+cljs
(enable-console-print!)

(define-record-type Pare
  (kons a b)
  pare?
  (a kar)
  (b kdr))

(defrecord FakePare [a b])

(deftest simple
  (let [r (kons 1 2)]
    (is (pare? r))
    (is (= 1 (kar r)))
    (is (= 2 (kdr r)))))

(deftest unique
  (is (thrown? Throwable
               (kar (FakePare. 1 2)))))

(define-record-type Pu
  (make-pu c a)
  pu?
  (a pua)
  (b pub)
  (c puc))

(deftest constructor
  (let [p (make-pu 1 2)]
    (is (pu? p))
    (is (not (pare? p)))
    (is (= 2 (pua p)))
    (is (= 1 (puc p)))
    (is (nil? (pub p)))))
