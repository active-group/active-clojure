(ns active.clojure.record-runtime-test
  (:require #?(:clj [clojure.test :refer :all])
            [active.clojure.record-runtime :as r :include-macros true]
            #?(:cljs [active.clojure.cljs.record])
            #?(:cljs [cljs.test]))
  #?(:cljs
     (:require-macros [cljs.test :refer (is deftest run-tests testing)])))

(def rtd0 (r/make-record-type-descriptor `rtd0 nil []))
(def rtd1 (r/make-record-type-descriptor `rtd1 nil [(r/make-record-field "f1")]))
(def rtd2 (r/make-record-type-descriptor `rtd2 nil [(r/make-record-field "f1")
                                                    (r/make-record-field "f2")]))
(def rtd100 (r/make-record-type-descriptor `rtd100 nil
                                           (map #(str "f" %) (range 100))))

(deftest t-make-record-simple
  (let [r0 (r/make-record rtd0)
        r1 (r/make-record rtd1 1)
        r2 (r/make-record rtd2 1 2)]
    (is (r/record? r0))
    (is (r/record-of-type? r0 rtd0))
    (is (not (r/record-of-type? r0 rtd1)))
    (is (r/record? r1))
    (is (= 1 (r/record-get rtd1 r1 0)))
    (is (r/record? r2))
    (is (= 1 (r/record-get rtd2 r2 0)))
    (is (= 2 (r/record-get rtd2 r2 1)))))

(deftest t-make-record-100
  (let [r100 (apply r/make-record rtd100 (range 100))]
    (doseq [i (range 100)]
      (is (= i (r/record-get rtd100 r100 i))))))

(deftest t-record-update
  (let [r2 (r/make-record rtd2 1 2)
        r2a (r/record-update rtd2 r2 1 5)]
    (is (= 1 (r/record-get rtd2 r2a 0)))
    (is (= 5 (r/record-get rtd2 r2a 1)))))

(deftest t-record-check
  (let [r2 (r/make-record rtd2 1 2)]
    (is (thrown? #?(:clj Error :cljs js/Error)
                 (r/record-get rtd0 r2 0)))
    (is (thrown? #?(:clj Error :cljs js/Error)
                 (r/record-update rtd0 r2 0 :new)))))

(deftest to-string-test
  (let [r0 (r/make-record rtd0)
        r1 (r/make-record rtd1 1)
        r2 (r/make-record rtd2 1 2)]
    (testing "str / toString"
      (is (= "active.clojure.record-runtime-test/rtd0{}"
             (str r0)))
      (is (= "active.clojure.record-runtime-test/rtd1{:f1 1}"
             (str r1)))
      (is (= "active.clojure.record-runtime-test/rtd2{:f1 1, :f2 2}"
             (str r2))))
    (testing "pr-str / print-method"
      (is (= "active.clojure.record-runtime-test/rtd0{}"
             (pr-str r0)))
      (is (= "active.clojure.record-runtime-test/rtd1{:f1 1}"
             (pr-str r1)))
      (is (= "active.clojure.record-runtime-test/rtd2{:f1 1, :f2 2}"
             (pr-str r2))))))


(defrecord NotRTDRecord [a b])
(def rtd0-2 (r/make-record-type-descriptor `rtd0-2 nil []))
(def rtd2-2 (r/make-record-type-descriptor `rtd2-2 nil [(r/make-record-field "f1")
                                                        (r/make-record-field "f2")]))

;; generate regexp for the expected error message
;; Todo: sth like escape for the cljs-regexp, \Q and \E is not available in JavaScript.
(defn expected-check-rtd!-throw-error-message [wrong-rec]
  (let [tstr (pr-str wrong-rec)
        res-str #?(:clj (str "^\\QNot a record of the correct type [[active.clojure.record-runtime-test/rtd2]]:" tstr "\\E$")
                   :cljs (str "^Not a record of the correct type \\[\\[active.clojure.record-runtime-test/rtd2\\]\\]:" tstr "$"))]
    (println res-str)
    (re-pattern res-str)))


(deftest record-check-rtd!-throws-when-wrong-record-type
  (testing "not even an rtd-record"
    (is (thrown-with-msg?
         #?(:clj Error :cljs js/Error)
         (expected-check-rtd!-throw-error-message 4)
         (r/record-check-rtd! ^RecordTypeDescriptor rtd2 4)))
    (is (thrown-with-msg?
         #?(:clj Error :cljs js/Error)
         (expected-check-rtd!-throw-error-message "blub")
         (r/record-check-rtd! rtd2 "blub")))
    (is (thrown-with-msg?
         #?(:clj Error :cljs js/Error)
         (expected-check-rtd!-throw-error-message (->NotRTDRecord 1 2))
         (r/record-check-rtd! rtd2 (->NotRTDRecord 1 2)))))
  (testing "wrong rtd-record"
    (is (thrown-with-msg?
         #?(:clj Error :cljs js/Error)
         (expected-check-rtd!-throw-error-message (r/make-record rtd0))
         (r/record-check-rtd! rtd2 (r/make-record rtd0))))
    (is (thrown-with-msg?
         #?(:clj Error :cljs js/Error)
         (expected-check-rtd!-throw-error-message (r/make-record rtd1))
         (r/record-check-rtd! rtd2 (r/make-record rtd1))))
    (is (thrown-with-msg?
         #?(:clj Error :cljs js/Error)
         (expected-check-rtd!-throw-error-message (r/make-record rtd2-2))
         (r/record-check-rtd! rtd2 (r/make-record rtd2-2))))
    (is (thrown-with-msg?
         #?(:clj Error :cljs js/Error)
         (expected-check-rtd!-throw-error-message (r/make-record rtd0-2))
         (r/record-check-rtd! rtd2 (r/make-record rtd0-2)))))
  (testing "correct record, shouldnt throw"
    (is (nil? (r/record-check-rtd! rtd0 (r/make-record rtd0)))))
  (testing "correct record, shouldnt throw"
    (is (nil? (r/record-check-rtd! rtd2 (r/make-record rtd2 1 2))))))
