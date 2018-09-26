(ns active.clojure.sum-type-test
  (:require #?(:clj [active.clojure.record :refer (define-record-type)])
            #?(:clj [active.clojure.sum-type :refer (define-sum-type match)])
            #?(:clj [clojure.test :refer :all])
            [active.clojure.sum-type-data-test :as data]
            ;; The following is needed because the unique test
            ;; below contains `Throwable`.
            #?(:cljs [active.clojure.condition :refer (Throwable)])
            #?(:cljs [cljs.test]))

  #?(:cljs
     (:require-macros [cljs.test
                       :refer (is deftest run-tests testing)]
                      [active.clojure.record :refer (define-record-type)]
                      [active.clojure.sum-type :refer (define-sum-type)])))



(define-record-type Red
  (make-red saturation) red?
  [saturation red-saturation])

(define-record-type Green
  (make-green saturation) green?
  [saturation green-saturation])

(define-record-type Blue
  (make-blue saturation) blue?
  [saturation blue-saturation])


(def red (make-red 0.4))
(def green (make-green 1.0))
(def blue (make-blue 0.2))

(define-record-type UltraViolet
  (make-ultra-violet wave-length) ultra-violet?
  [wave-length ultra-violet-wave-length])

(define-record-type InfraRed
  (make-infra-red wave-length) infra-red?
  [wave-length infra-red-wave-length])

(def ultra-violet (make-ultra-violet 42))
(def infra-red (make-infra-red 43))

(define-sum-type RGBColor
  rgb-color?
  [red? green? blue?])

(define-sum-type Invisible
  invisible?
  [ultra-violet? infra-red?])

(define-sum-type All
  all?
  [rgb-color? invisible?])

(deftest sum-type-predicate
  (is (rgb-color? red))
  (is (rgb-color? green))
  (is (rgb-color? blue))

  (is (not (invisible? red)))
  (is (not (invisible? green)))
  (is (not (invisible? blue)))

  (is (invisible? infra-red))
  (is (invisible? ultra-violet))

  (is (not (rgb-color? infra-red)))
  (is (not (rgb-color? ultra-violet)))

  (is (all? red))
  (is (all? green))
  (is (all? blue))
  (is (all? infra-red))
  (is (all? ultra-violet)))

(defn match-rgb [rgb-color]
  (match rgb-color? rgb-color
         red? (red-saturation rgb-color)
         green? (green-saturation rgb-color)
         blue? (blue-saturation rgb-color)))

(deftest working-matching
  (is (= (match-rgb red) 0.4))
  (is (= (match-rgb green) 1.0))
  (is (= (match-rgb blue) 0.2)))


(deftest working-matching-default
  (letfn [(foo [rgb-color]
            (match rgb-color? rgb-color
                   red? "Hello"
                   :default "Bye!"))]
    (is (= (foo red) "Hello"))
    (is (= (foo blue) "Bye!"))))

(deftest wrong-argument-type
  (is (thrown? Throwable (match-rgb 2))))


(deftest combined-sum-type

  (letfn [(foo [color]
            (match all? color
                   red? "Visible"
                   blue? "Visible"
                   green? "Visible"
                   infra-red? "Invisible"
                   ultra-violet? "Invisible"))]
    (is (= (foo red) "Visible"))
    (is (= (foo blue) "Visible"))
    (is (= (foo infra-red) "Invisible")))

  (letfn [(foo [color]
            (match all? color
                   red? "Red"
                   blue? "Blue"
                   green? "Green"
                   invisible? "Opaque"))]
    (is (= (foo red) "Red"))
    (is (= (foo blue) "Blue"))
    (is (= (foo infra-red) "Opaque"))))


(deftest extractor-tests
  (letfn [(desaturate [color]
            (match all? color
                   (make-red s) (make-red (/ s 2))
                   (make-green s) (make-green (/ s 2))
                   (make-blue s) (make-blue (/ s 2))
                   invisible? color))]
    (is (= (make-red 6)  (desaturate (make-red 12))))
    (is (= (make-green 6)(desaturate (make-green 12))))
    (is (= (make-blue 6) (desaturate (make-blue 12))))
    (is (= (make-ultra-violet 123) (desaturate (make-ultra-violet 123))))))


(deftest nestes-extractor
  (letfn [(crazy [color]
            (match all? color

                   (make-ultra-violet containing-color)
                   (match rgb-color? containing-color
                          (make-red a) (str "You got red with " a)
                          (make-blue b) (str "You got blue with " b)
                          (make-green _) (str "Oh, this is green!"))

                   all?
                   "It wasn rgb in invisible disguise :("))]

    (is (= "You got red with green" (crazy (make-ultra-violet (make-red "green")))))
    (is (= "You got blue with green" (crazy (make-ultra-violet (make-blue "green")))))
    (is (= "Oh, this is green!" (crazy (make-ultra-violet (make-red "green")))))
    (is (= "It wasn rgb in invisible disguise :(" (crazy (make-infra-red 123))))))


(define-sum-type FormsAndColors forms&colors? [data/circle? data/square? rgb-color?])

(deftest from-other-ns
  (letfn [(form-or-color [foc]
            (match forms&colors? foc
                   data/circle? "It's a circle!"
                   (data/make-square a b) (str "It's a square with " a " and " b)
                   rgb-color? "It's a color!"))]

    (is (= "It's a circle!" (form-or-color (data/make-circle 12))))
    (is (= "It's a square with 12 and 42" (form-or-color (data/make-square 12 42))))
    (is (= "It's a color!" (form-or-color (make-red 42))))))
