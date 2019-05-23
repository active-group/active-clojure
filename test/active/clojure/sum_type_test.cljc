(ns active.clojure.sum-type-test
  (:require #?(:clj [active.clojure.record :refer (define-record-type)])
            #?(:clj [clojure.test :refer :all])
            [active.clojure.sum-type :as st :include-macros true]
            [active.clojure.sum-type-data-test :as data]
            #?(:cljs [active.clojure.cljs.record])
            #?(:cljs [cljs.test])
            [clojure.string :as str])
  #?(:cljs
     (:require-macros [cljs.test :refer (is deftest run-tests testing)]
                      [active.clojure.sum-type-test :refer [throws-exception?]]
                      [active.clojure.cljs.record :refer [define-record-type]])))

(define-record-type red
  #?(:clj {:java-class? false}
     :cljs {:rtd-record? true})
  (make-red saturation) red?
  [saturation red-saturation])

(define-record-type green
  #?(:clj {:java-class? false}
     :cljs {:rtd-record? true})
  (make-green saturation) green?
  [saturation green-saturation])

(define-record-type blue
  #?(:clj {:java-class? false}
     :cljs {:rtd-record? true})
  (make-blue saturation) blue?
  [saturation blue-saturation])


(st/define-sum-type rgb-color rgb-color? [red green blue])



(def red-inst (make-red 0.4))
(def green-inst (make-green 1.0))
(def blue-inst (make-blue 0.2))


(define-record-type ultra-violet
  #?(:clj {:java-class? false}
     :cljs {:rtd-record? true})
  (make-ultra-violet wave-length) ultra-violet?
  [wave-length ultra-violet-wave-length])

(define-record-type infra-red
  #?(:clj {:java-class? false}
     :cljs {:rtd-record? true})
  (make-infra-red wave-length) infra-red?
  [wave-length infra-red-wave-length])

(def ultra-violet-inst (make-ultra-violet 42))
(def infra-red-inst (make-infra-red 43))


(st/define-sum-type invisible
  invisible?
  [ultra-violet infra-red])


(st/define-sum-type all
  all?
  [rgb-color invisible])



(deftest sum-type-predicate
  (is (rgb-color? red-inst))
  (is (rgb-color? green-inst))
  (is (rgb-color? blue-inst))

  (is (not (invisible? red-inst)))
  (is (not (invisible? green-inst)))
  (is (not (invisible? blue-inst)))

  (is (invisible? infra-red-inst))
  (is (invisible? ultra-violet-inst))

  (is (not (rgb-color? infra-red-inst)))
  (is (not (rgb-color? ultra-violet-inst)))

  (is (all? red-inst))
  (is (all? green-inst))
  (is (all? blue-inst))
  (is (all? infra-red-inst))
  (is (all? ultra-violet-inst)))






;; (red? red-inst) (let [a (red-saturation red-inst)]
;;                   (red-saturation red-inst (/ a 2)))


;; #_(testing "A records type function stuff is accessible"

;;   (testing "in macro expansion"
;;     (is (= 4 (helper/inc simon))))

;;   (testing "in macro expansion"
;;     (is (= [4 4] (helper/incs [simon simon]))))

;;   (testing "during runtime")
;;   )



;; ;; (def a 4)


;; ;; (def red-inst (make-red 0.4))
;; ;; (def green-inst (make-green 1.0))
;; ;; (def blue-inst (make-blue 0.2))


;; ;; (define-record-type ultra-violet
;; ;;   #?(:clj {:java-class? false}
;; ;;      :cljs {:rtd-record? true})
;; ;;   (make-ultra-violet wave-length) ultra-violet?
;; ;;   [wave-length ultra-violet-wave-length])

;; ;; (define-record-type infra-red
;; ;;   #?(:clj {:java-class? false}
;; ;;      :cljs {:rtd-record? true})
;; ;;   (make-infra-red wave-length) infra-red?
;; ;;   [wave-length infra-red-wave-length])

;; ;; (def ultra-violet-inst (make-ultra-violet 42))
;; ;; (def infra-red-inst (make-infra-red 43))


;; ;; (st/define-sum-type rgb-color
;; ;;   rgb-color?
;; ;;   [red green blue])


;; ;; (st/define-sum-type invisible
;; ;;   invisible?
;; ;;   [ultra-violet infra-red])

;; ;; (st/define-sum-type all
;; ;;   all?
;; ;;   [rgb-color invisible])

;; ;; (deftest sum-type-predicate
;; ;;   (is (rgb-color? red-inst))
;; ;;   (is (rgb-color? green-inst))
;; ;;   (is (rgb-color? blue-inst))

;; ;;   (is (not (invisible? red-inst)))
;; ;;   (is (not (invisible? green-inst)))
;; ;;   (is (not (invisible? blue-inst)))

;; ;;   (is (invisible? infra-red-inst))
;; ;;   (is (invisible? ultra-violet-inst))

;; ;;   (is (not (rgb-color? infra-red-inst)))
;; ;;   (is (not (rgb-color? ultra-violet-inst)))

;; ;;   (is (all? red-inst))
;; ;;   (is (all? green-inst))
;; ;;   (is (all? blue-inst))
;; ;;   (is (all? infra-red-inst))
;; ;;   (is (all? ultra-violet-inst)))


(defn match-rgb [rgb-color-inst]
  (st/match rgb-color rgb-color-inst
    red? (red-saturation rgb-color-inst)
    green? (green-saturation rgb-color-inst)
    blue? (blue-saturation rgb-color-inst)))

(deftest working-matching
  (is (= (match-rgb red-inst) 0.4))
  (is (= (match-rgb green-inst) 1.0))
  (is (= (match-rgb blue-inst) 0.2)))


(deftest working-matching-default
  (letfn [(foo [rgb-color-inst]
            (st/match rgb-color rgb-color-inst
                   red? "Hello"
                   :default "Bye!"))]
    (is (= (foo red-inst) "Hello"))
    (is (= (foo blue-inst) "Bye!"))))

;; (deftest wrong-argument-type
;;   (is (thrown? Throwable (match-rgb 2))))


(deftest combined-sum-type

  (letfn [(foo [color]
            (st/match all color
                   red? "Visible"
                   blue? "Visible"
                   green? "Visible"
                   infra-red? "Invisible"
                   ultra-violet? "Invisible"))]
    (is (= (foo red-inst) "Visible"))
    (is (= (foo blue-inst) "Visible"))
    (is (= (foo infra-red-inst) "Invisible")))

  (letfn [(foo [color]
            (st/match all color
                   red? "Red"
                   blue? "Blue"
                   green? "Green"
                   invisible? "Opaque"))]
    (is (= (foo red-inst) "Red"))
    (is (= (foo blue-inst) "Blue"))
    (is (= (foo infra-red-inst) "Opaque"))))


(deftest extractor-tests
  (letfn [(desaturate [color]
            (st/match all color
                   (make-red s) (make-red (/ s 2))
                   (make-green s) (make-green (/ s 2))
                   (make-blue s) (make-blue (/ s 2))
                   invisible? color))]
    (is (= 6 (red-saturation (desaturate (make-red 12)))))
    (is (= 6 (green-saturation (desaturate (make-green 12)))))
    (is (= 6 (blue-saturation (desaturate (make-blue 12)))))
    (is (= 123 (ultra-violet-wave-length (desaturate (make-ultra-violet 123)))))))


(deftest nested-extractor
  (letfn [(crazy [color]
            (st/match all color

                   (make-ultra-violet containing-color)
                   (st/match rgb-color containing-color
                          (make-red a) (str "You got red with " a)
                          (make-blue b) (str "You got blue with " b)
                          (make-green _) (str "Oh, this is green!"))

                   all?
                   "It wasn rgb in invisible disguise :("))]

    (is (= "You got red with green" (crazy (make-ultra-violet (make-red "green")))))
    (is (= "You got blue with green" (crazy (make-ultra-violet (make-blue "green")))))
    (is (= "Oh, this is green!" (crazy (make-ultra-violet (make-green "green")))))
    (is (= "It wasn rgb in invisible disguise :(" (crazy (make-infra-red 123))))))


(st/define-sum-type forms&colors forms&colors? [data/circle data/square rgb-color])




(deftest from-other-ns

  (letfn [(form-or-color [foc]
            (st/match forms&colors foc
                   data/circle? "It's a circle!"
                   (data/make-square a b) (str "It's a square with " a " and " b)
                   rgb-color? "It's a color!"))]

    (is (= "It's a circle!" (form-or-color (data/make-circle 12))))
    (is (= "It's a square with 12 and 42" (form-or-color (data/make-square 12 42))))
    (is (= "It's a color!" (form-or-color (make-red 42))))))


#?(:clj (defmacro throws-exception?
          [msg form]
          (try
            (eval form)
            (catch Exception e
              (or
                (clojure.string/includes? (str e) msg)
                (clojure.string/includes? (.getMessage (.getCause e)) msg))))))


(deftest throws-when-unexhaustive-test
  (is (throws-exception?
        "Arguments of the following types will fail matching of type `"
        (st/match forms&colors foc
          data/circle? 12))))


(deftest throws-when-wrong-function-test
  (is (throws-exception?
        "The following functions don't belong to records or sum-types of type `"
        (st/match forms&colors foc
          odd? 12)))

  (is (throws-exception?
        "The following functions don't belong to records or sum-types of type `"
        (st/match forms&colors foc
          (odd? a) 12))))


(deftest throws-when-misplaced-default
  (is (throws-exception?
        "Default clause only allowed as last clause"
        (st/match rgb-color red-inst
          red? 1
          blue? 2
          :default 3
          green? 4))))

(deftest throws-when-uneven-clauses
  (is (throws-exception?
        "even number of clauses"
        (st/match rgb-color red-inst
          red? 1
          blue? 2
          3))))

(deftest throws-when-no-symbol
  (is (throws-exception?
        "must be a symbol"
        (st/match 12 red-inst
          red? 1
          blue? 2
          3))))

(deftest throws-when-no-sum-type
  (let [a 12]
   (is (throws-exception?
         "is no sum-type"
         (st/match a red-inst
           red? 1
           blue? 2)))))


(deftest throws-when-argument-wrong-type
  (is (thrown?
        #?(:cljs js/Object
           :clj IllegalArgumentException)
        (match-rgb 12))))


(define-record-type chaotic
  #?(:clj {:java-class? false}
     :cljs {:rtd-record? true})
  (make-chaotic a b) chaotic?
  [b chaotic-b
   a chaotic-a])

(st/define-sum-type chaotics chaotics? [chaotic])


(deftest extracts-differently-ordered-args
  (letfn [(cm [a]
            (st/match chaotics a
              (make-chaotic a _) a))]
    (is (= 1 (cm (make-chaotic 1 2))))))
