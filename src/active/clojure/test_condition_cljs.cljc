(ns active.clojure.test-condition-cljs
  (:require #?(:clj [clojure.test :as ct])
            #?(:cljs [cljs.test :as ct])
            [active.clojure.condition :as c]))

#?(:clj
   (defmethod ct/assert-expr 'raised? [_env ?msg ?form]
     (let [[_raised? ?condition-predicate ?expr] ?form]
       `(c/guard [con#
                  (~?condition-predicate con#)
                  (ct/do-report {:type :pass :message ~?msg
                                 :expected ~?condition-predicate})


                  :else
                  (ct/do-report {:type :fail
                                 :message "invalid condition"
                                 :expected (list ~?condition-predicate con#)
                                 :actual (list 'not (list ~?condition-predicate con#))})]


                 (let [val# ~?expr]
                   (ct/do-report {:type :fail
                                  :message "condition expected"
                                  :expected ~?condition-predicate
                                  :actual val#}))))))
