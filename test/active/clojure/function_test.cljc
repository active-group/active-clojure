(ns active.clojure.function-test
  (:require [active.clojure.function :as f]
            #?(:clj [active.clojure.function-test-util :as u])
            #?(:clj [clojure.test :refer :all])
            #?(:cljs [cljs.test :refer-macros [deftest is testing]]))
  #?(:cljs (:require-macros [active.clojure.function-test-util :as u])))

(deftest partial-test
  (u/generate-tests "partial" f/partial partial
                    [[+] [list 0]]
                    [[] [42] [1 2 3 4]])

  ;; Clojurescript bug: https://dev.clojure.org/jira/browse/CLJS-3024
  #(:clj
    ;; all (critical) arities of our IFn:
    (u/generate-tests "partial" f/partial partial
                      [[list -2 -1]]
                      [[0] [1]
                       [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17]
                       [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18]
                       [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19]
                       [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20]
                       [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21]])))

(deftest constantly-test
  (u/generate-tests "constantly" f/constantly constantly
                  [[1] [2]]
                  [[] [4 5 6]]))

(deftest comp-test
  (u/generate-tests "comp" f/comp comp
                  [[]]
                  [[1]])
  (u/generate-tests "comp" f/comp comp
                  [[-] [- *]]
                  [[5 7] [1 3]])
  (u/generate-tests "comp" f/comp comp
                  [[reverse reverse]]
                  [[[]] [[1 2 3]]]))

(deftest complement-test
  (u/generate-tests "complement" f/complement complement
                  [[nil?] [boolean]]
                  [[nil] [42]]))

(deftest juxt-test
  (u/generate-tests "juxt" f/juxt juxt
                  [[first count]]
                  [["Hello"] [[1 2 3]]]))

(deftest fnil-test
  (u/generate-tests "fnil" f/fnil fnil
                    [[list 42] [list 42 21]]
                    [[nil 1] [nil 2 3] [1 2 3]]))

(deftest every-pred-test
  (u/generate-tests "every-pred" f/every-pred every-pred
                    [[odd?] [even? #(> % 5)]]
                    [[] [1] [1 2 3]]))

(deftest some-fn-test
  (u/generate-tests "some-fn" f/some-fn some-fn
                    [[even? #(< % 10)]]
                    [[] [1 2 3]]))

(deftest completing-test
  (u/generate-tests "completing" f/completing completing
                    [[concat] [concat reverse]]
                    [[[1]] [[1] [2]]]))

  ;; I don't think bound-fn* is doing much in clojurescript anyway

#?(:clj
   (deftest bound-fn*-test
     (def ^:dynamic *some-var* nil)
     (defn bound-fn*-test-f [res] (deliver res *some-var*))
       
     (u/generate-tests "bound-fn*" f/bound-fn* bound-fn*
                       [[list]]
                       [])
     (let [res (promise)]      
       (binding [*some-var* "goodbye"]
         (let [g (f/bound-fn* bound-fn*-test-f)]
           (.start (Thread. (fn []
                              (g res))))))

       (is (= (deref res 1000 :timeout)
              "goodbye")))))
