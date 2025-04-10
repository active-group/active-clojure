(ns active.clojure.dynj-test
  (:require [active.clojure.dynj :as dynj]
            [clojure.test :as t]))

(dynj/declare-dynj foo "Foo" [bar])

(t/deftest declare-dynj-test
  ;; (t/is (:dynamic (meta #'foo)))
  (t/is (= "Foo" (:docstring (meta #'foo)))))

(t/deftest binding-test
  (t/is (thrown? Exception (foo 4)))
  
  (dynj/binding [foo (fn [x] (* x 2))]
    (t/is (= 8 (foo 4)))))

(t/deftest threading-test
  ;; threads 'inherit' current bindings
  (dynj/binding [foo (fn [x] (* x 2))]
    (t/is (= 8 @(future (foo 4))))))

(t/deftest bound-fn-test
  ;; bind a function to current bindings
  (let [f (dynj/binding [foo (fn [x] (* x 2))]
            (dynj/bound-fn* (fn [v]
                              (foo v))))]
    (t/is (= 8 (f 4)))))

(t/deftest with-bindings*-test
  (t/is
   (= 7
      (dynj/with-bindings*
        ;; just "foo" doesn't work here; Var is expected
        {#'foo (fn [x] (+ x 4))}
        ;; expects a thunk
        (fn [] (foo 3))))))

(dynj/defn-dynj bar [arg]
  (* 3 arg))

(t/deftest defn-dynj-default-implementation-test
  (t/is (= 9 (bar 3))))
