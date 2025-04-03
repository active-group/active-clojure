(ns active.clojure.dynj-test
  (:require [active.clojure.dynj :as sut]
            [clojure.test :as t]))

(sut/declare-dynj foo "Foo" [bar])

(t/deftest declare-dynj-test
  ;; (t/is (:dynamic (meta #'foo)))
  (t/is (= "Foo" (:docstring (meta #'foo)))))

(t/deftest binding-test
  (t/is (thrown? Exception (foo 4)))
  
  (sut/binding [foo (fn [x] (* x 2))]
    (t/is (= 8 (foo 4)))))

(t/deftest threading-test
  ;; threads 'inherit' current bindings
  (sut/binding [foo (fn [x] (* x 2))]
    (t/is (= 8 @(future (foo 4))))))

(t/deftest bound-fn-test
  ;; bind a function to current bindings
  (let [f (sut/binding [foo (fn [x] (* x 2))]
            (sut/bound-fn* (fn [v]
                             (foo v))))]
    (t/is (= 8 (f 4)))))

(t/deftest with-bindings*-test
  (t/is
   (= 7
      (sut/with-bindings*
        ;; just "foo" doesn't work here; Var is expected
        {#'foo (fn [x] (+ x 4))}
        ;; expects a thunk
        (fn [] (foo 3))))))

(sut/defn-dynj bar [arg]
  (* 3 arg))

(t/deftest defn-dynj-default-implementation-test
  (t/is (= 9 (bar 3))))
