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

(t/deftest bound-fn*-test
  ;; bind a function to current bindings
  (let [f (dynj/binding [foo (fn [x] (* x 2))]
            (dynj/bound-fn* (fn [v]
                              (foo v))))]
    (t/is (= 8 (f 4))))
  ;; it would be "unbound" otherwise
  (let [f (dynj/binding [foo (fn [x] (* x 2))]
            (fn [v] (foo v)))]
    (t/is (thrown? Exception (f 4)))))

(t/deftest bind-fn*-test
  (let [bindings {#'foo #'str}
        f (fn [x] (foo (inc x)))
        bound (dynj/bind-fn* bindings f)]
    (t/is (= "5" (bound 4)))))

(t/deftest with-bindings*-test
  (t/is
   (= 7
      (dynj/with-bindings*
        ;; just "foo" doesn't work here; Var is expected
        {#'foo (fn [x] (+ x 4))}
        ;; expects a thunk
        (fn [] (foo 3))))))

(t/deftest with-bindings-test
  (t/is
   (= 11
      (dynj/with-bindings
        {#'foo (fn [x] (+ x 4))}
        (foo 7)))))

(dynj/defn-dynj bar [arg]
  (* 3 arg))

(t/deftest defn-dynj-default-implementation-test
  (t/is (= 9 (bar 3))))

(t/deftest merge-dynjs-test
  (let [b1 {#'foo #'inc}
        b2 {#'bar (fn [x] (str x "foo"))}
        bindings (dynj/merge-dynjs b1 b2)]
    (t/is "17foo"
          (dynj/with-bindings bindings
            (bar (foo 16))))
    (dynj/merge-dynjs b1 b2))
  (t/is
   (thrown? AssertionError
            (dynj/merge-dynjs {#'println (fn [s] (str s "foo"))}
                              {#'foo #'inc}))))
