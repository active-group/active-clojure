(ns active.clojure.effect-test
  (:require [active.clojure.effect :as sut]
            [clojure.test :as t]))

(sut/declare-effect foo "Foo" [bar])

(t/deftest declare-effect-test
  ;; (t/is (:dynamic (meta #'foo)))
  (t/is (= "Foo" (:docstring (meta #'foo)))))

(t/deftest with-effects-test
  (sut/with-effects [foo (fn [x] (* x 2))]
    (t/is (= 8 (foo 4)))))

(t/deftest threading-test
  (sut/with-effects [foo (fn [x] (* x 2))]
    (t/is (= 8 @(future (foo 4))))))
