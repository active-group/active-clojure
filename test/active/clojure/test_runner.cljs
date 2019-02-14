(ns active.clojure.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [active.clojure.condition-test]
            [active.clojure.debug-test]
            [active.clojure.lens-test]
            [active.clojure.monad-test]
            [active.clojure.mock-monad-test]
            [active.clojure.record-test]
            [active.clojure.record-spec-test]
            [active.clojure.match-test]
            [active.clojure.config-test]))

(doo-tests 'active.clojure.condition-test
           'active.clojure.debug-test
           'active.clojure.lens-test
           'active.clojure.monad-test
           'active.clojure.mock-monad-test
           'active.clojure.record-test
           'active.clojure.record-spec-test
           'active.clojure.match-test
           'active.clojure.config-test)
