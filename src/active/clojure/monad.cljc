(ns active.clojure.monad
  "Monad related functionality, particularly free monads."
  (:require [active.clojure.freer-monad :as fm])
  #?(:cljs (:require-macros [active.clojure.monad :refer [monadic]])))

(def free-return fm/return)
(def free-return? fm/return?)
(def free-return-val fm/return-result)
(def return fm/return)

(def free-bind fm/bind)
(def free-bind? fm/bind?)
(def free-bind-monad fm/bind-first-action)
(defn free-bind-cont [b] #(fm/apply-continuations (fm/bind-continuations b) %))

(def call-cc fm/call-cc)
(def call-cc? fm/call-cc?)
(def call-cc-f fm/call-cc-f)

(defmacro monadic [& ?stmts] `(fm/monadic ~@?stmts))

(def sequ fm/sequ)
(def sequ_ fm/sequ_)

(def free-throw fm/signal)
(def free-throw? fm/signal?)
(def free-throw-exception fm/signal-exception)

(def with-handler fm/with-handler)
(def with-handler? fm/with-handler?)
(def with-handler-handler fm/with-handler-handler)
(def with-handler-body fm/with-handler-body)
(def and-finally fm/and-finally)
(def bind-except fm/bind-except)

(def make-exception-value fm/make-exception-value)
(def exception-value? fm/exception-value?)
(def exception-value-exception fm/exception-value-exception)

(def get-env fm/get-env)
(def get-env? fm/get-env)

(def get-env-component fm/get-env-component)

(def with-env fm/with-env)
(def with-env? fm/with-env?)
(def with-env-trans fm/with-env-trans)
(def with-env-body fm/with-env-body)

(def with-env-component fm/with-env-component)

(def get-state fm/get-state)
(def get-state? fm/get-state?)

(def put-state! fm/put-state!)
(def put-state? fm/put-state!?)
(def put-state-state fm/put-state-state)

(def get-state-component fm/get-state-component)
(def put-state-component! fm/put-state-component!)
(def update-state-component! fm/update-state-component!)
(def make-unknown-command fm/make-unknown-command)
(def unknown-command? fm/unknown-command?)
(def unknown-command fm/unknown-command)

(def make-monad-command-config fm/make-monad-command-config)
(def monad-command-config-run-command fm/monad-command-config-run-command)
(def monad-command-config-env fm/monad-command-config-env)
(def monad-command-config-state fm/monad-command-config-state)

(def run-no-commands fm/run-no-commands)

(def combine-monad-command-configs fm/combine-monad-command-configs)

(def null-monad-command-config fm/null-monad-command-config)

(def run-free-reader-state-exception fm/run-freer-reader-state-exception)
(def execute-free-reader-state-exception fm/execute-freer-reader-state-exception)

(def run-monadic-swiss-army fm/run-monadic-swiss-army)
(def execute-monadic-swiss-army fm/execute-monadic-swiss-army)

(def reify-as fm/reify-as)
(def reified? fm/reified?)
(def reify-command fm/reify-command)
