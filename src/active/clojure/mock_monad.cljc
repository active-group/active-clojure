(ns active.clojure.mock-monad
  "Mock monadic programs"
  (:require [active.clojure.monad :as monad]
            #?(:clj [active.clojure.record :refer :all])
            #?(:clj [active.clojure.macro :refer [if-cljs]])
            #?(:clj [clojure.test :refer :all])
            #?(:cljs [active.clojure.cljs.record :refer-macros [define-record-type]])
            #?(:cljs [active.clojure.macro :refer-macros [if-cljs]])
            #?(:cljs [cljs.test :refer-macros [is]])))

(define-record-type Mock
  ^{:doc "Run `(check! m)` a command `m`, then execute `(replace m)` instead.

  `check!` should contain `is` assertions - it's not enough to return a
  boolean."}
  (mock check! get-result)
  mock?
  [check! mock-check!
   get-result mock-get-result])

(defn- mocked-result [mock m]
  ((mock-get-result mock) m))

(defn- check-mock! [mock m]
  ((mock-check! mock) m))

(defn- run-mock-commands [run-any env state m]
  (let [[mock & r-mocks] (::mocks state)]
    (if mock
      (do (check-mock! mock m)

          (let [nstate (assoc state ::mocks r-mocks)]
            (run-any env nstate (mocked-result mock m))))
      (do
        (is (= nil (monad/reify-command m)) "Unexpected command after end of mock list.")
        monad/unknown-command))))

(defn mock-commands [mocks]
  (monad/make-monad-command-config
   run-mock-commands
   {}
   {::mocks mocks}))

;; clojure-check assertions as a monadic command:
;; NOTE Don't use this in CLJS
(def check-mocks-empty
  (monad/monadic
   [mocks (monad/get-state-component ::mocks)]
   (let [rmocks (map monad/reify-command mocks)])
   (if-cljs
    ;; We must be careful here to call the function version in CLJS
    ;; (otherwise, it expands to clojure.test/is and not cljs.test/is)
     (monad/return (cljs.test/is (empty? rmocks) "Did not see expected mocked commands."))
     (monad/return (clojure.test/is (empty? rmocks) "Did not see expected mocked commands.")))
    (monad/return nil)))

(defn mock-effect
  "If `(= m-expected m)` returns true for a command `m`, then execute `m-replacement` instead."
  [m-expected m-replacement]
  (mock (fn [m]
          (if-cljs
           (cljs.test/is (= (monad/reify-command m-expected)
                            (monad/reify-command m)))
           (clojure.test/is (= (monad/reify-command m-expected)
                               (monad/reify-command m)))))
        (constantly m-replacement)))

(defn mock-result
  "If `(= m-expected m)` returns true for a command `m`, then return `value` instead."
  [m-expected value]
  (mock-effect m-expected (monad/return value)))

(def mock-ignore
  ^{:doc "Replace the next (unhandled) command by `(return nil)`, no matter what it is."}
  (mock (constantly nil) (constantly (monad/return nil))))

;; FIXME This should be renamed to `mock-run-monad`
(defn mock-execute-monad
  ([mocks m]
   (mock-execute-monad (monad/null-monad-command-config {} {}) mocks m))
  ([command-config mocks m]
   (monad/run-free-reader-state-exception
    (monad/combine-monad-command-configs command-config
                                         (mock-commands mocks))
    (monad/monadic
     [res m]
     ;; check that mock stack is empty at 'end'
     check-mocks-empty
     (monad/return res)))))

;; FIXME This should be renamed to `mock-execute-monad`
(defn mock-run-monad
  "Run m under the given monad command configs, and the given mocked commands, returning the result of m.
   `mocks` should be a sequence, whose values can be created by the
  `mock`, `mock-result` or `mock-effect` and other functions above, and are
  expected to appear in that order while executing `m`."
  ([mocks m]
   (mock-run-monad (monad/null-monad-command-config {} {}) mocks m))
  ([command-config mocks m]
   (first (mock-execute-monad command-config mocks m))))

(defn with-mock-run-monad
  "Immediately calls f with a function of two arguments, `mocks` and
  `m`, which can be repeatedly called to evaluate monadic commands
  `m`, with mocked commands like in `test-run-monad`. The monad state
  is preserved from call to call.  Returns `[result state]`."
  [command-config f]
  (let [state (atom {})]
    (f (fn [mocks m]
         (let [st1 @state
               [r st2]
               (mock-run-monad command-config
                               mocks
                               (monad/monadic
                                (monad/put-state! (assoc st1 ::mocks mocks))
                                [r m]
                                [st2 (monad/get-state)]
                                (monad/return [r st2])))]
           (reset! state st2)
           [r st2])))))

(defn is-mocked-state
  "Takes an `expectation` and the `monad-command-config-state` after running a mock.
  Applies `is` to the `=` of both, dissocing the ::mocks from the returned state."
  [expectation mock-result-state]
  (#?(:clj clojure.test/is :cljs cljs.test/is)
   (= expectation (dissoc mock-result-state ::mocks))))
