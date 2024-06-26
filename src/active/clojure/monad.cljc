(ns active.clojure.monad
  "Monad related functionality, particularly free monads."
  (:require [active.clojure.condition :as c]
            #?(:clj [active.clojure.record :refer [define-record-type]])
            #?(:cljs [active.clojure.cljs.record :refer-macros [define-record-type]])
            #?(:clj [clojure.core :as core])
            #?(:cljs [cljs.core :as core])
            )
  #?(:cljs (:require-macros [active.clojure.monad :refer [monadic]])))

(define-record-type ^:no-doc Return
  (free-return val)
  free-return?
  [val free-return-val])
(def return free-return)

(define-record-type Bind
  (make-free-bind monad cont)
  free-bind?
  [monad free-bind-monad
   cont free-bind-cont])

(define-record-type ^{:doc "Accepts a function `f` with one argument that
  returns a monadic value.  `call-cc` packages the current continuation as a
  function and passes it as an argument to `f`.  If the function is called
  later, the continuation of that call will be discarded and instead reinstate
  the continuation that was in effect when the function was created."}
  CallCC
  (call-cc f)
  call-cc?
  [f call-cc-f])

;; This marks a continuation that was captured by call-cc to be able to discard
;; the current continuation when calling the captured one.
(define-record-type ^:private ReplaceCont
  (replace-cont v cont)
  replace-cont?
  [v replace-cont-v
   cont replace-cont-cont])

(define-record-type ^{:doc "Accepts a arbitrary value `thing` that can be used
  to mark the type of the [[IntermediateResult]].  `pause` can be used to pause
  the current monadic calculation and return an [[Intermediate Result]]."}
  Pause
  (pause thing)
  pause?
  [thing pause-thing])

(define-record-type ^{:doc "Possible return value
  for [[run-monadic]], when monadic program wants to [[pause]]
  calculation.  Call [[intermediate-result-resume]] with a value [[v]] to resume
  the paused calculation with [[v]]."}
  IntermediateResult
  (make-intermediate-result thing resume)
  intermediate-result?
  [thing intermediate-result-thing
   resume intermediate-result-resume])

(defn free-bind
  "Bind/flatMap for the free monad."
  [mv f]
  ;; catch common errors early
  (when-not (some? mv)
    (if-let [{line :line column :column statement :statement} (meta mv)]
      (c/assertion-violation `free-bind
                             (str "unknown monad command in bind, line " line ", column " column)
                             mv
                             statement)
      (c/assertion-violation `free-bind
                             "unknown monad command in bind"
                             mv)))

  (cond
    (free-bind? mv) (Bind. (free-bind-monad mv)
                           (fn [x]
                             (let [next ((free-bind-cont mv) x)] ; yields a monad
                               (free-bind next f))))

    :else (make-free-bind mv f)))

(defn throw-illegal-argument-exception
  [msg]
  (c/assertion-violation `throw-illegal-argument-exception "Illegal argument" msg))

#?(:clj
(defmacro monadic
  "Construct a monadic computation.

  The syntax is `(monadic <stmt> ...)` where `<stmt>` is one of the following:

  - `[<pat> <exp> ...]` which creates monadic bindings
  - `(let <bindings>)` which creates regular bindings
  - anything else is just a regular expression, expected to yield a monadic value.

  Example:

      (monadic [first (ask \"what's your first name?\")
                last (ask \"what's your last name?\")]
               (let [s (str \"Hello, \" first \" \" last)])
               (tell s))"
  [& ?stmts]
  (if (empty? ?stmts)
    (throw-illegal-argument-exception (str "there must be at least one statement in " *ns* " " (meta &form)))
    (let [?stmt (first ?stmts)]
      (cond
        ;; monadic binding:
        (vector? ?stmt)
        `(monadic-binds ~?stmt (monadic ~@(rest ?stmts)))

        ;; non-monadic binding:
        (and (list? ?stmt)
             (= 'let (first ?stmt)))
        `(let ~(second ?stmt) (monadic ~@(rest ?stmts)))

        ;; anything else in the middle
        (not-empty (rest ?stmts))
        `(monadic-bind _# ~?stmt (monadic ~@(rest ?stmts)))

        ;; anything else in the end:
        :else
        ?stmt)))))

#?(:clj
   (defmacro ^:no-doc monadic-bind [?pat ?rhs ?cont]
     (let [mdata (assoc (meta ?rhs) :statement `(quote ~[?pat ?rhs]))]
       `(with-meta (free-bind ~?rhs
                           (fn [~?pat]
                             ~?cont))
          ~mdata))))

#?(:clj
   (defmacro ^:no-doc monadic-binds [?bindings ?cont]
     (when-not (even? (count ?bindings))
       (throw-illegal-argument-exception (str "bindings must be even-sized vector in "
                                              *ns* " " (meta &form))))
     (if (empty? ?bindings)
       ?cont
       `(monadic-bind ~(first ?bindings) ~(second ?bindings)
                      (monadic-binds ~(rest (rest ?bindings))
                                     ~?cont)))))


(defn sequ
  "Evaluate each action in the sequence from left to right, and collect the results."
  [ms]
  (let [f (fn f [ms res]
            (if (seq ms)
              (free-bind (first ms)
                         (fn [v]
                           (f (rest ms)
                              (cons v res))))
              (free-return (reverse res))))]
    (f ms '())))

(defn sequ_
  "Evaluate each action in the sequence from left to right, and ignore the results."
  [ms]
  (reduce (fn [q p]
            (free-bind p (fn [_] q)))
          (free-return nil)
          (reverse ms)))

(defn reduce-m
  "Like reduce, but takes a monadic function `m-fn` for folding the
  results.  Returns the final result."
  [m-fn acc xs]
  (if (empty? xs)
    (return acc)
    (monadic [res (m-fn acc (first xs))]
             (reduce-m m-fn res (rest xs)))))

(defn scan-m
  "Like `reduce-m`, but returns a sequence of each intermediate
  result.  The actual result is the last element of that sequence.

  `(last (scan-m f zero xs)) = (reduce-m f zero xs)` holds."
  [m-fn acc xs]
  (monadic [reversed (reduce-m (fn [acc* x]
                                 (monadic [res (m-fn (first acc*) x)]
                                          (return (cons res acc*))))
                               (list acc)
                               xs)]
           (return (reverse reversed))))

(define-record-type ^:no-doc Throw
  (free-throw exception)
  free-throw?
  [exception free-throw-exception])

; handler: condition -> monad
; body: monad

(define-record-type ^:no-doc WithHandler
  (with-handler handler body)
  with-handler?
  [handler with-handler-handler
   body with-handler-body])

(defn and-finally
  "Execute m, and always final-m, no matter if an exception occurs or not."
  [m final-m]
  (free-bind (with-handler (fn [e] (free-bind final-m (fn [_] (free-throw e))))
               m)
             (fn [r] (free-bind final-m (fn [_] (free-return r))))))

(defn bind-except
  "Evaluate m, and if an exception occurs continue with (handler exception), otherwise with (f value-of-m).
   Note that neither the handler result is to f, nor exceptions are caught in f."
  [m handler f]
  (free-bind (with-handler
               (fn [e] (free-bind (handler e)
                                 (fn [res] (free-return [:failed res]))))
               (free-bind m (fn [value] (free-return [:ok value]))))
             (fn [x]
               (case (first x)
                 :ok (f (second x))
                 :failed (free-return (second x))))))

(define-record-type ^:no-doc ExceptionValue
  (make-exception-value exception)
  exception-value?
  [exception exception-value-exception])

(define-record-type ^:no-doc GetEnv
  (get-env)
  get-env?
  [])

(defn get-env-component
  "Retrieve a named component of a map-valued environment."
  [name]
  (monadic
   [x (get-env)]
   (return (get x name))))

(define-record-type ^:no-doc WithEnv
  (with-env trans body)
  with-env?
  [trans with-env-trans
   body with-env-body])

(defn with-env-component
  "Transform a keyed component of a map-valued environment."
  [key f m]
  (with-env (fn [env]
              (update env key f))
    m))

(define-record-type ^:no-doc GetState
  (get-state)
  get-state?
  [])

(define-record-type ^:no-doc PutState
  (put-state! state)
  put-state!?
  [state put-state-state])

(defn get-state-component
  "Retrieve a named component of a map-valued state."
  [name]
  (monadic
   [x (get-state)]
   (return (get x name))))

(defn put-state-component!
  "Set a named component of a map-valued state."
  [name value]
  (monadic
   [x (get-state)
    _ (put-state! (assoc x name value))]
   (return nil)))

(defn update-state-component!
  "Calls f with the current value of the component and puts back return value as the new component."
  [name f & args]
  (monadic
   [val (get-state-component name)]
   (let [new (apply f val args)])
   (put-state-component! name new)))

(define-record-type ^:no-doc UnknownCommand
  (make-unknown-command)
  unknown-command?
  [])

(def ^{:doc "Marker that command functions can return to signal they don't recognize a command."}
  unknown-command (make-unknown-command))

(define-record-type
  ^{:doc "Configuration for supporting a set of monad commands."}
  MonadCommandConfig
  (make-monad-command-config run-command env state)
  monad-command-config?
  [run-command
   ^{:doc "`run-command` :: run-any env state comp -> (| [(| exception-value? val) state] unknown-command)
  Where `run-any` is a function for running any monad command of the signature
  `env state comp -> (| [(| exception-value? val) state] unknown-command)"}
   monad-command-config-run-command

   env ^{:doc "reader-monad initial environment represented as a mergable map"}
   monad-command-config-env

   state ^{:doc "state-monad initial state represented as a mergable map"}
   monad-command-config-state])

(defn- combine-run-commands
  "Combine a sequence of run-command functions into one."
  [rcs]
  (fn [run-any env state m]
    (loop [rcs rcs]
      (if (seq rcs)
        (let [r ((first rcs) run-any env state m)]
          (if-not (unknown-command? r)
            r
            (recur (rest rcs))))
        unknown-command))))

(defn run-no-commands
  "For use in [[make-monad-command-config]] when there are no commands."
  [_run-any _env _state _m]
  unknown-command)

(defn combine-monad-command-configs
  "Combine a sequence of monad-command configs into one.
  The earlier entries have precedence."
  [& mccs]
  (make-monad-command-config
   (combine-run-commands (map monad-command-config-run-command mccs))
   (apply merge (reverse (map monad-command-config-env mccs)))
   (apply merge (reverse (map monad-command-config-state mccs)))))

(defn null-monad-command-config
  "Monad command-configuration with no commands except for reader/state/exception."
  [env state]
  (make-monad-command-config run-no-commands env state))

(defn run-free-reader-state-exception
  "Run monadic computation in a reader-state-exception monad.

  - `command-config` is the configuration object for running commands
  - `m` is the computation to run
  - `state` an optional initial state (from a previous run) that is merged
    into the one from `command-config`

  Returns [result state]"
  [^MonadCommandConfig command-config m & [state]]
  (let [run-command (monad-command-config-run-command command-config)
        env (monad-command-config-env command-config)
        ;; we could try to add the initial-state of command-configs
        ;; that haven't been used yet; but a map-valued state is not
        ;; required by the interface. So we can only take one of them
        state (merge (monad-command-config-state command-config)
                     state)
        unknown-command (fn [bind m]
                          (if-let [{line :line column :column statement :statement} (meta bind)]
                            (c/assertion-violation `run-free-reader-state-exception
                                                   (str "unknown monad command in bind, line " line ", column " column ": " m)
                                                   bind m
                                                   statement)
                            (c/assertion-violation `run-free-reader-state-exception
                                                   (str "unknown monad command in bind: " m)
                                                   bind m)))]
    (letfn [(run [env state m]
              (cond
               (free-return? m) [(free-return-val m) state]
               (free-throw? m) [(make-exception-value (free-throw-exception m)) state]

               (free-bind? m)
               (let [m1 (:monad m)
                     cont (:cont m)]
                 (cond
                  (free-return? m1) (recur  env state (cont (free-return-val m1)))
                  (free-throw? m1) [(make-exception-value (free-throw-exception m1)) state]

                  (free-bind? m1) (unknown-command m m1)

                  (with-handler? m1)
                  (let [[x state] (run env state (with-handler-body m1))]
                    (if (exception-value? x)
                      (let [res (run env state ((with-handler-handler m1) (exception-value-exception x)))
                            [x state] res]
                        (if (exception-value? x)
                          res
                          (recur env state (cont x))))
                      (recur env state (cont x))))

                  (get-env? m1)
                  (recur env state (cont env))

                  (with-env? m1)
                  (let [res (run ((with-env-trans m1) env) state (with-env-body m1))
                        [x state] res]
                    (if (exception-value? x)
                      res
                      (recur env state (cont x))))

                  (get-state? m1)
                  (recur env state (cont state))

                  (put-state!? m1)
                  (recur env (put-state-state m1) (cont nil))

                  :else
                  (let [res (run-command run env state m1)]
                    (if (unknown-command? res)
                      (unknown-command m m1)
                      (let [[x state] res]
                        (if (exception-value? x)
                          res
                          (recur env state (cont x))))))))

               (with-handler? m)
               (let [[x state :as res] (run env state (with-handler-body m))]
                 (if (exception-value? x)
                   (recur env state ((with-handler-handler m) (exception-value-exception x)))
                   res))

               (get-env? m)
               [env state]

               (with-env? m)
               (recur ((with-env-trans m) env) state (with-env-body m))

               (get-state? m)
               [state state]

               (put-state!? m)
               [nil (put-state-state m)]

               :else
               (let [res (run-command run env state m)]
                 (if (unknown-command? res)
                   (unknown-command nil m)
                   res))))]

      (run env state m))))

(defn execute-free-reader-state-exception
  "Run monadic computation in a reader-state-exception monad, turning exceptions
  into Clojure exceptions.

  - `command-config` is the configuration object for running commands
  - `m` is the computation to run
  - `state` is an optional initial state (from a previous run)

  Returns [result state].
  "
  [^MonadCommandConfig command-config m & [state]]
  (let [res (run-free-reader-state-exception command-config m state)
        [r _state] res]
    (if (exception-value? r)
      (throw (exception-value-exception r))
      res)))

(defn run-monadic
  "Run a monadic computation in an almighty monad.  Same as
  [[`run-free-reader-state-exception`]] with the addition of `call-cc`
  and `pause`.

  - `command-config` is the configuration object for running commands
  - `m` is the computation to run
  - `state` an optional initial state (from a previous run) that is merged
    into the one from `command-config`

  Returns [result state]"
  [^MonadCommandConfig command-config m & [state]]
  (let [run-command (monad-command-config-run-command command-config)
        env (monad-command-config-env command-config)
        ;; we could try to add the initial-state of command-configs
        ;; that haven't been used yet; but a map-valued state is not
        ;; required by the interface. So we can only take one of them
        state (merge (monad-command-config-state command-config)
                     state)
        unknown-command (fn [bind m]
                          (if-let [{line :line column :column statement :statement} (meta bind)]
                            (c/assertion-violation `run-monadic
                                                   (str "unknown monad command in bind, line " line ", column " column ": " m)
                                                   bind m
                                                   statement)
                            (c/assertion-violation `run-monadic
                                                   (str "unknown monad command in bind: " m)
                                                   bind m)))]
    (letfn [(run [env state m]
              (cond
               (free-return? m) [(free-return-val m) state]
               (free-throw? m) [(make-exception-value (free-throw-exception m)) state]

               (call-cc? m) (recur env state ((call-cc-f m) (fn [v] (replace-cont v return))))
               (replace-cont? m) (recur env state ((replace-cont-cont m) (replace-cont-v m)))

               (pause? m) [(make-intermediate-result (pause-thing m) (fn [v] (run env state (return v))))
                           state]

               (free-bind? m)
               (let [m1 (:monad m)
                     cont (:cont m)]
                 (cond
                  (free-return? m1) (recur  env state (cont (free-return-val m1)))
                  (free-throw? m1) [(make-exception-value (free-throw-exception m1)) state]

                  (call-cc? m1) (recur env state (free-bind ((call-cc-f m1) (fn [v] (replace-cont v cont))) cont))
                  (replace-cont? m1) (recur env state ((replace-cont-cont m1) (replace-cont-v m1)))

                  (pause? m1) [(make-intermediate-result (pause-thing m1) (fn [v] (run env state (cont v))))
                               state]

                  (free-bind? m1) (unknown-command m m1)

                  (with-handler? m1)
                  (let [[x state] (run env state (with-handler-body m1))]
                    (if (exception-value? x)
                      (let [res (run env state ((with-handler-handler m1) (exception-value-exception x)))
                            [x state] res]
                        (if (exception-value? x)
                          res
                          (recur env state (cont x))))
                      (recur env state (cont x))))

                  (get-env? m1)
                  (recur env state (cont env))

                  (with-env? m1)
                  (let [res (run ((with-env-trans m1) env) state (with-env-body m1))
                        [x state] res]
                    (if (exception-value? x)
                      res
                      (recur env state (cont x))))

                  (get-state? m1)
                  (recur env state (cont state))

                  (put-state!? m1)
                  (recur env (put-state-state m1) (cont nil))

                  :else
                  (let [res (run-command run env state m1)]
                    (if (unknown-command? res)
                      (unknown-command m m1)
                      (let [[x state] res]
                        (if (exception-value? x)
                          res
                          (recur env state (cont x))))))))

               (with-handler? m)
               (let [[x state :as res] (run env state (with-handler-body m))]
                 (if (exception-value? x)
                   (recur env state ((with-handler-handler m) (exception-value-exception x)))
                   res))

               (get-env? m)
               [env state]

               (with-env? m)
               (recur ((with-env-trans m) env) state (with-env-body m))

               (get-state? m)
               [state state]

               (put-state!? m)
               [nil (put-state-state m)]

               :else
               (let [res (run-command run env state m)]
                 (if (unknown-command? res)
                   (unknown-command nil m)
                   res))))]

      (run env state m))))

(defn execute-monadic
  "Run monadic computation in an almighty monad, turning exceptions
  into Clojure exceptions.  See [[`run-monadic`]].

  - `command-config` is the configuration object for running commands
  - `m` is the computation to run
  - `state` is an optional initial state (from a previous run)

  Returns [result state].
  "
  [^MonadCommandConfig command-config m & [state]]
  (let [res (run-monadic command-config m state)
        [r _state] res]
    (if (exception-value? r)
      (throw (exception-value-exception r))
      res)))

(defn reify-as
  "Adds `reification` meta data to `m` that helps utilities (like the
  mock runner for tests), to reify a composed command as something
  comparable (and printable), for cases where the command itself is
  not; e.g. it includes a `bind` inside. See [[`reify-command`]] to
  extract the meta data again.

  Note that `reify-as` does not work on `bind`s like for example composed
  monadic programs with `monadic`.  Because the reification meta data sticks at
  the first `bind` that is handled by `run-monadic` and friends above -- and not
  by the command runners that are mocked away by the mock runner.  To improve on
  that we might either need to add the awareness of mocking into `run-monadic`
  and friends above or copy their implementation into to `mock` namespace and
  include mocking of binds there."
  [m reification]
  (assert reification) ;; should not be falsy, but only to simplify reify-command
  (vary-meta m
             assoc
             ::reify-as reification))

(defn reified?
  "Checks whether a monadic command is reified."
  [m]
  (contains? (meta m) ::reify-as))

(defn reify-command
  "Return the reification object of `m`, if one was added
  with [[`reify-as`]], or m itself otherwise."
  [m]
  (or (::reify-as (meta m))
      m))
