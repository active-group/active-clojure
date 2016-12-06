(ns ^{:doc "Monad related functionality, particularly free monads."}
  active.clojure.monad
  #?(:cljs (:require-macros [active.clojure.record :refer (define-record-type)]
                            [active.clojure.monad :refer (monadic)]))
  (:require #?(:clj [active.clojure.record :refer :all])
            #?(:cljs active.clojure.record)
            #?(:clj [clojure.core :as core])
            #?(:cljs [cljs.core :as core])
            [active.clojure.condition :as c]))

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
   ;; normalize nested binds, keeping original meta (source, line)
   (free-bind? mv) (with-meta
                      (Bind. (free-bind-monad mv)
                          (fn [x]
                            (let [next ((free-bind-cont mv) x)] ; yields a monad
                              (free-bind next f))))
                      (meta mv))

   :else (with-meta (make-free-bind mv f)
           (meta mv))))

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
  `(monadic-1 ~(meta &form) ~@?stmts)))

#?(:clj
(defmacro monadic-1
  [?meta & ?stmts]
  (if (empty? ?stmts)
    (throw (IllegalArgumentException. (str "there must be at least one statement in " *ns* " " ?meta)))

    (let [?stmt (first ?stmts)
          check-bindings (fn [bindings]
                           (when-not (vector? bindings)
                             (throw (IllegalArgumentException. (str "bindings must be an vector in "
                                                                    *ns* " " ?meta))))
                           (when (empty? bindings)
                             (throw (IllegalArgumentException. (str "bindings must be non-empty in "
                                                                    *ns* " " ?meta))))
                           (when-not (even? (count bindings))
                             (throw (IllegalArgumentException. (str "bindings must be even-sized vector in "
                                                                    *ns* " " ?meta)))))]

      (cond
       (vector? ?stmt)
       (do
         (check-bindings ?stmt)
         (letfn [(recurse [?pairs]
                   (let [[?pat ?rhs] (first ?pairs)
                         ?rest (rest ?pairs)]
                     `(with-meta (free-bind ~?rhs
                                            (fn [~?pat]
                                              ~(if (empty? ?rest)
                                                 `(monadic-1 ~?meta ~@(rest ?stmts))
                                                 (recurse ?rest))))
                        '~(assoc ?meta :statement [?pat ?rhs]))))]
           (recurse (partition 2 ?stmt))))

       (and (list? ?stmt)
            (= 'let (first ?stmt)))
       (do (when-not (= 2 (count ?stmt))
             (throw (IllegalArgumentException. (str "let statement must have exactly one subform in "
                                                    *ns* " " ?meta))))
           (check-bindings (second ?stmt))
           `(let ~(second ?stmt)
              (monadic-1 ~?meta ~@(rest ?stmts))))

       (empty? (rest ?stmts))
       `(vary-meta ~(first ?stmts)
          ~'assoc :statement '~(first ?stmts))

       :else
       `(with-meta (free-bind ~?stmt (fn [_#] (monadic-1 ~?meta ~@(rest ?stmts))))
          '~(assoc ?meta :statement ?stmt)))))))

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
  ^{doc "Configuration for supporting a set of monad commands."}
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
  [run-any env state m]
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
                                                   (str "unknown monad command in bind, line " line ", column " column)
                                                   bind m
                                                   statement)
                            (c/assertion-violation `run-free-reader-state-exception 
                                                   "unknown monad command in bind"
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
               [nil state]

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
        [r state] res]
    (if (exception-value? r)
      (throw (exception-value-exception r)))
      res))

(defn reify-as
  "Adds `reification` meta data to `m` that helps utilities (like the
  mock runner for tests), to reify a composed command as something
  comparable (and printable), for cases where the command itself is
  not; e.g. it includes a `bind` inside. See [[`reify-command`]] to
  extract the meta data again."
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
