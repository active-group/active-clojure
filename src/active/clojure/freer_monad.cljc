(ns active.clojure.freer-monad
  "Freer monads.

  This is a variant of `active.clojure.monad` with a more efficient
  representation for bind.
  
  See http://okmij.org/ftp/Haskell/extensible/index.html for details."
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [active.clojure.condition :as c]
            #?(:clj [active.clojure.record :refer [define-record-type]])
            #?(:cljs [active.clojure.cljs.record :refer-macros [define-record-type]])
            #?(:clj [clojure.core :as core])
            #?(:cljs [cljs.core :as core])
            )
  #?(:cljs (:require-macros [active.clojure.freer-monad :refer [monadic]])))

(define-record-type ^:no-doc Return
  (return result)
  return?
  [result return-result])

(define-record-type Bind
  (make-bind first-action continuations)
  bind?
  [first-action  bind-first-action
   ;; queue, as per below
   continuations bind-continuations])

;; A queue is a queue node or something else - in which case it's a 
;; singleton queue with that element.

(define-record-type QueueNode
  (make-queue-node left right)
  queue-node?
  [left  queue-node-left 
   right queue-node-right])
  
(defn- queue-snoc
  [queue last]
  (make-queue-node queue last))

; identical, for clarity
(defn- queue-append
  [queue-1 queue-2]
  (make-queue-node queue-1 queue-2))

(defmacro queue-decompose-m
  [queue
   singleton-name singleton-code
   first-name rest-name queue-code]
  `(let [queue# ~queue]
     (if (queue-node? queue#)
       (loop [left# (queue-node-left queue#)
              right# (queue-node-right queue#)]
         (if (queue-node? left#)
           (recur (queue-node-left left#)
                  (make-queue-node (queue-node-right left#) right#))
           (let [~first-name left#
                 ~rest-name right#]
             ~queue-code)))
       (let [~singleton-name queue#]
         ~singleton-code))))

;; The crimes we commit in the name of performance

(defprotocol IDecomposedQueue
  (set-decomposed [this first rest])
  (get-first-continuation [this])
  (get-rest-continuations [this]))

(deftype DecomposedQueue [^:unsynchronized-mutable first-continuation
                          ^:unsynchronized-mutable rest-continuations]
  IDecomposedQueue
  (set-decomposed
    [this first rest]
    (set! first-continuation first)
    (set! rest-continuations rest))
  (get-first-continuation [this] first-continuation)
  (get-rest-continuations [this] rest-continuations))

(defn queue-decompose
  [queue ^DecomposedQueue decomposed-queue]
  (if (queue-node? queue)
    (loop [left (queue-node-left queue)
           right (queue-node-right queue)]
      (if (queue-node? left)
        (recur (queue-node-left left)
               (make-queue-node (queue-node-right left) right))
        (do
          (set-decomposed decomposed-queue left right)
          nil)))
    queue))

(defnp bind
  "Bind/flatMap for the free monad."
  [first-action continuation]
  (cond
    (return? first-action) (continuation (return-result first-action))
    
    (bind? first-action)
    (make-bind (bind-first-action first-action)
               (queue-snoc (bind-continuations first-action) continuation))

    :else
    (make-bind first-action continuation)))

(defn throw-illegal-argument-exception
  [msg]
  (c/assertion-violation `monadic (str "Illegal argument: " msg)))

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
       `(with-meta (bind ~?rhs
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
              (bind (first ms)
                    (fn [v]
                      (f (rest ms)
                         (cons v res))))
              (return (reverse res))))]
    (f ms '())))

(defn sequ_
  "Evaluate each action in the sequence from left to right, and ignore the results."
  [ms]
  (reduce (fn [q p]
            (bind p (fn [_] q)))
          (return nil)
          (reverse ms)))

(define-record-type ^:no-doc Signal
  (signal exception)
  signal?
  [exception signal-exception])

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
  (bind (with-handler (fn [e] (bind final-m (fn [_] (signal e))))
               m)
             (fn [r] (bind final-m (fn [_] (return r))))))

(defn bind-except
  "Evaluate m, and if an exception occurs continue with (handler exception), otherwise with (f value-of-m).
   Note that neither the handler result is to f, nor exceptions are caught in f."
  [m handler f]
  (bind (with-handler
          (fn [e] (bind (handler e)
                        (fn [res] (return [:failed res]))))
          (bind m (fn [value] (return [:ok value]))))
        (fn [x]
          (case (first x)
            :ok (f (second x))
            :failed (return (second x))))))

(define-record-type ^:no-doc ExceptionValue
  (make-exception-value exception)
  exception-value?
  [exception exception-value-exception])

(defn apply-continuations
  "Apply continuations to the result of the first action."
  ([continuations first-result]
   (apply-continuations continuations first-result (DecomposedQueue. nil nil)))
  ([continuations first-result ^DecomposedQueue decomposed-queue]
   (if-let [continuation (queue-decompose continuations decomposed-queue)]
     (continuation first-result)
     (let [first-continuation (get-first-continuation decomposed-queue)
           rest-continuations (get-rest-continuations decomposed-queue)
           action (first-continuation first-result)]
       (cond
         (return? action) (recur rest-continuations (return-result action) decomposed-queue)

         (bind? action)
         (make-bind (bind-first-action action)
                    (queue-append (bind-continuations action)
                                  rest-continuations))

         :else
         (make-bind action rest-continuations))))))

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
  (replace-cont v continuations)
  replace-cont?
  [v replace-cont-v
   ;; this is the continuations queue from bind
   continuations replace-cont-continuations])

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

(defn run-freer-reader-state-exception
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
                            (c/assertion-violation `run-freer-reader-state-exception
                                                   (str "unknown monad command in bind, line " line ", column " column)
                                                   bind m
                                                   statement)
                            (c/assertion-violation `run-freer-reader-state-exception
                                                   "unknown monad command in bind"
                                                   bind m)))
        decomposed-queue (DecomposedQueue. nil nil)] ; reuse

    (letfn [(run [env state m]
              (cond
               (return? m) [(return-result m) state]
               (signal? m) [(make-exception-value (signal-exception m)) state]

               (bind? m)
               (let [m1 (bind-first-action m)
                     conts (bind-continuations m)]
                 (cond
                   (signal? m1) [(make-exception-value (signal-exception m1)) state]

                   (bind? m1) (unknown-command m m1)

                   (with-handler? m1)
                   (let [[x state] (run env state (with-handler-body m1))]
                     (if (exception-value? x)
                       (let [res (run env state ((with-handler-handler m1) (exception-value-exception x)))
                             [x state] res]
                         (if (exception-value? x)
                           res
                           (recur env state (apply-continuations conts x decomposed-queue))))
                       (recur env state (apply-continuations conts x decomposed-queue))))

                   (get-env? m1)
                   (recur env state (apply-continuations conts
                                                         env
                                                         decomposed-queue))
                 
                   (with-env? m1)
                   (let [res (run ((with-env-trans m1) env) state (with-env-body m1))
                         [x state] res]
                     (if (exception-value? x)
                       res
                       (recur env state (apply-continuations conts x decomposed-queue))))

                   (get-state? m1)
                   (recur env state (apply-continuations conts state decomposed-queue))

                   (put-state!? m1)
                   (recur env (put-state-state m1) (apply-continuations conts nil decomposed-queue))

                   :else
                   (let [res (run-command run env state m1)]
                     (if (unknown-command? res)
                       (unknown-command m m1)
                       (let [[x state] res]
                         (if (exception-value? x)
                           res
                           (recur env state (apply-continuations conts x decomposed-queue))))))))

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

(defn execute-freer-reader-state-exception
  "Run monadic computation in a reader-state-exception monad, turning exceptions
  into Clojure exceptions.

  - `command-config` is the configuration object for running commands
  - `m` is the computation to run
  - `state` is an optional initial state (from a previous run)

  Returns [result state].
  "
  [^MonadCommandConfig command-config m & [state]]
  (let [res (run-freer-reader-state-exception command-config m state)
        [r state] res]
    (if (exception-value? r)
      (throw (exception-value-exception r)))
    res))

(defn run-monadic-swiss-army
  "Run a monadic computation in an almighty monad.  Same as
  [[`run-freer-reader-state-exception`]] with the addition of `call-cc`.

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
                            (c/assertion-violation `run-freer-reader-state-exception
                                                   (str "unknown monad command in bind, line " line ", column " column)
                                                   bind m
                                                   statement)
                            (c/assertion-violation `run-freer-reader-state-exception
                                                   "unknown monad command in bind"
                                                   bind m)))
        decomposed-queue (DecomposedQueue. nil nil)] ; reuse

    (letfn [(run [env state m]
              (cond
               (return? m) [(return-result m) state]
               (signal? m) [(make-exception-value (signal-exception m)) state]
               (call-cc? m) (recur env state ((call-cc-f m) (fn [v] (replace-cont v return))))
               (replace-cont? m) (recur env state (apply-continuations (replace-cont-continuations m) (replace-cont-v m)
                                                                       decomposed-queue))

               (bind? m)
               (let [m1 (bind-first-action m)
                     conts (bind-continuations m)]
                 (cond
                   (signal? m1) [(make-exception-value (signal-exception m1)) state]

                   (call-cc? m1) (recur env state ((call-cc-f m1) (fn [v] (replace-cont v conts))))
                   (replace-cont? m1) (recur env state (apply-continuations (replace-cont-continuations m1) (replace-cont-v m1)
                                                                            decomposed-queue))

                   (bind? m1) (unknown-command m m1)

                   (with-handler? m1)
                   (let [[x state] (run env state (with-handler-body m1))]
                     (if (exception-value? x)
                       (let [res (run env state ((with-handler-handler m1) (exception-value-exception x)))
                             [x state] res]
                         (if (exception-value? x)
                           res
                           (recur env state (apply-continuations conts x decomposed-queue))))
                       (recur env state (apply-continuations conts x decomposed-queue))))

                   (get-env? m1)
                   (recur env state (apply-continuations conts
                                                         env
                                                         decomposed-queue))
                 
                   (with-env? m1)
                   (let [res (run ((with-env-trans m1) env) state (with-env-body m1))
                         [x state] res]
                     (if (exception-value? x)
                       res
                       (recur env state (apply-continuations conts x decomposed-queue))))

                   (get-state? m1)
                   (recur env state (apply-continuations conts state decomposed-queue))

                   (put-state!? m1)
                   (recur env (put-state-state m1) (apply-continuations conts nil decomposed-queue))

                   :else
                   (let [res (run-command run env state m1)]
                     (if (unknown-command? res)
                       (unknown-command m m1)
                       (let [[x state] res]
                         (if (exception-value? x)
                           res
                           (recur env state (apply-continuations conts x decomposed-queue))))))))

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

(defn execute-monadic-swiss-army
  "Run monadic computation in an almighty monad, turning exceptions
  into Clojure exceptions.  See [[`run-monadic-swiss-army]].

  - `command-config` is the configuration object for running commands
  - `m` is the computation to run
  - `state` is an optional initial state (from a previous run)

  Returns [result state].
  "
  [^MonadCommandConfig command-config m & [state]]
  (let [res (run-monadic-swiss-army command-config m state)
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
