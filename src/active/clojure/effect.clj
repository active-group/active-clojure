(ns active.clojure.effect
  "Effects based on dynamic vars. Effects can be used to have better
  control over side effects, to abstract over different possible
  interpretations of an aspect of a program or to make things easier
  testable.

  Main usage patterns:

  ```
  (declare-effect eff [a])

  (defn foo []
    (assert (= 4 (eff 2))))
  
  (defn square [a] (* a a))

  (foo) ;; => throws exception
  
  (with-effects [eff square]
    (foo))

  ((bind-effects* {#'eff square} foo))

  ```
  "
  (:refer-clojure :rename {bound-fn* clj-bound-fn*}))

(defn not-implemented [effect]
  (ex-info "Effect not implemented." {:effect effect :type ::not-implemented}))

(defmacro declare-effect
  "Declares `name` as an abstract effect, to be bound to an
  implementation later via [[with-effects]]. `params` and `docstring`
  are for documentation purposes."
  ([name params]
   `(declare-effect ~name nil ~params))
  ([name docstring params]
   `(do (defn ~name [~@params] (throw (not-implemented ~name)))
        (alter-meta! (var ~name) assoc
                     :dynamic true
                     ::effect true
                     :docstring ~docstring)
        ;; Note: adding :dynamic meta data is not enough in clojure :-/ need to call clojure.lang.Var/setDynamic.
        (.setDynamic (var ~name))
        (var ~name))))

(defn effect-var? [v]
  (and (var? v)
       (::effect (meta v))))

(defmacro with-effects
  "Binds effects to implementations during the evaluation of `body`.

  ```
  (declare-effect add-user! [user])
  
  (with-effects [add-user! db-add-user!]
    ...)
  ```
  "
  [bindings & body]
  `(binding ~bindings
     ~@body))

(defn with-effects*
  "Calls `(thunk)` and binds effects to implementation via a map of
  effect vars during the evaluation of `(thunk)`."
  [binding-map thunk]
  (assert (every? effect-var? (keys binding-map)))
  (with-bindings* binding-map thunk))

(defn merge-effects
  "Like merge, but asserts that all keys are effect vars, and the same
  vars are not bound to different implementations."
  [binding-map & more]
  (assert (every? #(every? effect-var? (keys %)) (cons binding-map more)))
  (apply merge-with (fn [v1 v2]
                      (assert (= v1 v2) (str "Conflicting effect implementations: " v1 v2))
                      v2)
         (cons binding-map more)))

(defn bound-fn*
  "Returns a function that will call `f` with the same effect
  implementations in place as there are now. Passes all arguments though to f."
  [f]
  (clj-bound-fn* f))

(defn bind-effects*
  "Returns a function that will call `f` with the given map of effect
  implementations in place. Note that the returned function can then
  be called on other threads, too."
  [binding-map f]
  (with-effects* binding-map
    (fn []
      (bound-fn* f))))
