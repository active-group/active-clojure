(ns active.clojure.dynj
  "Thin layer over dynamic vars for implicit dependency injection. Dynjs
  can be used to have better control over side effects, to abstract
  over different possible interpretations of an aspect of a program or
  to make things easier for testing.

  Main usage patterns:

  ```
  (declare-dynj eff [a])

  (defn foo []
    (assert (= 4 (eff 2))))
  
  (defn square [a] (* a a))

  (foo) ;; => throws exception
  
  (binding [eff square]
    (foo))

  ((with-bindings* {#'eff square} foo))

  ```
  "
  (:refer-clojure :rename {bound-fn* clj-bound-fn*
                           binding clj-binding
                           with-bindings* clj-with-bindings*}))

(defn- dynj-name [dynj]
  #_(symbol (name (ns-name (:ns (meta dynj))))
            (name (:name (meta dynj))))
  ;; TODO: nicer
  (str dynj))

(defn ^:no-doc not-implemented [dynj]
  (ex-info (str "Dynj var " (dynj-name dynj) " not implemented.") {:dynj dynj :type ::not-implemented}))

(defmacro defn-dynj
  "Declares `name` as a dynamic injection point, to be bound to an
  implementation/value later via [[binding]], and adds a default
  implementation. Typically you would throw a helpful exception in the
  body."
  [name params & body]
  (let [[docstring params body]
        (if (string? params)
          [params (first body) (rest body)]
          [nil params body])]
    
    `(do (defn ~name [~@params] ~body)
         (alter-meta! (var ~name) assoc
                      :dynamic true
                      ::dynj true
                      :docstring ~docstring)
         ;; Note: adding :dynamic meta data is not enough in clojure :-/ need to call clojure.lang.Var/setDynamic.
         (.setDynamic (var ~name))
         (var ~name))))

(defmacro declare-dynj
  "Declares `name` as a dynamic injection point, to be bound to an
  implementation/value later via [[binding]]. `params` and `docstring`
  are for documentation purposes."
  ([name params]
   `(defn-dynj ~name ~params
      (throw (not-implemented ~name))))
  ([name docstring params]
   `(defn-dynj ~name ~docstring ~params
      (throw (not-implemented ~name)))))

(defn- dynj-var? [v]
  (and (var? v)
       (contains? (meta v) ::dynj)))

(defmacro binding
  "Binds one or more dynjs to implementations during the evaluation of `body`.

  ```
  (declare-dynj add-user! [user])
  
  (binding [add-user! db-add-user!]
    ...)
  ```
  "
  [bindings & body]
  `(clj-binding ~bindings
     ~@body))

(defn with-bindings*
  "Calls `(thunk)` and binds implementations via a map of
  dynj vars during the evaluation of `(thunk)`."
  [binding-map thunk]
  (assert (every? dynj-var? (keys binding-map)))
  (clj-with-bindings* binding-map thunk))

(defn merge-dynjs
  "Like merge, but asserts that all keys are dynj vars, and the same
  vars are not bound to different implementations."
  [binding-map & more]
  (assert (every? #(every? dynj-var? (keys %)) (cons binding-map more)))
  (apply merge-with (fn [v1 v2]
                      (assert (= v1 v2) (str "Conflicting dynj implementations: " v1 v2))
                      v2)
         (cons binding-map more)))

(defn bound-fn*
  "Returns a function that will call `f` with the same dynj
  implementations in place as there are now. Passes all arguments though to f."
  [f]
  (clj-bound-fn* f))

(defn bind-fn*
  "Returns a function that will call `f` with the given map of dynj
  implementations in place. Note that the returned function can then
  be called on other threads, too."
  [binding-map f]
  (with-bindings* binding-map
    (fn []
      (bound-fn* f))))
