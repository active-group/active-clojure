(ns hooks.match
  (:require [clj-kondo.hooks-api :as api]))

(defn- lhs-match-expr [e]
  ;; (:or ...), (:compare-fn ...),
  ;; or a value

  (cond
    (and (api/list-node? e)
         (= :or (:k (first (:children e)))))
    (api/vector-node (rest (:children e)))

    (and (api/list-node? e)
         (= :compare-fn (:k (first (:children e)))))
    (api/vector-node (rest (:children e)))

    :else
    e))

(defn- obscure-value [node]
  ;; we generate some 'fake values' and expressions here; to prevent clj-kondo complaining about some, e.g. '(zero? nil)',
  ;; we obscure the actual value with this construct
  (api/list-node (list (api/token-node 'deref)
                       (api/list-node (list (api/token-node 'atom)
                                            node)))))

(defn- match-lhs-binding [form]
  ;; (`path ~match :as `name) => [binding match]
  ;; (? `path :as `name)
  ;; (:as `name)
  (let [as? (fn [e]
              (and (api/keyword-node? e)
                   (= :as (:k e))))
        [match-expr binding]
        (let [forms (:children form)
              ;; drop optional indicator
              forms (if (and (api/token-node? (first forms))
                             (= '? (:value (first forms))))
                      (drop 1 forms)
                      forms)
              ;; drop path (could check it for syntax only)
              forms
              (if (as? (first forms))
                forms ;; no path
                (rest forms))

              ;; take match expr
              [match-expr forms]
              (if (as? (first forms))
                [nil forms] ;; no match expr
                [(first forms) (rest forms)])

              ;; take binding
              binding
              (if (as? (first forms))
                (second forms)
                nil)]
          [match-expr binding])]

    [(or binding (api/token-node '_))
     ;; Note: not what's actually bound
     (obscure-value (or (lhs-match-expr match-expr) (api/token-node 'nil)))]))

(defn- match-lhs-bindings [lhs]
  (if (api/vector-node? lhs)
    (mapcat match-lhs-binding (:children lhs))
    ;; can also be a name, referring to a defpattern; bind nothing in that case (we can't evaluate it here)
    [(api/token-node '_) lhs]))

(defn- match-rhs [form]
  form)

(defn- match-1 [lhs rhs]
  (api/list-node
   (list (api/token-node 'let)
         (api/vector-node (match-lhs-bindings lhs))
         (match-rhs rhs))))

(defn- match-forms [forms]
  (let [lhs-rhs (partition 2 forms)]
    (api/list-node
     (list* (api/token-node 'do)
            (map (fn [[lhs rhs]]
                   (match-1 lhs rhs))
                 lhs-rhs)))))

(defn matcher [{node :node}]
  (let [[& forms] (rest (:children node))]
    {:node (match-forms forms)}))

(def map-matcher matcher)

(defn match [{node :node}]
  (let [[value & forms] (rest (:children node))]
    {:node (api/list-node
            (list (api/token-node 'do)
                  value
                  (api/list-node (list* (api/token-node 'active.clojure.match/matcher) forms))))}))

(defn defpattern
  [{node :node}]
  ;; Note: the actual semantics are impossible to define here; it's very unhygenic.
  (let [declare* (fn [sym-node]
                   (api/list-node (list (api/token-node 'declare)
                                        sym-node)))
        def* (fn [sym-node v-node]
               (api/list-node (list (api/token-node 'def)
                                    sym-node
                                    v-node)))
        [pattern-name pattern-vector] (rest (:children node))]
    {:node (api/list-node (list* (api/token-node 'do)
                                 (declare* pattern-name)
                                 (map (fn [[b v]]
                                        (if (= '_ (:value b))
                                          (declare* b)
                                          (def* b v)))
                                      (partition 2 (match-lhs-bindings pattern-vector)))))}))

