(ns hooks.condition
  (:require [clj-kondo.hooks-api :as api]))

(defn define-condition-type
  [{:keys [:node]}]
  (let [[_ condition _super ctor pred & [field-specs]] (:children node)
        accessors (map second (partition 2 (:children field-specs)))
        new-node (api/list-node
                  (list* (api/token-node 'do)
                         (api/list-node [(api/token-node 'declare) condition])
                         (api/list-node [(api/token-node 'declare) pred])
                         (api/list-node [(api/token-node 'declare) ctor])
                         (map (fn [t] (api/list-node [(api/token-node 'declare) t]))
                              accessors)))]
    {:node new-node}))

(defn guard
  [{:keys [:node]}]
  (let [[_ condition+clauses & body] (:children node)
        [condition & clauses] (:children condition+clauses)]
    {:node
     (api/list-node
      (list*
       (api/token-node 'do)
       (api/list-node
        [(api/token-node 'let)
         (api/vector-node [condition (api/token-node 'nil)])
         (api/list-node
          (list*
           (api/token-node 'do)
           clauses))])
       body))}))
