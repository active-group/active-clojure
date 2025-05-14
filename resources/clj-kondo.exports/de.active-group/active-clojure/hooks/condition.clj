(ns hooks.condition
  (:require [clj-kondo.hooks-api :as api]))

(defn define-condition-type
  [{:keys [:node]}]
  (let [[condition _super ctor pred & [field-specs]] (rest (:children node))
        accessors (map second (partition 2 (:children field-specs)))
        new-node (api/list-node
                  (list* (api/token-node 'do)
                         (api/list-node [(api/token-node 'declare) condition])
                         (api/list-node [(api/token-node 'declare) pred])
                         (api/list-node [(api/token-node 'declare) ctor])
                         (map (fn [t] (api/list-node [(api/token-node 'declare) t]))
                              accessors)))]
    {:node new-node}))
