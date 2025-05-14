(ns hooks.dynj
  (:require [clj-kondo.hooks-api :as api]))

(defn declare-dynj
  [{:keys [node]}]
  (let [[name _second & _more] (rest (:children node))
        new-node (api/list-node
                  (list (api/token-node 'do)
                        (api/list-node [(api/token-node 'declare) name])))]
    {:node new-node}))
