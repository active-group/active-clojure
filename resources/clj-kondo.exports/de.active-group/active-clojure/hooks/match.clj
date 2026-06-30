(ns hooks.match
  (:require [clj-kondo.hooks-api :as api]))

(defn defpattern
  [{:keys [:node]}]
  (let [[& forms] (rest (:children node))
        [pattern-name pattern-vector] forms
        patterns (:children pattern-vector)
        patterns-with-bindings (filter #(= :as (api/sexpr (second %))) (map #(reverse (:children %)) patterns))
        pattern-bindings (map first patterns-with-bindings)
        new-node (api/list-node
                   (list* (api/token-node 'do)
                          (api/list-node [(api/token-node 'declare) pattern-name])
                          (mapv #(api/list-node [(api/token-node 'declare) %]) pattern-bindings)))]
    {:node new-node}))
