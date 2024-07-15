(ns hooks.sum-type
  (:require [clj-kondo.hooks-api :as api]))

(defn define-sum-type
  [expr]
  (update expr :node
          (fn [node]
            (let [[sum-type-name predicate] (rest (:children node))]
              (api/list-node
               (list (api/token-node 'do)
                     (api/list-node [(api/token-node 'declare) sum-type-name])
                     (api/list-node [(api/token-node 'declare) predicate])))))))

