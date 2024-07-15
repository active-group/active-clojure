(ns hooks.record
  (:require [clj-kondo.hooks-api :as api]))

(defn define-record-type
  [{:keys [:node]}]
  (let [[record-name & more] (rest (:children node))
        [constructor-spec predicate field-specs] (if (api/map-node? (first more))
                                                   ;; remove options
                                                   (rest more)
                                                   more)
        [constructor & _fields] (if-let [ch (:children constructor-spec)]
                                  ch
                                  [constructor-spec])
        accessors (map second (partition 2 (:children field-specs)))
        new-node
        (api/list-node
         (list* (api/token-node 'do)
                (api/list-node [(api/token-node 'declare) record-name])
                (api/list-node [(api/token-node 'declare) predicate])
                (api/list-node [(api/token-node 'declare) constructor])
                (map (fn [t] (api/list-node [(api/token-node 'declare) t]))
                     (if-let [projection-lens (and (api/map-node? (first more))
                                                   (:projection-lens (api/sexpr (first more))))]
                       (conj accessors (api/token-node projection-lens))
                       accessors))))]
  {:node new-node}))

(defn define-singleton-type
  [expr]
  (update expr :node
          (fn [node]
            (let [[record-name singleton predicate] (rest (:children node))]
              (api/list-node
               (list (api/token-node 'do)
                     (api/list-node [(api/token-node 'declare) record-name])
                     (api/list-node [(api/token-node 'declare) singleton])
                     (api/list-node [(api/token-node 'declare) predicate])))))))
