(ns hooks.monad
  (:require [clj-kondo.hooks-api :as api]))

(defn monadic
  [{:keys [:node]}]
  (letfn [(rewrite-monadic-form
            [forms]
            (if (empty? forms)
              forms
              (let [form (first forms)
                    forms (rest forms)]
                (cond
                  (= :vector (:tag form))
                  (api/list-node
                   (list (api/token-node 'let)
                         form
                         (rewrite-monadic-form forms)))
                  (and (= :list (:tag form))
                       (= 2 (count (:children form)))
                       (= "let" (:string-value (first (:children form)))))
                  (api/list-node
                    (list (first (:children form))
                          (second (:children form))
                          (rewrite-monadic-form forms)))
                  :else
                  (if (empty? forms)
                    form
                    (api/list-node
                      (list (api/token-node 'do)
                            form
                            (rewrite-monadic-form forms))))))))]
    (let [[& forms] (rest (:children node))]
      (if (empty? forms)
        (api/reg-finding! (assoc (meta node)
                                 :message "monadic must not be empty"
                                 :type :monad/empty))
        (let [new-node (rewrite-monadic-form forms)]
          {:node new-node})))))
