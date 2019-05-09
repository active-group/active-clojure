(ns active.clojure.record-helper
  (:require [active.clojure.lens :as lens]
            [active.clojure.condition :as c]))

;;; Nongenerative stuff
;; Maps from nongenerative-id to {:ns *ns* :form define-record-form}
#?(:clj
   (defonce global-record-type-registry (atom {})))

#?(:clj
   (defn remove-record-type
     [nongenerative-id]
     (swap! global-record-type-registry
            (fn [old-reg] (dissoc old-reg nongenerative-id)))))


#?(:clj
   (defn throw-illegal-argument-exception
     [msg]
     (c/assertion-violation `throw-illegal-argument-exception "Illegal argument" msg)))


#?(:clj
   (defn prepare-arguments!
     "Checks validity of arguments and prepares them for `define-record-type` call.
  Returns vector of arguments:
  [type options constructor constructor-args predicate field-triples opt+specs].

  If :nongenerative option is truthy, the given nongenerative-id is registered in
  the global-record-type-registry. If this id already exists, but the definitions
  are different, an error is thrown. Otherwise `nil` is returned."
     [form ns ?type ?second ?params]
     (let [?options          (when (map? ?second) ?second)
           ?constructor-call (if ?options (first ?params) ?second)
           ?predicate        (if ?options (second ?params) (first ?params))
           ?field-specs      (if ?options (nth ?params 2) (second ?params))
           ?opt+specs        (if ?options (drop 3 ?params) (drop 2 ?params))]
       (when-not (or (and (list? ?constructor-call)
                          (not (empty? ?constructor-call)))
                     (symbol? ?constructor-call))
         (throw (throw-illegal-argument-exception (str "constructor call must be a list in " ns " " (meta form)))))
       (when-not (vector? ?field-specs)
         (throw (throw-illegal-argument-exception (str "field specs must be a vector in " ns " " (meta form)))))
       (when-not (even? (count (remove seq? ?field-specs)))
         (throw (throw-illegal-argument-exception (str "odd number of elements in field specs in " ns " " (meta form)))))
       (when-not (every? true? (map #(= 3 (count %1)) (filter seq? ?field-specs)))
         (throw (throw-illegal-argument-exception (str "wrong number of elements in field specs with lens in " ns " " (meta form)))))
       (let [?field-triples (loop [specs (seq ?field-specs)
                                   triples '()]
                              (if (empty? specs)
                                (reverse triples)
                                (let [spec (first specs)]
                                  (cond
                                    (list? spec)
                                    (do
                                      (when-not (and (= 3 (count spec))
                                                     (every? symbol spec))
                                        (throw-illegal-argument-exception (str "invalid field spec " spec " in " ns " " (meta form))))
                                      (recur (rest specs) (list* spec triples)))

                                    (symbol? spec)
                                    (do
                                      (when (empty? (rest specs))
                                        (throw (throw-illegal-argument-exception (str "incomplete field spec for " spec " in " ns " " (meta form)))))
                                      (when-not (symbol? (fnext specs))
                                        (throw (throw-illegal-argument-exception (str "invalid accessor " (fnext specs) " for " spec " in " ns " " (meta form)))))
                                      (recur (nnext specs)
                                             (list* [spec (fnext specs) nil] triples)))

                                    :else
                                    (throw (throw-illegal-argument-exception (str "invalid field spec " spec " in " ns " " (meta form))))))))

             [?constructor & ?constructor-args] (cond
                                                  (list? ?constructor-call)
                                                  ?constructor-call

                                                  (symbol? ?constructor-call)
                                                  (concat [?constructor-call]
                                                          (map first ?field-triples)))
             ;; Rename for nongenerative test
             new-ns ns
             new-form form]
       ;;; Check nongenerative option
         (if-let [non-g-id (:nongenerative ?options)]
           ;; nongenerative
           (if-let [{:keys [ns form]} (get @global-record-type-registry non-g-id)]
             (if (and (= ns new-ns) (= form new-form))
               ;; non-g-id exists, but definitions are the same
               nil
               (throw (Exception. "This record type definition already exists with different arguments.")))
             ;; nongenerative, but id doesn't exist. Register id and return arguments.
             (let [non-g-id (if (= true non-g-id) (str new-ns "/" ?type) non-g-id)] ; default non-g-id when key is `true`
               (swap! global-record-type-registry
                      (fn [old-reg] (assoc old-reg non-g-id {:ns new-ns :form new-form})))
               [?type ?options ?constructor ?constructor-args ?predicate ?field-triples ?opt+specs]))
           ;; generative, just return arguments.
           [?type ?options ?constructor ?constructor-args ?predicate ?field-triples ?opt+specs])))
     ))
