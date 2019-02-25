(ns active.clojure.record
  #?@
  (:clj
   [(:require
     [active.clojure.lens :as lens]
     [active.clojure.condition :as c]
     [active.clojure.macro :refer [if-cljs cljs-env?]]
     [clojure.spec.alpha :as spec]
     [active.clojure.record-clj-internals :refer [emit-java-record-definition]]
     [active.clojure.record-cljs-internals :refer [emit-javascript-record-definition]]
     )
    (:import clojure.lang.IPersistentMap)
    (:import clojure.lang.RT)
    (:import java.lang.IllegalArgumentException)
    ]
   :cljs
   [(:require
     [active.clojure.lens :as lens]
     [active.clojure.condition :as c]

     [active.clojure.record-cljs-internals :refer [emit-javascript-record-definition]]
     [cljs.spec.alpha :as s :include-macros true])
    (:require-macros [active.clojure.macro :refer [if-cljs]])]))


;;;; Nongenerative stuff
;;; Maps from type-uuid to define-record-type-form
(defonce global-record-type-registry (atom {}))

(defn remove-record-type
  [non-generative-id]
  (swap! global-record-type-registry
         (fn [old-reg] (dissoc old-reg non-generative-id))))


;;;; Helper function
(defn throw-illegal-argument-exception
  [msg]
  (c/assertion-violation `throw-illegal-argument-exception "Illegal argument" msg))

(defn emit-record-definition
  [env type options constructor constructor-args predicate field-triples opt+specs]
  (if (cljs-env? env)
    (emit-javascript-record-definition env type options constructor constructor-args predicate field-triples opt+specs)
    (emit-java-record-definition type options constructor constructor-args predicate field-triples opt+specs)))

;;;; record type definition
#?(:clj
   (defmacro define-record-type
     "Attach doc properties to the type and the field names to get reasonable docstrings."
     [?type ?second & ?params]
     (let [?options          (when (map? ?second) ?second)
           ?constructor-call (if ?options (first ?params) ?second)
           ?predicate        (if ?options (second ?params) (first ?params))
           ?field-specs      (if ?options (nth ?params 2) (second ?params))
           ?opt+specs        (if ?options (drop 3 ?params) (drop 2 ?params))]
       (when-not (or (and (list? ?constructor-call)
                          (not (empty? ?constructor-call)))
                     (symbol? ?constructor-call))
         (throw (throw-illegal-argument-exception (str "constructor call must be a list in " *ns* " " (meta &form)))))
       (when-not (vector? ?field-specs)
         (throw (throw-illegal-argument-exception (str "field specs must be a vector in " *ns* " " (meta &form)))))
       (when-not (even? (count (remove seq? ?field-specs)))
         (throw (throw-illegal-argument-exception (str "odd number of elements in field specs in " *ns* " " (meta &form)))))
       (when-not (every? true? (map #(= 3 (count %1)) (filter seq? ?field-specs)))
         (throw (throw-illegal-argument-exception (str "wrong number of elements in field specs with lens in " *ns* " " (meta &form)))))
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
                                        (throw-illegal-argument-exception (str "invalid field spec " spec " in " *ns* " " (meta &form))))
                                      (recur (rest specs) (list* spec triples)))

                                    (symbol? spec)
                                    (do
                                      (when (empty? (rest specs))
                                        (throw (throw-illegal-argument-exception (str "incomplete field spec for " spec " in " *ns* " " (meta &form)))))
                                      (when-not (symbol? (fnext specs))
                                        (throw (throw-illegal-argument-exception (str "invalid accessor " (fnext specs) " for " spec " in " *ns* " " (meta &form)))))
                                      (recur (nnext specs)
                                             (list* [spec (fnext specs) nil] triples)))

                                    :else
                                    (throw (throw-illegal-argument-exception (str "invalid field spec " spec " in " *ns* " " (meta &form))))))))

             [?constructor & ?constructor-args] (cond
                                                  (list? ?constructor-call)
                                                  ?constructor-call

                                                  (symbol? ?constructor-call)
                                                  (concat [?constructor-call]
                                                          (map first ?field-triples)))]
      ;;; Check nongenerative option
         (if-let [non-g-id (:nongenerative ?options)]
           ;; nongenerative
           (if-let [{:keys [ns form]} (get @global-record-type-registry non-g-id)]
             (when-not (and (= ns *ns*)
                            (= form &form))
               (throw (Exception. "This record type definition already exists with different arguments.")))
             (let [non-g-id (if (= true non-g-id) (str *ns* "/" ?type) non-g-id)] ; default non-g-id when key is `true`
               (swap! global-record-type-registry
                      (fn [old-reg] (assoc old-reg non-g-id {:ns *ns* :form &form})))
               (emit-record-definition &env ?type ?options ?constructor ?constructor-args ?predicate ?field-triples ?opt+specs)))
           ;; generative, create new type.
           (emit-record-definition &env ?type ?options ?constructor ?constructor-args ?predicate ?field-triples ?opt+specs))))
     ))

;; (defn predicate->record-meta [predicate]
;;   ;; Expects a namespace resolved predicate
;;   ;; if the predicate meta contains UnresolvedRecordMeta it returns a RecordMeta
;;   ;; record with resolved values. Else nil.
;;   (:meta (meta predicate)))

;; (defn record-type-predicate? [foo]
;;   (instance? RecordMeta (predicate->record-meta foo)))
