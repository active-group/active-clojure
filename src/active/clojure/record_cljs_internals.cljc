(ns active.clojure.record-cljs-internals
  (:require #?(:clj [active.clojure.lens :as lens])))

(defn- validate-fields
  [case name fields]
  (when-not (vector? fields)
    (throw
     #?(:clj (AssertionError. (str case " " name ", no fields vector given."))
        :cljs (js/Error. (str case " " name ", no fields vector given."))))))

(defn- document-with-arglist
  [n arglist doc]
  (vary-meta n
             (fn [m]
               (let [m (if (contains? m :doc)
                         m
                         (assoc m :doc doc))]
                 (if (contains? m :arglists)
                   m
                   (assoc m :arglists `'(~arglist)))))))

(defn- name-doc
  [field]
  (if-let [doc (:doc (meta field))]
    (str " (" doc ")")
    ""))

(defn- name-spec
  [field]
  (or (:spec (meta field))
      any?))

(defn- reference
  [name]
  (str "[[" (ns-name *ns*) "/" name "]]"))


(defn report-lens-deprecation [type]
  (println (str "active.clojure.record WARNING for record-type `" type
                "`: the explicit definition of lenses is deprecated in favor of regular "
                "accessors already being lenses")))

(defn emit-javascript-record-definition
  [env ?type ?options ?constructor ?constructor-args ?predicate ?field-triples ?opt+specs]
  (let [?docref               (str "See " (reference ?constructor) ".")
        ?constructor-args-set (set ?constructor-args)
        fields                (mapv first ?field-triples)]
    (validate-fields "defrecord" ?type fields)
    `(do
       ;; direct use of `defrecord` - to be replaced in the future
       (defrecord ~?type [~@fields] ~@?opt+specs)

       (def ~?predicate (fn [x#] (instance? ~?type x#)))
       (def ~(document-with-arglist ?constructor
                                    (vec ?constructor-args)
                                    (str "Construct a `" ?type "`"
                                         (name-doc ?type)
                                         " record.\n"
                                         (apply str
                                                (map (fn [[?field ?accessor ?lens]]
                                                       (str "\n`" ?field "`" (name-doc ?field) ": access via " (reference ?accessor)
                                                            (if ?lens
                                                              (str ", lens " (reference ?lens))
                                                              "")))
                                                     ?field-triples))))
         (fn [~@?constructor-args]
           (new ~?type
                ~@(map (fn [[?field _]]
                         (if (contains? ?constructor-args-set ?field)
                           `~?field
                           `nil))
                       ?field-triples))))
       (declare ~@(map (fn [[?field ?accessor ?lens]] ?accessor) ?field-triples))
       ~@(mapcat (fn [[?field ?accessor ?lens]]
                   (let [?rec (with-meta `rec# {:tag ?type})
                         ?data `data#
                         ?v `v#]
                     `((def ~(document-with-arglist
                              ?accessor
                              (vector ?type)
                              (str "Lens for the `" ?field "` field"
                                   (name-doc ?field)
                                   " from a [[" ?type "]] record. " ?docref))
                         (lens/lens (fn [~?rec]
                                      (when-not (instance? ~?type ~?rec)
                                        (throw (js/Error. ~(str "Wrong record type (" ?rec ") passed to accessor ("
                                                                ?accessor ")."))))
                                      (. ~?rec ~(symbol (str "-" ?field))))
                                    (fn [~?data ~?v]
                                      (~?constructor ~@(map
                                                        (fn [[?shove-field ?shove-accessor]]
                                                          (if (= ?field ?shove-field)
                                                            ?v
                                                            `(~?shove-accessor ~?data)))
                                                        ?field-triples)))))
                       ~(when ?lens
                          (report-lens-deprecation ?type)
                          `(def ~?lens ~?accessor))
                       )))
                 ?field-triples))))
