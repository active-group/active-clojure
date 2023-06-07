(ns active.clojure.struct.validator)

(defprotocol IMapValidator
  "Note: Both methods are called on every construction and change;
  usually only one of them needs to be implemented, resp. the other as
  `(constantly nil)`."
  
  (-validate-field! [this changed-key new-value]
    "Throws an exception if the new value is not acceptable for the given key.")
  (-validate-map! [this m changed-keys]
    "Throws an exception if the given new map is not acceptable. The given
     changed keys can be used to optimize the test. Other keys are
     guaranteed to have been checked before."
    ;; Note: m can be any kind of map; not guaranteed to be a struct-map.
    ))

(defn validate! [t m changed-keys changed-values]
  (assert (map? m))
  (doall (map (partial -validate-field! t)
              changed-keys
              changed-values))
  (-validate-map! t m changed-keys))

(defn valid? [t m]
  ;; Note: a pity that we have to try-catch here, but the advantage of
  ;; having the validate-fns throw, is that the stack traces for errors
  ;; will directly point at the validator impl.
  (try (validate! t m (keys m) (vals m))
       true
       #?(:clj (catch Exception e
                 false)
          :cljs (catch :default e
                  false))))
