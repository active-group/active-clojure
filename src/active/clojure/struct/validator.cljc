(ns active.clojure.struct.validator)

(defprotocol IStructMapValidator
  (-validate-field [this changed-key new-value])
  (-validate [this m changed-keys]))

(defn validate [t ^clojure.lang.IPersistentMap m changed-keys changed-values]
  (doall (map (fn [k v]
                (-validate-field t k v))
              changed-keys
              changed-values))
  (-validate t m changed-keys))
