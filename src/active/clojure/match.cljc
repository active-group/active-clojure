(ns active.clojure.match
  "Syntactic sugar for map matching around `core.match`."
  (:require #?(:clj [clojure.core.match :as match])
            #?(:clj [clojure.core.match.regex]))
  #?(:cljs
  (:require-macros [clojure.core.match :as match])))

#?(:clj
(defmethod match/to-source ::match/regex
  [pat ocr]
  `(and (not= ~ocr ::match/not-found) (re-matches ~(:regex pat) ~ocr))))

#?(:clj
(defmacro map-matcher
  "Construct a map matcher.  Syntactic sugar for `core.match`.

  The syntax is `(map-matcher <pattern> <consequent> ... :else <alternative>)` where
  `<pattern>` is a vector of clauses `[<clause>+]` where `clause` is one of the following:

  - `(<key> <value> :as <name>)` which requires the key `<key>` to be
    mapped to `<value>` in the map and binds `<name>` to `<value>`.

  - `(<key-and-name> <value>)` which requires the key `<key-and-name>`
    to be mapped to `<value>` in the map and binds `<key-and-name>` to
    `<value>`.

  - `(<key> :as <name>)` which requires `<key>` to be present in the map
    and binds `<name>` to its value.

  - `<key-and-name>` which requires `<key-and-name>` to be present in
    the map and binds `<key-and-name>` to its value

  The map matcher also supports optional keys:

  - `(? <key> <default> :as <name>)` binds `<name>` to to the value of
    `<key>` in the map or to `<default>` if `<key>` is not in the map.

  - `(? <key-and-name> <default>)` binds `<key-and-name>` to the value of
    `<key-and-name>` in the map or to `<default>` if `<key-and-name>` is not
    in the map.

  - `(? <key> :as <name>)` binds `<name>` to to the value of `<key>`
    in the map or to `nil` if `<key>` is not in the map.

  - `(? <key-and-name>)` binds `<key-and-name>` to the value of
    `<key-and-name>` in the map or to `nil` if `<key-and-name>` is not
    in the map.

  Access to nested values is also possible.  Use `[<key>+]` to access
  a nested value, where `[<key>+]` is a sequence of keys.  When no
  `:as <name>` clause is given, the last `<key>` of the sequence of
  keys is used as a name to bind the value.

  `<key>` and `<key-and-name>` can be either a symbol or a keyword.
  If `<key-and-name>` is a symbol, it is converted to a string when
  used as a key (and used as symbol for binding the value).  If
  `<key-and-name>` is a keyword, it is converted to a name for binding
  the value (and usesd as keyword when used as a key).

  `<value>` can be any value, regular expressions are also
  possible (only in Clojure, though, `core.match` does not support
  regex matching in ClojureScript).

  `<value>` also can be a list of alternative values in the form of:
  `(:or <value> <value>*)`.

  `map-matcher` returns a function that accepts a map and evaluates
  `<consequent>` with all the `<name>`s bound when the message matches
  the given `<clause>`s, otherwise it evaluates `<alternative>`. or
  throws `IllegalArgumentException` if `<clause>` matches and no
  `<alternative>` is given.

  Example:

        (def example-map-matcher
          (map-matcher
            [(:x \"x\" :as x)
             (:y \"y\")
             (:z :as z)
             :w]
            (println x y z w)
            [(:a \"a\" :as a)
             (:b \"b\")
             (:c :as c)
             ([:d Z] 42 :as Z)
             ([:d Y] :as Y)
             ([:d X] 65)
             [:d W foo]]
            (println a b c Z Y X foo)
            :else false))

        (example-map-matcher {:a \"a\" :b \"b\" :c \"c\"
                              :d {\"Z\" 42 \"Y\" 23 \"X\" 65
                                  \"W\" {\"foo\" \"bar\"}}})

    prints

     \"a b c d 42 23 65 bar\""
  [& args]
  (when-not (even? (count args))
    (throw (IllegalArgumentException. (str "expecting an even number of arguments " *ns* " " (meta &form)))))
  (let [message `message#
        [bindings match-clauses+consequents]
        (reduce (fn [[b mcc] [clauses consequent]]
                  (if (= :else clauses)
                    [b (concat mcc [clauses consequent])]
                    (let [clauses (if (symbol? clauses) (eval clauses) clauses)
                          match-and-bind-clauses-with-as (filter #(and (seq? %) (not= '? (first %)) (= 4 (count %))) clauses)
                          destructure-clauses-with-as (filter #(and (seq? %) (not= '? (first %)) (= 3 (count %))) clauses)
                          match-and-bind-clauses (filter #(and (seq? %) (not= '? (first %)) (= 2 (count %))) clauses)
                          destructure-clauses (filter #(or (keyword? %) (symbol? %) (vector? %)) clauses)
                          make-name #(if (keyword? %) (symbol (name %)) %)
                          make-key #(if (keyword? %) % (str %))
                          make-keys #(if (vector? %) (vec (map make-key %)) [(make-key %)])
                          special-default #(if (and (list? %) (= :or (first %))) nil %)
                          make-binding (fn [b v] [(make-name (if (vector? b) (last b) b)) v])
                          make-post-binding (fn [b k d] (make-binding b `(get-in ~message ~(make-keys k) ~(special-default d))))
                          required-bindings
                          (vec (concat
                                (mapcat (fn [[k v _ b]] (make-post-binding b k v)) match-and-bind-clauses-with-as)
                                (mapcat (fn [[kb v]] (make-post-binding kb kb v)) match-and-bind-clauses)))
                          assoc-once (fn [m k v] (if (get-in m k)
                                                   (throw (IllegalArgumentException. (str "keys must be unique in " *ns* " " (meta &form) ": key " k " is already in " m)))
                                                   (assoc-in m k v)))
                          make-match-map (fn [m k b]
                                           [m k b]
                                           (if (vector? k)
                                             (assoc-once m (vec (map make-key k))
                                                       (make-name (if (vector? b) (last b) b)))
                                             (assoc-once m [(make-key k)] (make-name (if (vector? b) (last b) b)))))
                          match-clause (reduce (fn [m [k b]]
                                                 (make-match-map m k b))
                                               {}
                                               (concat
                                                (map (fn [[k v _ b]] [k v]) match-and-bind-clauses-with-as)
                                                (map (fn [[kb v]] [kb v]) match-and-bind-clauses)
                                                (map (fn [[k _ b]] [k b]) destructure-clauses-with-as)
                                                (map (fn [kb] [kb (make-name kb)]) destructure-clauses)))
                          all-optional-clauses (filter #(and (seq? %) (= '? (first %))) clauses)
                          optional-clauses (filter #(= 2 (count %)) all-optional-clauses)
                          optional-clauses-with-default (filter #(= 3 (count %)) all-optional-clauses)
                          optional-clauses-with-as (filter #(= 4 (count %)) all-optional-clauses)
                          optional-clauses-with-as-and-default (filter #(= 5 (count %)) all-optional-clauses)
                          optional-bindings
                          (vec (concat
                                (mapcat (fn [[? k d _ b]] (make-post-binding b k d)) optional-clauses-with-as-and-default)
                                (mapcat (fn [[? k _ b]] (make-post-binding b k nil)) optional-clauses-with-as)
                                (mapcat (fn [[? kb d]] (make-post-binding kb kb d)) optional-clauses-with-default)
                                (mapcat (fn [[? kb]] (make-post-binding kb kb nil)) optional-clauses)))
                          bindings (vec (concat required-bindings optional-bindings))
                          _ (let [names (map first (partition 2 required-bindings))]
                              (when (and (not-empty names) (not (apply distinct? names)))
                                (throw (IllegalArgumentException. (str "binding names must be unique in " *ns* " "
                                                                       (meta &form) ": names "
                                                                       (mapv key (remove (comp #{1} val) (frequencies names)))
                                                                       " are not uniqe")))))]
                      [(vec (concat b bindings)) (concat mcc [match-clause (if (empty? bindings)
                                                                             consequent
                                                                             `(let ~bindings ~consequent))])])))
                [[] []] (partition 2 args))]
    `(fn [~message]
         (match/match ~message
                      ~@match-clauses+consequents)))))

#?(:clj
(defmacro defpattern
  "Bind a match pattern to a name.

  The syntax is `(defpattern <name> <pattern>)` where `<pattern>` is a
  pattern for `map-matcher`, where this binding is supposed
  to be used with."
  [binding pattern]
  `(def ~binding (quote ~pattern))))

#?(:clj
(defmacro matcher
  [& args]
  (let [event `event#]
    `(fn [~event]
       ((map-matcher ~@args) ~event)))))

#?(:clj
(defmacro match
  [event & args]
  `((matcher ~@args) ~event)))
