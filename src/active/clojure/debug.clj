(ns active.clojure.debug)

(defmacro pret [x]
  "Print and return the argument."
  `(let [x# ~x]
     (println x#)
     x#))
