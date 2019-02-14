(ns active.clojure.function-test-util)

(defmacro generate-tests [name repl orig fargs-list rargs-list]
  ;; Note that 'is' must be emitted unhygienic, because we want to pickup cljs.test/is resp. clojure.test/is
  `(do
     ~@(concat
        (map (fn [fargs]
               ;; comparability is given:
               `(let [args# ~fargs]
                  (~'is (= (apply ~repl args#)
                           (apply ~repl args#))
                        (str ~name " returns equal objects for equal arguments"))))
             fargs-list)
        (mapcat (fn [fargs]
                  (map (fn [rargs]
                         ;; functionality same as original:
                         `(~'is (= (apply (apply ~repl ~fargs) ~rargs)
                                   (apply (apply ~orig ~fargs) ~rargs))
                                (str ~name " returns something that works the same as clojure.core equivalent")))
                       rargs-list))
                fargs-list))))
