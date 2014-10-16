(defproject active-clojure "0.2.0-SNAPSHOT"
  :description "Active Clojure: Various Clojure utilities in use at Active Group"
  :url "http://github.com/active-group/reacl/active-group"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2280" :scope "provided"]]

  :plugins [;; NB: The :exclusions argument quiets version-ranges warning.
            [com.keminglabs/cljx "0.4.0" :exclusions [org.clojure/clojure]]
            [lein-cljsbuild "1.0.3"]
            ;; NB: This needs a version of clojurescript.test with the Nashorn runner,
            ;; for example from the nashorn-runner branch from
            ;; https://github.com/active-group/clojurescript.test
            ;; The :exclusions argument quiets version-ranges warning.
            [com.cemerick/clojurescript.test "0.3.2-SNAPSHOT"  :exclusions [org.clojure/clojure]]
            [org.bodil/lein-nashorn "0.1.2"]]

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/generated/src"
                   :rules :clj}

                  {:source-paths ["src/cljx"]
                   :output-path "target/generated/src"
                   :rules :cljs}

                  {:source-paths ["test/cljx"]
                   :output-path "target/generated/test/clj"
                   :rules :clj}
 
                  {:source-paths ["test/cljx"]
                   :output-path "target/generated/test/cljs"
                   :rules :cljs}]}
  :source-paths ["target/generated/src"]

  :test-paths ["target/generated/test/clj"]

  :cljsbuild {:builds
              {:dev {:source-paths ["target/classes"]
                             :compiler {:output-to "target/main.js"
                                        :optimizations :whitespace
                                        :pretty-print true}}
               :test {:source-paths ["target/generated/src"
                                     "target/generated/test/cljs"]
                      :compiler {:output-to "target/test.js"
                                 :optimizations :whitespace
                                 :pretty-print true}}}
              :test-commands {"nashorn" ["jrunscript" "-e" "var global = this" "-f" :nashorn-runner "target/test.js"]}}

  :hooks [cljx.hooks leiningen.cljsbuild]

  :aliases {"testall" ["do" "cljsbuild" "test" "nashorn," "test"]}
  
  :global-vars {*warn-on-reflection* true})
