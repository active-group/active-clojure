(defproject active-clojure "0.3.0-SNAPSHOT"
  :description "Active Clojure: Various Clojure utilities in use at Active Group"
  :url "http://github.com/active-group/reacl/active-group"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2371"]]

  :plugins [;; NB: The :exclusions argument quiets some version-ranges warning.
            [com.keminglabs/cljx "0.4.0" :exclusions [org.clojure/clojure]]
            [lein-cljsbuild "1.0.3"]
            ;; NB: This needs a version of clojurescript.test with the Nashorn runner,
            ;; for example from the nashorn-runner branch from
            ;; https://github.com/active-group/clojurescript.test
            ;; The :exclusions argument quiets some version-ranges warning.
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
  :source-paths ["src"
                 "target/generated/src"]

  :test-paths ["test" 
               "target/generated/test/clj"]

  :cljsbuild {:builds
              {:dev {:source-paths ["target/classes"]
                     :compiler {:output-to "target/main.js"
                                :source-map "target/main.map"
                                :optimizations :whitespace
                                :pretty-print true}}
               :test {:source-paths ["src"
                                     "target/generated/src"
                                     "target/generated/test/cljs"]
                      :compiler {:output-to "target/test.js"
                                 :source-map "target/test.map"
                                 :optimizations :whitespace
                                 :pretty-print true}}}
              :test-commands {"nashorn" ["jrunscript" "-e" "var global = this" "-f" :nashorn-runner "target/test.js"]
                              "phantomjs" ["phantomjs" :runner "this.literal_js_was_evaluated=true" "target/test.js"]
                              ;; open test-resources/unit-tests.html in a browser and look at the JavaScript console
                              }}

  :hooks [cljx.hooks leiningen.cljsbuild]

  :jar-exclusions [#"^cljx/"]

  :aliases {"testall" ["do" "cljx," "test"]}
  
  :global-vars {*warn-on-reflection* true})
