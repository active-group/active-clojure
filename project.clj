(defproject active-clojure "0.5.0-SNAPSHOT"
  :description "Active Clojure: Various Clojure utilities in use at Active Group"
  :url "http://github.com/active-group/active-clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2985"]]

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
              :test-commands {"phantomjs" ["phantomjs" 
                                           "test/vendor/unit-test.js" "test/vendor/unit-test.html"]
                              }}


  :plugins [[lein-cljsbuild "1.0.5"]]

  :profiles {:dev {:plugins [ ;; NB: The :exclusions argument quiets some version-ranges warning.
                             [com.keminglabs/cljx "0.6.0" :exclusions [org.clojure/clojure]]
                             ]}}
  
  
  :prep-tasks [["cljx" "once"] "javac" "compile"]
  
  :jar-exclusions [#"^cljx/"]
  
  :aliases {"testall" ["do" "cljx," "test"]}
  
  :global-vars {*warn-on-reflection* true})
