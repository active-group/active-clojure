(defproject active-clojure "0.12.0-SNAPSHOT"
  :description "Active Clojure: Various Clojure utilities in use at Active Group"
  :url "http://github.com/active-group/active-clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "0.0-3308"]
                 [io.aviso/pretty "0.1.18"]]

  :generated-paths ["target"]
    
  :clean-targets ^{:protect false} [:generated-paths]

  :cljsbuild {:builds
              {:dev {:source-paths ["src"]
                     :compiler {:output-to "target/main.js"
                                :source-map "target/main.map"
                                :optimizations :whitespace
                                :pretty-print true}}
               :test {:source-paths ["src" "test"]
                      :compiler {:output-to "target/test.js"
                                 :source-map "target/test.map"
                                 :optimizations :whitespace
                                 :pretty-print true}}}
              :test-commands {"phantomjs" ["phantomjs" 
                                           "test/vendor/unit-test.js" "test/vendor/unit-test.html"]
                              }}


  :plugins [[lein-cljsbuild "1.0.6"]]

  :global-vars {*warn-on-reflection* true})
