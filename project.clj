(defproject active-clojure "0.13.0"
  :description "Active Clojure: Various Clojure utilities in use at Active Group"
  :url "http://github.com/active-group/active-clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.293"]
                 [io.aviso/pretty "0.1.24"]]

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
                                 ;; this fixes an error from doo
                                 :output-dir "target"
                                 :source-map "target/test.map"
                                 :main active.clojure.test-runner
                                 :optimizations :whitespace
                                 :pretty-print true}}}}

  :profiles {:dev {:dependencies [[lein-doo "0.1.6"]]}}

  :aliases {"test-nashorn" ["doo" "nashorn" "test"]}

  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-doo "0.1.6"]]

  :global-vars {*warn-on-reflection* true})
