(defproject active-clojure "0.28.0-SNAPSHOT"
  :description "Active Clojure: Various Clojure utilities in use at Active Group"
  :url "http://github.com/active-group/active-clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238" :scope "provided"]
                 [io.aviso/pretty "0.1.34"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/test.check "0.10.0-alpha2"]]

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
                                 :main active.clojure.test-runner
                                 :optimizations :whitespace  ;; This is required for testing with nashorn.
                                 :pretty-print true}}}}

  :profiles {;; to use figwheel-main
             ;; run `lein fig` and then open browser at
             ;; http://localhost:9500/figwheel-extra-main/auto-testing
             :dev {:dependencies   [[lein-doo "0.1.7"]
                                    [com.bhauman/figwheel-main "0.2.0"]
                                    [com.bhauman/rebel-readline-cljs "0.1.4"]]
                   :resource-paths ["target" "resources"]}

             :test {:source-paths ["src" "test"]}

             :cljs {:dependencies [[org.clojure/clojurescript "1.10.238"]
                                   [com.cemerick/piggieback "0.2.2"]
                                   [org.clojure/tools.nrepl "0.2.10"]
                                   [doo "0.1.10"]]
                    ;; run CLJS repl with
                    ;; lein with-profile cljs repl
                    ;; (cemerick.piggieback/cljs-repl (cljs.repl.rhino/repl-env))
                    :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}

  :aliases {"test-nashorn" ["with-profile" "cljs" "doo" "nashorn" "test"]
            "test-phantom" ["with-profile" "cljs" "doo" "phantom" "test"]
            "fig" ["trampoline" "with-profile" "+dev,+test" "run" "-m" "figwheel.main" "-b" "dev" "-r"]}

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-doo "0.1.10"]]

  :global-vars {*warn-on-reflection* true})
