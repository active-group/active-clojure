(defproject de.active-group/active-clojure "0.38.0-SNAPSHOT"
  :description "Active Clojure: Various Clojure utilities in use at Active Group"
  :url "http://github.com/active-group/active-clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0" :scope "provided"]
                 [org.clojure/clojurescript "1.10.238" :scope "provided"]
                 [io.aviso/pretty "0.1.34"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/test.check "0.10.0-alpha4"]]

  :generated-paths ["target"]

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
             :dev {:dependencies   [[lein-doo "0.1.10"]
                                    [com.bhauman/figwheel-main "0.2.0"]
                                    [com.bhauman/rebel-readline-cljs "0.1.4"]
                                    [compojure "1.6.1"]]
                   :source-paths ["src" "dev"]
                   :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                                     "resources/public/cljs-out"
                                                     :target-path]}

             :codox {:dependencies [[codox-theme-rdash "0.1.2"]]}

             :cljs {:dependencies [[org.clojure/clojurescript "1.10.238"]
                                   [cider/piggieback "0.4.0"]
                                   [nrepl/nrepl "0.6.0"]]
                    ;; run CLJS repl with
                    ;; lein with-profile cljs repl
                    ;; (cemerick.piggieback/cljs-repl (cljs.repl.rhino/repl-env))
                    :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}

  :aliases {"test-nashorn" ["with-profile" "cljs" "doo" "nashorn" "test"]
            "test-phantom" ["with-profile" "cljs" "doo" "phantom" "test"]
            "fig" ["trampoline" "with-profile" "+dev,+test" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "figtest" ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" active.clojure.figwheel-test-runner]
            "figtest-headless" ["run" "-m" "figwheel.main" "-fwo" "{:launch-js [\"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome\" \"--headless\" \"--disable-gpu\" \"--repl\" :open-url]}" "-co" "test.cljs.edn" "-m" active.clojure.figwheel-test-runner]
            "figtest-headless-linux" ["run" "-m" "figwheel.main" "-fwo" "{:launch-js [\"/opt/google/chrome/chrome\" \"--no-sandbox\" \"--headless\" \"--disable-gpu\" \"--repl\" :open-url] :repl-eval-timeout 30000}" "-co" "test.cljs.edn" "-m" active.clojure.figwheel-test-runner]
            ;; google-chrome-stable seems to be broken at the moment:
            ;; https://github.com/bhauman/figwheel-main/issues/159
            ;; we start chrome headless in karma in travis.yml
            ;; "figtest-travis" ["run" "-m" "figwheel.main" "-fwo" "{:launch-js [\"google-chrome-stable\" \"--no-sandbox\" \"--headless\" \"--disable-gpu\" \"--repl\" :open-url] :repl-eval-timeout 30000}" "-co" "test.cljs.edn" "-m" active.clojure.figwheel-test-runner]
            }

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-doo "0.1.10"]
            [lein-codox "0.10.7"]]

  :codox {:language :clojure
          :metadata {:doc/format :markdown}
          :themes [:rdash]
          :src-dir-uri "http://github.com/active-group/active-clojure/blob/master/"
          :src-linenum-anchor-prefix "L"}

  :global-vars {*warn-on-reflection* true})
