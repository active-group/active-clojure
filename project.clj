(defproject de.active-group/active-clojure "0.43.0-SNAPSHOT"
  :description "Active Clojure: Various Clojure utilities in use at Active Group"
  :url "http://github.com/active-group/active-clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0" :scope "provided"]
                 [org.clojure/clojurescript "1.10.238" :scope "provided"]
                 [io.aviso/pretty "1.1.1"]
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
                                    [com.bhauman/figwheel-main "0.2.18"]
                                    [com.bhauman/rebel-readline-cljs "0.1.4"]
                                    [compojure "1.6.1"]
                                    [pjstadig/humane-test-output "0.11.0"]
                                    [prismatic/schema "1.4.1"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]
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

  :aliases {"figtest-chrome" ["with-profile" "cljs,dev" "doo" "chrome-headless" "test" "once"]
            "figtest-firefox" ["with-profile" "cljs,dev" "doo" "firefox-headless" "test" "once"]
            
            "figtest-phantom" ["with-profile" "cljs,dev" "doo" "phantom" "test"]

            "fig" ["trampoline" "with-profile" "dev" "run" "-m" "figwheel.main" "-b" "dev" "-r"]

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
