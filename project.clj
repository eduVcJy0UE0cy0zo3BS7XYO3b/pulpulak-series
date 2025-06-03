(defproject pulpulak-series "0.1.0-SNAPSHOT"
  :description "Cooperative multiplayer text adventure game"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  
  :min-lein-version "2.9.0"
  
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/clojurescript "1.11.132"]
                 [org.clojure/core.async "1.6.681"]
                 
                 ;; Backend dependencies
                 [ring/ring-core "1.12.2"]
                 [ring/ring-defaults "0.5.0"]
                 [compojure "1.7.1"]
                 [http-kit "2.8.0"]
                 
                 ;; WebSocket support
                 [com.taoensso/sente "1.19.2"]
                 
                 ;; Frontend dependencies
                 [reagent "1.2.0"]
                 [re-frame "1.4.3"]
                 
                 ;; Utility libraries
                 [org.clojure/tools.logging "1.3.0"] ; Logging
                 [ch.qos.logback/logback-classic "1.4.14"] ; Logging implementation
                 [com.stuartsierra/component "1.1.0"] ; Component system
                 [cprop "0.1.20"] ; Configuration
                 ]
  
  :source-paths ["src/clj" "src/cljc"]
  
  :profiles
  {:dev {:dependencies [[binaryage/devtools "1.0.7"]
                        [cider/piggieback "0.5.3"]
                        [ring/ring-mock "0.4.0"]]
         :source-paths ["dev"]
         :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}
         :plugins [[lein-figwheel "0.5.20"]
                   [lein-cljsbuild "1.1.8"]
                   [lein-doo "0.1.11"]]}
   
   :uberjar {:aot :all
             :prep-tasks ["compile" ["cljsbuild" "once" "prod"]]}}
  
  :test-paths ["test"]
  
  :cljsbuild
  {:builds
   [{:id "dev"
     :source-paths ["src/cljs" "src/cljc"]
     :figwheel {:on-jsload "pulpulak.core/mount-root"}
     :compiler {:main pulpulak.core
                :asset-path "js/compiled/out"
                :output-to "resources/public/js/compiled/app.js"
                :output-dir "resources/public/js/compiled/out"
                :source-map-timestamp true
                :preloads [devtools.preload]
                :external-config {:devtools/config {:features-to-install :all}}}}
    
    {:id "prod"
     :source-paths ["src/cljs" "src/cljc"]
     :compiler {:main pulpulak.core
                :output-to "resources/public/js/compiled/app.js"
                :output-dir "resources/public/js/compiled/prod"
                :optimizations :advanced
                :pretty-print false}}
    
    {:id "test"
     :source-paths ["src/cljs" "src/cljc" "test"]
     :compiler {:main pulpulak.test-runner
                :output-to "resources/public/js/compiled/test.js"
                :output-dir "resources/public/js/compiled/test-out"
                :optimizations :none}}]}
  
  :figwheel {:css-dirs ["resources/public/css"]
             :server-port 3449}
  
  :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                    "target"]
  
  :main pulpulak.server
  :uberjar-name "pulpulak-standalone.jar")