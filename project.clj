(defproject robinson "0.0.1-SNAPSHOT"
  :description "robinson"
  :plugins      [[lein-autoreload "0.1.0"]
                 [lein-bikeshed "0.1.8"]
                 [lein-deps-tree "0.1.2"]
                 [lein-idefiles "0.2.1"]
                 [lein-marginalia "0.8.0"]
                 [lein-kibit "0.0.8"]
                 [lein-cloverage "1.0.2"]
                 [lein-tarsier "0.10.0"]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.4.474"]
                 [org.clojure/core.cache "0.7.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/core.memoize "0.7.1"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [org.clojure/tools.reader "1.3.0"]
                 [org.clojure/data.generators "0.1.2"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.vishk/algotools "0.1.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [clj-audio "0.1.0"]
                 [org.clojars.automata/jl "1.0.0"]
                 [org.clojars.automata/mp3spi "1.9.4"]
                 [seesaw "1.5.0"]
                 [ns-tracker "0.3.1"]
                 [tailrecursion/cljson "1.0.7"]
                 [servant "0.1.5"]
                 [rockpick "0.1.0-SNAPSHOT"]
                 [aaron-santos/tinter "0.1.1-SNAPSHOT"]
                 [aaron-santos/clj-tiny-astar "0.1.1-SNAPSHOT"]
                 [zaffre "0.4.0-SNAPSHOT"]
                 [alandipert/enduro "1.2.0"]
                 [dorothy "0.0.7"]
                 [com.taoensso/nippy "2.14.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [clojure-watch "0.1.14"]
                 [overtone/at-at "1.2.0"]
                 [rm-hull/monet "0.2.2"]]
  :main robinson.core
  :manifest {"SplashScreen-Image" "icon.png"}
  :repl-init robinson.core
  :test-paths
  ["test/clj"
   "test/cljc"]

  ;:auto-clean false

  :profiles {
    :demos {:dependencies 
              [[quil "2.7.1"]]}
    :uberjar {:aot [robinson.update
                    robinson.render
                    robinson.npc
                    robinson.core]
              :keep-non-project-classes true
              :uberjar-exclusions [;#"cljs.*"
                                   ;#"clojurescript.*"
                                   ;#"clojure.core.typed.*"
                                   #"externs.*"
                                   #"goog.*"
                                  ]}}
  :filespecs [{:type :path :path "images/icon.png"}]

  :core.typed {:check [robinson.common robinson.crafting robinson.itemgen robinson.mapgen robinson.npc robinson.startgame robinson.update robinson.world
                       robinson.combat robinson.core robinson.describe robinson.endgame robinson.lineofsight robinson.main robinson.monstergen
                       robinson.player robinson.render robinson.swingterminal robinson.viewport robinson.worldgen]}
  :repl-options {:timeout 920000}
  :jvm-opts [
             ;"-agentpath:/home/santos/bin/yjp-2014-build-14096/bin/linux-x86-64/libyjpagent.so"
             "-Xdebug"
             ;"-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005"
             ;"-XX:+UnlockCommercialFeatures"
             ;"-XX:+FlightRecorder"
             "-XX:-OmitStackTraceInFastThrow"
             "-XX:MaxGCPauseMillis=20"
             "-Dhttps.protocols=TLSv1"
             ;"-Dsun.java2d.trace=log"
])
