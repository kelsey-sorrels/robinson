(defproject robinson "0.4.0b1-SNAPSHOT"
  :description "robinson"
  :plugins      [[lein-bikeshed "0.1.8"]
                 [lein-deps-tree "0.1.2"]
                 [lein-idefiles "0.2.1"]
                 [lein-marginalia "0.8.0"]
                 [lein-kibit "0.0.8"]
                 [lein-cloverage "1.0.2"]
                 [lein-tarsier "0.10.0"]
                 [lein-jlink "0.2.1"]
                 [lein-binplus "0.6.5"]
                 [lein-launch4j "0.1.2"]]
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "0.6.532"]
                 [org.clojure/core.cache "0.8.2"]
                 [org.clojure/core.match "0.3.0"]
                 [org.clojure/core.memoize "0.8.2"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [org.clojure/tools.reader "1.3.2"]
                 [org.clojure/data.generators "0.1.2"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.vishk/algotools "0.1.0"]
                 [org.clojure/data.json "0.2.7"]
                 [org.clojure/tools.namespace "0.3.1"]
                 [clj-audio "0.1.0"]
                 [org.clojars.automata/jl "1.0.0"]
                 [org.clojars.automata/mp3spi "1.9.4"]
                 [seesaw "1.5.0"]
                 [ns-tracker "0.4.0"]
                 [tailrecursion/cljson "1.0.7"]
                 [servant "0.1.5"]
                 [rockpick "0.1.0"]
                 [aaron-santos/tinter "0.1.1"]
                 [aaron-santos/clj-tiny-astar "0.1.1"]
                 [zaffre "0.4.0-b10"]
                 [alandipert/enduro "1.2.0"]
                 [dorothy "0.0.7"]
                 [com.taoensso/nippy "2.14.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [clojure-watch "0.1.14"]
                 [overtone/at-at "1.2.0"]
                 [rm-hull/monet "0.2.2"]
                 [datascript "0.18.7"]
                 [net.mikera/core.matrix "0.62.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [clatrix "0.5.0"]
                 [mogenslund/liquid "1.1.3"]
                 [aysylu/loom "1.0.2"]
                 #_[incanter "1.9.3"]
                 ;dev
                 [com.clojure-goes-fast/clj-async-profiler "0.4.0"]
               ]

  :main robinson.autoreloadcore
  :manifest {"SplashScreen-Image" "icon.png"}
  :repl-init robinson.core
  :test-paths
  ["test/clj"
   "test/cljc"]

  :uberjar-exclusions [;#"cljs.*"
                       ;#"clojurescript.*"
                       ;#"clojure.core.typed.*"
                       #"robinson-tools.worldgen"
                       #"externs.*"
                       #"goog.*"
                      ]
  ;:auto-clean false

  :profiles {
    :dev {:dependencies [[org.clojure/test.check "0.10.0"]]
          :jvm-opts ["-Xdebug"]}
    :test {:dependencies [[mvxcvi/puget "1.1.2"]
                          [quil "3.1.0"]
                          [fipp "0.6.22"]]}
    :demos {:dependencies 
              [[quil "3.0.0"]]}
    :uberjar {:aot [#"robinson/.*.clj"]
              :main robinson.autoreloadcore
              :keep-non-project-classes true}
    :jlink-linux-x64 {
      :target-path "target/linux-x64"}
    :bin-linux-x64 {
      :bin {
        :name "linux-x64-robinson.jar"
        :custom-preamble ":;exec $(dirname $0)/bin/java {{{jvm-opts}}} -D{{{project-name}}}.version={{{version}}} -jar $0 \"$@\"\n"
}}
    :jlink-osx-x64 {
      :target-path "target/osx-x64"}
    :bin-osx-x64 {
      :bin {
        :name "osx-x64-robinson.jar"
        :custom-preamble ":;exec $(dirname $0)/bin/java {{{jvm-opts}}} -D{{{project-name}}}.version={{{version}}} -jar $0 \"$@\"\n"
}}
    :jlink-windows-x64 {
      :target-path "target/windows-x64"}
    :bin-windows-x64 {
      :bin {
        :name "windows-x64-robinson.jar"
        :custom-preamble "@echo off\r\njava {{{win-jvm-opts}}} -D{{{project-name}}}.version={{{version}}} -jar \"%~f0\" %*\r\ngoto :eof\r\n\")"
}}}
  :filespecs [{:type :path :path "images/icon.png"}]

  :core.typed {:check [robinson.common robinson.crafting robinson.itemgen robinson.mapgen robinson.npc robinson.startgame robinson.update robinson.world
                       robinson.combat robinson.core robinson.describe robinson.endgame robinson.lineofsight robinson.main robinson.monstergen
                       robinson.player robinson.render robinson.swingterminal robinson.viewport robinson.worldgen]}
  :repl-options {:timeout 920000}
  :global-vars {*warn-on-reflection* true
                *assert* true}
  :jlink-modules ["java.base" "java.desktop" "java.sql" "java.naming" "jdk.unsupported" "java.management" "jdk.httpserver"]
  :launch4j-install-dir "/home/santos/bin/launch4j"
  :launch4j-config-file "dev-resources/config.xml"
  :jvm-opts [
             "-Djdk.attach.allowAttachSelf"
             "-XX:+UnlockExperimentalVMOptions"
             "-XX:+UnlockDiagnosticVMOptions"
             "-XX:+DebugNonSafepoints"
             "-XX:-OmitStackTraceInFastThrow"
             ;"-XX:+UseShenandoahGC"
             ;"-XX:+UseZGC"
             ;"-Xlog:gc"
             "-XX:MaxGCPauseMillis=20"
             "-Dhttps.protocols=TLSv1"
])
