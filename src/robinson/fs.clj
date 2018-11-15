(ns robinson.fs
  (:require
    clojure.string
    [clojure.java.io :as io]
    [taoensso.timbre :as log]))

(defn cwd []
  (let [jar-path (->
                   *ns*
                   class
                   .getProtectionDomain
                   .getCodeSource
                   .getLocation
                   .getPath)]
    (if (clojure.string/includes? jar-path "robinson.jar")
      (->
        jar-path
        io/file
        .toPath
        .getParent
        (str "/"))
      "")))

(defn cwd-path [path]
  (str (cwd) path))

(defn cwd-file [path]
  (io/file (cwd-path path)))
