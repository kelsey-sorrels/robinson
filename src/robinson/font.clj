(ns robinson.font
  (:require [robinson.fs :as rfs]
            [zaffre.font :as zfont]
            [taoensso.timbre :as log]
            clojure.edn))

(defn make-font-fn [font platform]
  (log/info "make-font-fn" font)
  (case (get font :type :ttf)
    :ttf
      (zfont/ttf-font
        (rfs/cwd-path
          (get font (case platform
                      :linux   :linux-font
                      :macosx  :macosx-font
                      :windows :windows-font
                      (assert false (str "Unknown platform requested" platform)))))
        (get font :font-size)
        true #_(get font :transparent))
    :cp437
      (zfont/scale
        (zfont/cp-437
          (let [{:keys [url path]} font]
            (if url
              url
              (rfs/cwd-path path)))
          (get font :alpha-channel :green)
          true #_(get font :transparent))
        (get font :scale 1))))

(defn current-font
  [state]
  (let [font-key (-> state :settings deref :font)
        font     (-> state :fonts font-key)]
    font))

(defn read-font-configs
  []
  (apply sorted-map
    (mapcat (fn [file]
              (let [file-name (.getName file)
                    map-key   (if (re-find #".edn$" file-name)
                                (keyword (clojure.string/replace-first file-name #".edn$" ""))
                                (keyword file-name))
                    map-value (->> (.getPath file)
                                (slurp)
                                (clojure.edn/read-string))]
              [map-key
               map-value]))
            (.listFiles (rfs/cwd-file "config/fonts")))))

