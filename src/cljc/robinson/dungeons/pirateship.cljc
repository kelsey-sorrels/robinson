;; Functions for randomly generating pirate ships
(ns robinson.dungeons.pirateship
  (:require [clojure.math.combinatorics :as combo]
            [algotools.algos.graph :as graph]
            [taoensso.timbre :as timbre]
            [rockpick.core :as rpc]
            clojure.set))

(defn canvas
  "Create a blank grid using `nil` for cell values."
  [width height]
  (vec (repeat height (vec (repeat width nil)))))

(defn read-level-data [level]
  {:pre [(<= 0 level 1)]}
  (let [files (filter (fn [file] (re-matches (re-pattern (format ".*%d-\\d.xp" level)) (.getName file)))
                      (file-seq (clojure.java.io/file "data/ship")))
        paths (map (fn [file] (.getPath file)) files)
        ships (map (fn [file] (rpc/read-xp (clojure.java.io/input-stream (.getPath file)))) files)]
    {:fore (map (fn [ship] (map (fn [line] (subvec line 0 30)) (nth ship 0))) ships)
     :mid  (map (fn [ship] (map (fn [line] (subvec line 30 46)) (nth ship 0))) ships)
     :aft  (map (fn [ship] (map (fn [line] (subvec line 46 80)) (nth ship 0))) ships)}))

(defn make-ship [level]
  (let [level-data (read-level-data level)
        _ (println "n-fore" (count (get level-data :fore)))
        _ (println "n-mid" (count (get level-data :mid)))
        _ (println "n-aft" (count (get level-data :aft)))
        fore       (rand-nth (get level-data :fore))
        mid        (rand-nth (get level-data :mid))
        aft        (rand-nth (get level-data :aft))]
    (mapv concat fore mid aft)))

(def brown {:r 51 :g 41 :b 26})
(def ^{:private true} tiles (atom {}))

(defn make-tile
  [ch decimal-codepoint cell-type]
  (let [kw   (keyword (str ch))
        tile {:kw                kw
              :ch                ch
              :decimal-codepoint decimal-codepoint
              :unicode-codepoint (int ch)
              :type              cell-type}]
    (swap! tiles (fn [tiles]
                   (assoc tiles
                          kw tile
                          ch  tile
                          decimal-codepoint tile
                          (int ch) tile
                          cell-type tile)))
    tile))

(defn lookup-tile [k]
  (get @tiles k))

(def wooden-wall (make-tile \#  35 :wooden-wall))
(def deck        (make-tile \.  46 :deck))
(def close-door  (make-tile \+  43 :close-door))
(def up-stairs   (make-tile \<  60 :up-stairs))
(def down-stairs (make-tile \>  62 :down-stairs))
(def tackle      (make-tile \º 167 :tackle))
(def grate       (make-tile \╬ 206 :grate))
(def table       (make-tile \╤ 209 :table))
(def chair       (make-tile \╥ 210 :chair))
(def mast        (make-tile \╨ 208 :mast))
(def beam        (make-tile \═ 205 :beam))
(def ships-wheel (make-tile \Φ 232 :ships-wheel))
(def ladder      (make-tile \≡ 240 :ladder))
(def porthole    (make-tile \° 248 :porthole))
(def chest       (make-tile \■ 254 :chest))

(defn ship->cells
  [ship]
  (mapv (fn [line]
         (map (fn [{ch :ch :as tile}]
                (if (contains? #{nil \space} ch)
                  nil
                  {:type (-> ch
                           int
                           lookup-tile
                           :type)}))
              line))
        ship))

(defn random-place
  "Create a grid of random rooms with corridors connecting them and doors
   where corridors connect to rooms."
  [width height]
  (let [ship (make-ship 0)
        cells (ship->cells ship)]
    {:place cells}))
;        upstairs     (conj [(first room-centers)] {:type :up-stairs})
;        downstairs   (conj [(last room-centers)] {:type :down-stairs})]
;    {:place (apply merge-with-canvas (canvas width height)
;                   (concat corridors rooms))
;     :up-stairs upstairs
;     :down-stairs downstairs}))

(defn place-to-ascii
  "Convert a grid of cells into a list of strings so that it can be rendered."
  [place]
  (let [contents (map (fn [line]
                        (let [chs (mapv (fn [cell]
                                          (if (nil? cell)
                                              \ 
                                             (or
                                               (-> cell
                                                 :type
                                                 lookup-tile
                                                 :ch)
                                               \ )))
                                        line)]
                          (clojure.string/join chs)))
                      place)]
    contents))

(defn -main
  "Generate a random grid and print it out."
  [& args]
  (println "generating...")
  (doall (map println (place-to-ascii (get (random-place 80 25) :place)))))

