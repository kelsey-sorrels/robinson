;; Functions for loading dev levels
(ns robinson.dungeons.dev
  (:require [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.monstergen :as mg]
            [robinson.itemgen :as ig]
            [robinson.world :as rw]
            [clojure.math.combinatorics :as combo]
            [algotools.algos.graph :as graph]
            [taoensso.timbre :as log]
            [rockpick.core :as rpc]
            clojure.set))

(defn canvas
  "Create a blank grid using `nil` for cell values."
  [width height]
  (vec (repeat height (vec (repeat width nil)))))

(defn read-level-data [path]
  (-> path clojure.java.io/file clojure.java.io/input-stream rpc/read-xp))

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

(def wooden-wall    (make-tile \#  35 :wooden-wall))
(def ground         (make-tile \.  46 :dirt))
(def close-door     (make-tile \+  43 :close-door))
(def shallow-water  (make-tile \~ 126 :shallow-water))

(defn level->cells
  [level]
  (mapv (fn [line]
         (mapv (fn [{ch :ch :as tile}]
                 (if (contains? #{nil \space} ch)
                   nil
                   (when-let [cell-type (-> ch
                                        int
                                        lookup-tile
                                        :type)]
                    {:type cell-type})))
               line))
        (first level)))

(defn load-place
  "Create a dev place. Returns a place, not a state."
  [path]
  (let [level (read-level-data path)
        cells (level->cells level)
        #_#_npcs  (if (> level 0)
                (make-npcs cells level)
                [])]
    {:cells    cells
     :movement :fixed
     :discovered-message :dev-level
     :npcs []}))

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
                      (get place :cells))]
    contents))

(defn merge-cell
  [cell dev-cell x y]
  (or dev-cell cell))

(defn merge-cells
  [cells path]
  (let [dev-place (load-place path)]
    (mapv (fn [line dev-line y]
            (mapv (fn [cell dev-cell x]
                   (merge-cell cell dev-cell x y))
                  line
                  dev-line
                  (range)))
          cells
          (get dev-place :cells)
          (range))))

(defn -main
  "Generate a random grid and print it out."
  [& args]
  (println args)
  (let [path (first args)]
    (println (format "loading level %s..." (first args)))
    (doall (map println (place-to-ascii (get (load-place path) :place))))))

