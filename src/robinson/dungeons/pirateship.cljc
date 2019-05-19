;; Functions for randomly generating pirate ships
(ns robinson.dungeons.pirateship
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

(defn read-level-data [level]
  {:pre [(<= 0 level 4)]}
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

(def bulkhead       (make-tile \◘   8 :bulkhead))
(def wheel          (make-tile \○   9 :wheel))
(def bulkhead2      (make-tile \◙  10 :bulkhead2))
(def wooden-wall    (make-tile \#  35 :wooden-wall))
(def railing        (make-tile \‼  19 :railing))
(def hammock-v      (make-tile \)  41 :hammock-v))
(def hammock-h      (make-tile \-  45 :hammock-h))
(def deck           (make-tile \.  46 :deck))
(def close-door     (make-tile \+  43 :close-door))
(def up-stairs      (make-tile \<  60 :up-stairs))
(def cannon-breach  (make-tile \=  61 :canon-breach))
(def down-stairs    (make-tile \>  62 :down-stairs))
(def shallow-water  (make-tile \~ 126 :shallow-water))
(def tackle         (make-tile \º 167 :tackle))
(def cannon         (make-tile \║ 186 :canon))
(def grate          (make-tile \╬ 206 :grate))
(def mast           (make-tile \╨ 208 :mast))
(def table          (make-tile \╤ 209 :table))
(def chair          (make-tile \╥ 210 :chair))
(def beam           (make-tile \═ 205 :beam))
(def cannon-truck-1 (make-tile \▄ 220 :canon-truck-1))
(def locker         (make-tile \▌ 221 :locker))
(def locker2        (make-tile \▐ 222 :locker2))
(def cannon-truck-2 (make-tile \▀ 223 :canon-truck-2))
(def ships-wheel    (make-tile \Φ 232 :ships-wheel))
(def artifact-chest (make-tile \∩ 239 :artifact-chest))
(def ladder         (make-tile \≡ 240 :ladder))
(def porthole       (make-tile \° 248 :porthole))
(def chest          (make-tile \■ 254 :chest))

(defn gen-chest-item [level]
  (let [item (ig/id->item (rand-nth [:spices
                                     :plank
                                     :sail
                                     :dice
                                     :blanket
                                     :cup
                                     :silver-bar
                                     :bowl
                                     :fork
                                     :spoon
                                     :rag
                                     :knife
                                     :rope]))]
    (case (get item :type)
      :knife
        (assoc item :utility 10)
      item)))

(defn ship->cells
  [ship level]
  (mapv (fn [line]
         (mapv (fn [{ch :ch :as tile}]
                 (if (contains? #{nil \space} ch)
                   nil
                   (when-let [cell-type (-> ch
                                        int
                                        lookup-tile
                                        :type)]
                     (cond
                       ;; remove some tables, chairs, and chests
                       (contains? #{:table :chair :chest} cell-type)
                         (if (< 0.5 (rand))
                           {:type :deck}
                           (if (and (= cell-type :chest)
                                    (< 0.2 (rand)))
                             {:type cell-type :items (vec (repeatedly (int (rand 3)) (partial gen-chest-item level)))}
                             {:type cell-type}))
                       ;; always put items in the artifact chest
                       (= cell-type :artifact-chest)
                         {:type cell-type  :items (rr/rand-nth [[(ig/id->item :cutlass)]
                                                                [(ig/id->item :pistol)
                                                                 (ig/id->item :paper-cartridge)
                                                                 (ig/id->item :paper-cartridge)
                                                                 (ig/id->item :paper-cartridge)
                                                                 (ig/id->item :paper-cartridge)
                                                                 (ig/id->item :paper-cartridge)
                                                                 (ig/id->item :paper-cartridge)
                                                                 (ig/id->item :paper-cartridge)
                                                                 (ig/id->item :paper-cartridge)
                                                                 (ig/id->item :paper-cartridge)
                                                                 (ig/id->item :paper-cartridge)
                                                                 (ig/id->item :paper-cartridge)
                                                                 (ig/id->item :paper-cartridge)]
                                                                [(ig/id->item :ale)]
                                                                [(ig/id->item :navy-uniform)]
                                                                [(ig/id->item :pirate-clothes)]])}
                       ;; include enouch information in down-stairs so
                       ;;; that the next level can be created when the player uses the stairs
                       (= cell-type :down-stairs)
                         {:type      cell-type
                          :dest-type :pirate-ship
                          :gen-args  [(inc level)]}
                       :else
                         {:type cell-type}))))
               line))
        ship))

(defn level->monster-probabilities
  [level]
  (get {1 [1 :rat
           1 :spider
           1 :gecko]
        2 [1 :spider
           1 :snake]
        3 [1 :snake
           1 :crab]
        4 [1 :crab
           1 :snake]}
       level))

(defn make-boss-npcs [cells boss-type]
 (case boss-type
   :eels
    (reduce (fn [npcs [_ x y]]
              (conj npcs (assoc (mg/id->monster :eel)
                                :pos (rc/xy->pos x y))))
            []
            (take 10
              (shuffle (filter (fn [[{cell-type :type} _ _]]
                                 (= cell-type :shallow-water))
                               (rw/with-xy cells)))))
   :giant-rat
    (reduce (fn [npcs [_ x y]]
              (conj npcs (assoc (mg/id->monster :giant-rat)
                                :pos (rc/xy->pos x y))))
            []
            (take 1
              (shuffle (filter (fn [[{cell-type :type} _ _]]
                                 (= cell-type :shallow-water))
                               (rw/with-xy cells)))))
   :giant-lizard
    (reduce (fn [npcs [_ x y]]
              (conj npcs (assoc (mg/id->monster :giant-lizard)
                                :pos (rc/xy->pos x y))))
            []
            (take 1
              (shuffle (filter (fn [[{cell-type :type} _ _]]
                                 (= cell-type :shallow-water))
                               (rw/with-xy cells)))))))

(defn make-npcs [cells level]
  (let [monster-probabilities (partition 2 (level->monster-probabilities level))]
  (log/info "monster-probabilities" monster-probabilities)
  (reduce (fn [npcs [_ x y]]
            (conj npcs (assoc (mg/id->monster (rr/rand-weighted-nth monster-probabilities))
                              :pos (rc/xy->pos x y))))
          (if (= level 3)
            (make-boss-npcs cells (rand-nth [:eels :giant-rat :giant-lizard]))
            [])
          (take 10
            (shuffle (filter (fn [[{cell-type :type} _ _]]
                               (= cell-type :deck))
                             (rw/with-xy cells)))))))

(defn random-place
  "Create a pirate ship place. Returns a place, not a state."
  [level]
  (let [ship (make-ship level)
        cells (ship->cells ship level)
        npcs  (if (> level 0)
                (make-npcs cells level)
                [])]
    {:cells    cells
     :movement :fixed
     :discovered-message :pirate-ship
     :npcs npcs}))

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

(defn merge-cell
  [cell ship-cell x y]
  (or ship-cell cell))

(defn merge-cells
  [cells]
  (let [ship-place (random-place 0)]
    (mapv (fn [line ship-line y]
            (mapv (fn [cell ship-cell x]
                   (merge-cell cell ship-cell x y))
                  line
                  ship-line
                  (range)))
          cells
          (get ship-place :cells)
          (range))))

(defn -main
  "Generate a random grid and print it out."
  [& args]
  (let [level (or (read-string (first args)) 0)]
    (println (format "generating level %d..." level))
    (doall (map println (place-to-ascii (get (random-place level) :place))))))

