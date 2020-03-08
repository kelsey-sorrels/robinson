;; Utility functions for rendering state
(ns robinson.fx.lightning
  (:require 
            [robinson.common :as rc]
            [robinson.error :as re]
            [robinson.catmull-rom :as rcr]
            [robinson.renderutil :as rutil]
            [robinson.math :as rmath]
            [robinson.color :as rcolor]
            [robinson.random :as rr]
            [robinson.actors :as ractors]
            [robinson.actor-protocol :as rap]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.fx :as rfx]
            [robinson.describe :as rdesc]
            [robinson.npc :as rnpc]
            [robinson.itemgen :as ig]
            [robinson.traps :as rt]
            [robinson.viewport :as rv]
            [robinson.combat :as rcombat]
            [taoensso.timbre :as log]))

(defn link-to-ch
  [[[x1 y1 :as p1] [x2 y2 :as p2] [x3 y3 :as p3]]]
  (cond
    ; start
    (= p1 p2)
      (cond
        (= x2 x3) \|
        (= y2 y3) \-
        (< x2 x3) \\
        :else \/)
    ; end
    (= p2 p3)
      (cond
        (= x1 x2) \|
        (= y1 y2) \-
        (< x1 x2) \\
        :else \/)
    ; middle
    ; vertical
    (= x1 x3) \|
    ; horizontal
    (= y1 y3) \-
    ; above
    (< y2 y3)
      (if (< x2 x3)
        \\
        \/)
    (> y2 y3)
      (if (< x2 x3)
        \/
        \\)
    :else \space))

(def colors [
  (rcolor/color->rgb :white)
  (rcolor/color->rgb :fushia)
  (rcolor/color->rgb :purple)
  (rcolor/color->rgb :dark-blue)])

(defn chain->multi-character-fx-ch-pos
  [chain]
  (map (fn [[[x1 y1] [x2 y2] [x3 y3] :as link]]
    {:pos (rc/xy->pos x2 y2)
     :ch (link-to-ch link)})
    ; sliding window
    (partition 3 1
      (concat [(first chain)]
              chain
              [(last chain)]))))

(defn fork-points
  [xys]
  (->>
    (concat [(first xys)]
            xys
            [(last xys)])
    ; sliding window
    (partition 3 1)
    (filter (fn [[[x1 y1] [x2 y2] [x3 y3] :as link]]
        (and (= x1 x3)
             (not= x1 x2))))
    (map second)))

(defn random-walk-up
  [x y]
  (iterate (fn [[x y]] [(rand-nth [(dec x) (inc x)]) (dec y)]) [x y]))

(defn random-walk-down
  [x y]
  (iterate (fn [[x y]] [(rand-nth [(dec x) (inc x)]) (inc y)]) [x y]))

(defn handle-recieve
  [this state bolt-xys target-x target-y max-y ttl fx-ks]
  (let [ttl-zero (zero? ttl)]
    (log/info "ttl" ttl)
    (letfn [(cleanup [state]
              (-> state
                (ractors/remove-actor this)
                (rc/dissoc-in fx-ks)
                (cond-> (rw/type->flammable? (get (rw/get-cell state target-x target-y) :type))
                  (rw/assoc-cell target-x target-y :type :fire :fuel (rr/uniform-int 100 200)))))
              (on-move [state]
                ; one of several things can happen
                (cond
                  ttl-zero
                    (cleanup state)
                  :default
                    (-> state
                      ; move item
                      (assoc-in (conj fx-ks :ch-pos) 
                        (mapcat chain->multi-character-fx-ch-pos
                          (cons bolt-xys
                            (map (fn [[x y]] (take 4 (random-walk-down x y)))
                              (take 3 (shuffle (filter (fn [[_ y]] (< y (- max-y 5)))
                                                 (fork-points bolt-xys))))))))
                      (assoc-in (conj fx-ks :color) (nth colors (- (count colors) ttl)))
                      ; update ttl
                      (ractors/update-actor this update :ttl dec))))]
    
    ; advance item one step
    (on-move state))))

(defrecord LightningActor [bolt-xys target-x target-y max-y ttl fx-ks]
  rap/Actor
  (receive [this state]
    (try
      (handle-recieve this state bolt-xys target-x target-y max-y ttl fx-ks)
      (catch Throwable t
        (try
          (re/log-exception t state)
          (catch Throwable t
            (log/error "Error logging error" t)))
        (ractors/remove-actor state this)))))
              

(defmethod rfx/conj-effect :lightning [state fx-type & [target-pos]]
  (let [fx-id (rfx/fx-id)
        [target-x target-y] (rc/pos->xy target-pos)
        ; find min-y for lightning
        [_ min-y] (rv/viewport-xy state)
        [_ v-height] (rv/viewport-wh state)
        main-bolt (take-while (fn [[_ y]] (<= min-y y)) (random-walk-up target-x target-y))
        actor (->LightningActor main-bolt target-x target-y target-y 4 (rfx/fx-ks fx-id))]
    (log/info "main-bolt" (vec main-bolt))
    (log/info "origin" target-pos)
    (log/info "v-height" v-height)
    (log/info "min-y" min-y)
    (-> state
      ; create a character fx
      (rfx/conj-fx (rfx/multi-character-fx []) fx-id)
      ; create a corresponding actor that controls the fx
      (ractors/add-actor actor))))

