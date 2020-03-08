;; Utility functions for rendering state
(ns robinson.fx.whip-item
  (:require 
            [robinson.common :as rc]
            [robinson.error :as re]
            [robinson.catmull-rom :as rcr]
            [robinson.renderutil :as rutil]
            [robinson.math :as rmath]
            [robinson.color :as rcolor]
            [robinson.actors :as ractors]
            [robinson.actor-protocol :as rap]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.fx :as rfx]
            [robinson.describe :as rdesc]
            [robinson.npc :as rnpc]
            [robinson.itemgen :as ig]
            [robinson.traps :as rt]
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

(defn handle-recieve
  [this state item hit-npc chains ttl fx-ks]
  (let [chain (or (first chains) [])
        npcs (when-not hit-npc
               (->> chain
                 (map (fn [[x y]]
                   (rnpc/npc-at-xy state x y)))
                 (remove nil?)))
        collision (some (fn [[x y]] (rw/collide? state x y {:include-npcs? false :collide-player? false})) chain)
        ttl-zero (zero? ttl)]
    (log/info "ttl" ttl)
    (log/info "npcs" (vec npcs))
    (log/info "collision" collision)
    (letfn [(cleanup [state]
              (-> state
                (ractors/remove-actor this)
                (rc/dissoc-in fx-ks)))
            (handle-npc [state npc]
              (log/info "handle-npc" npc)
              (log/info "npc-keys" (rnpc/npc->keys state npc))
              (if hit-npc
                state
                (-> state
                  (rcombat/attack [:world :player] (rnpc/npc->keys state npc) item)
                  (ractors/update-actor this assoc :hit-npc true))))
            #_(handle-trigger-trap [state]
              ; triggers a trap
              ;; remove item and trigger trap
              (-> state
                (rc/append-log "You throw it at the trap.")
                (rp/dec-item-count (get item :hotkey))
                (rt/trigger-if-trap state [x y])))
              (on-move [state]
                ; one of several things can happen
                (cond
                  ; npc in cell
                  (not (empty? npcs))
                    (reduce handle-npc state npcs)
                  ; stop on collision
                  collision
                    (cleanup state)
                    #_(cleanup state)
                  #_#_trap
                    (-> state
                      handle-trigger-trap
                      cleanup)
                  ttl-zero
                    (cleanup state)
                  :default
                    (-> state
                      ; move item
                      (assoc-in (conj fx-ks :ch-pos) 
                        (chain->multi-character-fx-ch-pos (first chains)))
                      ; update path
                      (ractors/update-actor this update :chains rest)
                      ; update ttl
                      (ractors/update-actor this update :ttl dec))))]
    
    ; advance item one step
    (on-move state))))

(defrecord WhipItemActor [item hit-npc chains ttl fx-ks]
  rap/Actor
  (receive [this state]
    (try
      (handle-recieve this state item hit-npc chains ttl fx-ks)
      (catch Throwable t
        (try
          (re/log-exception t state)
          (catch Throwable t
            (log/error "Error logging error" t)))
        (ractors/remove-actor state this)))))
              
(defn control-points
  "Calculate spline control points in space relative to the player (player @ 0,0)."
  [r theta t]
  (let [cp-2-r (-> t Math/sin inc Math/log inc Math/log (* r))
        cp-2-dt (-> t (* 2) Math/sin (* r -0.001)) 
        cp-3-r (-> t Math/sin inc Math/log inc Math/log inc Math/log (* 1.6 r))
        cp-3-dt 0
        cp-4-r (-> t Math/sin (Math/pow 2) (* r))
        cp-4-dt (-> t (* 2) Math/sin (* r 0.002))]
  [[0 0]
   [(-> theta Math/cos (* 0.5))
    (-> theta Math/sin (* 0.5))]
   [(-> cp-2-dt (+ theta) Math/cos (* cp-2-r))
    (-> cp-2-dt (+ theta) Math/sin (* cp-2-r))]
   [(-> cp-3-dt (+ theta) Math/cos (* cp-3-r))
    (-> cp-3-dt (+ theta) Math/sin (* cp-3-r))]
   [(-> cp-4-dt (+ theta) Math/cos (* cp-4-r))
    (-> cp-4-dt (+ theta) Math/sin (* cp-4-r))]
   [(-> cp-4-dt (+ theta) Math/cos (* cp-4-r) (+ 0.01))
    (-> cp-4-dt (+ theta) Math/sin (* cp-4-r) (+ 0.01))]]))

(defn whip-chain
  "Calculate set of chain [x y]s in world space."
  [r theta origin-x origin-y t]
  (->>
    (control-points r theta t)
    (rcr/catmull-rom-chain)
    (map (fn [[x y]]
         [(int (+ x origin-x)) (int (+ y origin-y))]))
    dedupe))


(defmethod rfx/conj-effect :whip [state fx-type & [item persist-item xy-path ttl]]
  (let [fx-id (rfx/fx-id)
        [xi yi :as start] (first xy-path)
        [xf yf :as end] (last xy-path)
        dx (- xf xi)
        dy (- yf yi)
        r (+ 0.5 (rc/distance (rc/xy->pos xi yi)
                              (rc/xy->pos xf yf)))
        theta (Math/atan2 dy dx)
        chains (map (partial whip-chain r theta xi yi) (range 0.01 Math/PI 0.2))
        actor (->WhipItemActor item false chains (count chains) (rfx/fx-ks fx-id))]
    (log/info "dx:" dx " dy:" dy)
    (log/info "origin" xi yi)
    (log/info "r:" r)
    (log/info "theta:" theta)
    (-> state
      ; create a character fx
      (rfx/conj-fx (rfx/multi-character-fx []) fx-id)
      ; create a corresponding actor that controls the fx
      (ractors/add-actor actor))))

