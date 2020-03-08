;; Utility functions for rendering state
(ns robinson.fx.boomerang-item
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

(defn boomerang-control-points
  [player-pos target-pos]
  (let [midpoint (rc/midpoint player-pos target-pos)
        tangent (rc/tangent (rc/sub-pos target-pos player-pos))
        mp1 (rc/sub-pos midpoint (rc/scale 0.5 tangent))
        mp2 (rc/add-pos midpoint (rc/scale 0.5 tangent))
        s1 (rc/sub-pos player-pos (rc/scale 0.2 tangent))
        s2 (rc/sub-pos player-pos (rc/scale -0.2 tangent))]
    [player-pos
     s1
     mp1
     target-pos
     mp2
     s2
     player-pos]))

(defn boomerang-path
  [player-pos target-pos]
  (->>
    (boomerang-control-points player-pos target-pos)
    (map rc/pos->xy)
    rcr/catmull-rom-chain 
    (map (fn [[x y]] [(int x) (int y)]))))
 
(defn handle-recieve
  [this state item xy-path ch-cycle ttl fx-ks]
  (let [[x y] (first xy-path)
        ch (first ch-cycle)
        npc (rnpc/npc-at-pos state (rc/xy->pos x y))
        ttl-zero (zero? ttl)]
    (log/info "ttl" ttl)
    (log/info "x y" x y)
    (log/info "npc" npc)
    (letfn [(cleanup [state]
              (-> state
                (ractors/remove-actor this)
                (rc/dissoc-in fx-ks)))
            (handle-npc [state npc]
              (rcombat/attack state [:world :player] (rnpc/npc->keys state npc) item))
            (move [state]
              (-> state
                ; move item
                (assoc-in (conj fx-ks :pos) (apply rc/xy->pos (first xy-path)))
                (assoc-in (conj fx-ks :ch) (first ch-cycle))
                ; update path
                (ractors/update-actor this update :xy-path rest)
                (ractors/update-actor this update :ch-cycle rest)
                ; update ttl
                (ractors/update-actor this update :ttl dec)))
            #_(handle-trigger-trap [state]
              ; triggers a trap
              ;; remove item and trigger trap
              (-> state
                (rc/append-log "You throw it at the trap.")
                (rp/dec-item-count (get item :hotkey))
                (rt/trigger-if-trap state [x y])))
              (handle-ttl-zero [state]
                ; hits the ground when ttl = 0
                ;; didn't hit anything, drop into cell at max-distance
                (cleanup state))
              (on-move [state]
                ; one of several things can happen
                (cond
                  ; npc in cell
                  npc
                    (-> state
                      (handle-npc npc)
                      move)
                  #_#_trap
                    (-> state
                      handle-trigger-trap
                      cleanup)
                  ttl-zero
                    (-> state
                      handle-ttl-zero
                      cleanup)
                  :default
                    (move state)))]
    
    ; will hit wall?
    #_(if (second xy-path)
      (let [next-pos (apply rc/xy->pos (second xy-path))
            next-cell (rw/get-cell state next-pos)]
        (if (rw/type->collide? (get next-cell :type))
          ;; drop item into cell before hitting colliding cell
          (if (= (get cell :type) :fire)
            ; items dropped into fire add to fuel
            (rw/update-cell state pos (fn [cell] (update-in cell [:fuel] (partial + (ig/id->fuel (get item :item/id))))))))))
      ; Nothing happened, advance item one step
    (on-move state))))

(defrecord BoomerangItemActor [item xy-path ch-cycle ttl fx-ks]
  rap/Actor
  (receive [this state]
    (try
      (handle-recieve this state item xy-path ch-cycle ttl fx-ks)
      (catch Throwable t
        (try
          (re/log-exception t state)
          (catch Throwable t
            (log/error "Error logging error" t)))
        (ractors/remove-actor state this)))))
 
(defmethod rfx/conj-effect :boomerang [state fx-type & [item persist-item xy-path ttl]]
  (let [fx-id (rfx/fx-id)
        start-pos (apply rc/xy->pos (first xy-path))
        target-pos (apply rc/xy->pos (last xy-path))
        xy-path (boomerang-path start-pos target-pos)
        ch-cycle (mapcat identity (repeat [\> \^ \< \v]))
        actor (->BoomerangItemActor item xy-path ch-cycle (count xy-path) (rfx/fx-ks fx-id))]
    (-> state
      ; create a character fx
      (rfx/conj-fx (rfx/character-fx (first ch-cycle) (apply rc/xy->pos (first xy-path))) fx-id)
      ; create a corresponding actor that controls the fx
      (ractors/add-actor actor))))

