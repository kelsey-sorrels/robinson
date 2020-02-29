;; Utility functions for rendering state
(ns robinson.fx.airborn-item
  (:require 
            [robinson.common :as rc]
            [robinson.error :as re]
            [robinson.renderutil :as rutil]
            [robinson.math :as rmath]
            [robinson.color :as rcolor]
            [robinson.actors :as ractors]
            [robinson.actor-protocol :as rap]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [robinson.fx :as rfx]
            [robinson.describe :as rdesc]
            [robinson.npc :as rnpc]
            [robinson.itemgen :as ig]
            [robinson.traps :as rt]
            [robinson.combat :as rcombat]
            [taoensso.timbre :as log]))

(defn handle-recieve
  [this state item persist-item xy-path ch-cycle ttl fx-ks]
  (let [ttl-zero (or (zero? ttl) (not (seq xy-path)))]
    (letfn [(cleanup [state]
              (-> state
                (ractors/remove-actor this)
                (rc/dissoc-in fx-ks)))]
      (if ttl-zero
        (cleanup state)
        (let [[x y :as xy] (first xy-path)
              ch (first ch-cycle)
              pos (rc/xy->pos x y)
              player-in-cell (= pos (rp/player-pos state))
              npc (rnpc/npc-at-pos state pos)
              _ (log/info "get-cell" pos)
              _ (log/info "ch" ch)
              cell (rw/get-cell state pos)
              trap (rw/is-trap-type? (get cell :type))]
          (letfn [(handle-player [state]
                    (log/error (get item :attacker) [:world :player] item)
                    (-> state
                        (rcombat/attack (get item :attacker) [:world :player] item)
                        (rw/conj-cell-items x y
                          (-> item
                            (dissoc :attacker)
                            (assoc :count 1)))))
                  (handle-npc [state npc]
                    (as-> state state
                        (rcombat/attack state [:world :player] (rnpc/npc->keys state npc) item)
                        (if (ig/requires-reload? item)
                          (rw/conj-cell-items state x y
                            (-> item
                              (dissoc :attacker)
                              (assoc :count 1)))
                          state)))
                  (handle-trigger-trap [state]
                    ; triggers a trap
                    ;; remove item and trigger trap
                    (-> state
                      (rc/append-log "You throw it at the trap.")
                      (ri/dec-item-count (get item :hotkey))
                      (rt/trigger-if-trap state [x y])))
                    (handle-ttl-zero [state]
                      ; hits the ground when ttl = 0
                      ;; didn't hit anything, drop into cell at max-distance
                      (-> state
                        (ri/dec-item-count (get item :hotkey))
                        (rw/conj-cell-items x y
                          (-> item
                            (dissoc :attacker)
                            (assoc :count 1)))))
                    (on-move [state]
                      ; one of several things can happen
                      (cond
                        ; player in cell
                        player-in-cell
                          (-> state
                            handle-player
                            cleanup)
                        ; npc in cell
                        npc
                          (-> state
                            (handle-npc npc)
                            cleanup)
                        trap
                          (-> state
                            handle-trigger-trap
                            cleanup)
                        ttl-zero
                          (-> state
                            handle-ttl-zero
                            cleanup)
                        :default
                          (-> state
                            ; move item
                            (assoc-in (conj fx-ks :pos) (apply rc/xy->pos xy))
                            (assoc-in (conj fx-ks :ch) ch)
                            ; update path
                            (ractors/update-actor this update :xy-path rest)
                            (ractors/update-actor this update :ch-cycle rest)
                            ; update ttl
                            (ractors/update-actor this update :ttl dec))))]
          
          ; will hit wall?
          (if (second xy-path)
            (let [next-pos (apply rc/xy->pos (second xy-path))
                  next-cell (rw/get-cell state next-pos)]
              (if (rw/type->collide? (get next-cell :type))
                ;; drop item into cell before hitting colliding cell
                (if (= (get cell :type) :fire)
                  ; items dropped into fire add to fuel
                  (rw/update-cell state pos (fn [cell] (update-in cell [:fuel] (partial + (ig/id->fuel (get item :item/id))))))
                  ; items not dropped into fire, get added normally
                  (-> state
                    (rw/conj-cell-items x
                                        y
                                        (if (get item :rot-time)
                                          (-> item
                                            (dissoc :attacker)
                                            (assoc :rot-time (inc (rw/get-time state))))
                                          (-> item
                                            (dissoc :attacker)
                                            (assoc :count 1))))
                    cleanup
                    (rc/append-log (format "Schwaff! Thump! The %s hits %s." (get item :name) (rdesc/describe-cell-type next-cell)))))
                ; Nothing happened, advance item one step
                (on-move state)))
            ; Nothing happened, advance item one step
            (on-move state))))))))

(defrecord AirbornItemActor [item persist-item xy-path ch-cycle ttl fx-ks]
  rap/Actor
  (receive [this state]
    (try
      (handle-recieve this state item persist-item xy-path ch-cycle ttl fx-ks)
      (catch Throwable t
        (re/log-exception t state)
        state))))
              

(defmethod rfx/conj-effect :airborn-item [state fx-type & [item persist-item xy-path ttl & [ch-cycle]]]
  (let [fx-id (rfx/fx-id)
        ch-cycle (if ch-cycle (cycle ch-cycle) (repeat \*))
        actor (->AirbornItemActor item persist-item xy-path ch-cycle ttl (rfx/fx-ks fx-id))]
    (log/debug "created AirbornItemActor " item (vec xy-path) (take 10 ch-cycle) ttl (rfx/fx-ks fx-id))
    (-> state
      ; create a character fx
      (rfx/conj-fx (rfx/character-fx (first ch-cycle) (apply rc/xy->pos (first xy-path))) fx-id)
      ; create a corresponding actor that controls the fx
      (ractors/add-actor actor))))

