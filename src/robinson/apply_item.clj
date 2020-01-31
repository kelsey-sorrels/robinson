;; Functions that manipulate state when applying/using items
(ns robinson.apply-item
  (:require
            [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [robinson.itemgen  :as ig]
            [robinson.monstergen :as mg]
            robinson.macros
            [robinson.macros :as rm]
            [taoensso.timbre :as log]))

(defn assoc-apply-item
  [state item]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (assoc-in state [:world :apply-item] item))

(defn get-apply-item
  [state]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (get-in state [:world :apply-item]))

(defn dig-hole
  "Dig in the ground, creating a hole."
  [state]
  (let [[x y] (rp/player-xy state)]
    (rw/assoc-cell state x y :type
      (rr/rand-nth [:freshwater-hole :saltwater-hole :dry-hole]))))

(defn apply-fishing-pole
  "Start fishing for something."
  [state direction]
  (let [[target-x
         target-y] (rw/player-adjacent-xy state direction)
        target-cell   (rw/get-cell state target-x target-y)
        new-state     (case direction
                        :left  :fishing-left
                        :right :fishing-right
                        :up    :fishing-up
                        :down  :fishing-down)]
    (if (rw/type->water? (get target-cell :type))
      (-> state
        (rc/append-log "You start fishing.")
        (rw/assoc-current-state new-state))
      (rc/append-log state "You can't fish here."))))

(defn do-fishing
  "Fish somewhere."
  [state]
  (let [p (rr/uniform-int 0 50)]
    ;; chance of catching a fish
    (cond
      (= p 0)
      ;; catch a fish
      (ri/add-to-inventory state [(ig/gen-corpse (mg/gen-random-monster 1 :water))])
      :else
      state)))


(defn start-fire
  "Light something on fire, creating chaos."
  [state direction]
  (let [[target-x
         target-y] (rw/player-adjacent-xy state direction)
        target-cell   (rw/get-cell state target-x target-y)]
    (cond
      (or (rw/type->flammable? (get target-cell :type))
        (some (fn [item] (rw/type->flammable? (get item :item/id))) (get target-cell :items)))
      (-> state
        (rc/append-log (format "You light the %s." (clojure.string/replace (name (get target-cell :type))
                                                                                            #"-"
                                                                                            " ")))
        (rw/assoc-cell target-x target-y :type :fire :fuel (if (= (get target-cell :type)
                                                               :campfire)
                                                          (rr/uniform-int 500 600)
                                                          (rr/uniform-int 100 300))))
      :else
      (rc/append-log state "You don't think that is flammable."))))

(defn apply-match
  "Light something on fire, creating chaos."
  [state hotkey direction]
  (-> state
    (ri/dec-item-count hotkey)
    (start-fire direction)))

(defn apply-fire-plough
  "Light something on fire, creating chaos."
  [state direction]
  (-> state
    (rp/player-update-thirst inc)
    (ri/dec-item-utility :fire-plough)
    (start-fire direction)))

(defn apply-hand-drill
  "Light something on fire, creating chaos."
  [state direction]
  (-> state
    (rp/player-update-hunger inc)
    (ri/dec-item-utility :hand-drill)
    (start-fire direction)))

(defn apply-bow-drill
  "Light something on fire, creating chaos."
  [state direction]
  (-> state
    (ri/dec-item-utility :bow-drill)
    (start-fire direction)))

(defn saw
  "Saw nearby tree creating logs."
  [state direction keyin]
  (let [[target-x
         target-y] (rw/player-adjacent-xy state direction)
        target-cell   (rw/get-cell state target-x target-y)]
    (log/info "saw dir" direction)
    (log/info "sawing at" target-x target-y)
    (if (contains? #{:tree :palm-tree :fruit-tree} (get target-cell :type))
      (-> state
        (rc/append-log "You saw the tree into logs.")
        ;; sawing = more hunger
        (update-in [:world :player :hunger] (partial + 10))
        ;; decrease item utility
        (ri/dec-item-utility :saw)
        (rw/update-cell target-x
                        target-y
                        (fn [cell] (-> cell
                                     (dissoc :harvestable)
                                     (assoc :type (rr/rand-nth [:dirt :gravel :tall-grass :short-grass]))
                                     (assoc :items (concat (get cell :items)
                                                         (repeat (rr/uniform-int 1 2) (ig/gen-item :log))))))))
      state)))
(defn apply-plant-guide
  "Apply a plant-guide to the inventory item."
  [state item]
  (log/info "applying plant guide to" item)
  (log/info "identified"  (get-in state [:world :fruit :identified]))
  (if (ig/is-fruit? item)
    (-> state
      (rc/conj-in [:world :fruit :identified] (get item :item/id))
      (rc/append-log (format "Identified %s." (name (get item :item/id)))))
    (rc/append-log state (format "You're not able to identify the %s." (name (get item :item/id))))))


(defn apply-flint
  "Apply flint to the inventory item."
  [state item]
  (log/info "applying flint to" item)
  (if (ig/is-metal? item)
    (-> state
      (assoc-apply-item {:item/id :flint-and-steel})
      (rw/assoc-current-state :apply-item-normal)
      (rc/ui-hint "Pick a direction to start a fire."))
    (-> state
      (rc/append-log "You're not sure how to apply that")
      (rw/assoc-current-state :normal))))

(defn apply-sharp-item
  "Apply a sharp item to the inventory item."
  [state item]
  (log/info "applying sharp item to" item)
  (log/info (get item :item/id))
  (log/info (ig/is-corpse-id? (get item :item/id)))
  
  (rm/first-vec-match [(get item :item/id) item]
    [ig/is-corpse-id? :*]
      (-> state
        (ri/dec-item-count (get item :hotkey))
        (cond->
          ; always gen meat
          true
            (ri/add-to-inventory [
              (ig/gen-meat item)])
          ; if corpse has a skeleton gen bones
          (mg/has-endoskeleton? (get item :race))
            (ri/add-to-inventory [
              (ig/gen-bones item)])
          ; if corpse has hide gen hide
          (mg/has-hide? (get item :race))
            (ri/add-to-inventory [
              (ig/gen-hide item)])
          ; if corpse has feathers gen feathers
          (mg/has-feathers? (get item :race))
            (ri/add-to-inventory [
              (ig/id->item :feather)])))
    [:unhusked-coconut :*]
      (-> state
        (ri/dec-item-count (get item :hotkey))
        (ri/add-to-inventory [(ig/gen-item :coconut)]))
    [:stick :*]
      (-> state
        (ri/dec-item-count (get item :hotkey))
        (ri/add-to-inventory [(ig/gen-item :sharpened-stick)]))
    state))

(defn apply-pen
  "Apply a pen to the inventory item."
  [state item]
  (log/info "applying pen item to" item)
  (case (get item :item/id)
    :paper
    (-> state
      (ri/dec-item-count (get item :hotkey))
      (ri/add-to-inventory [(ig/gen-item :note)]))
    :stick
    (-> state
      (ri/dec-item-count (get item :hotkey))
      (ri/add-to-inventory [(ig/gen-item :sharpened-stick)]))
    state))

(defn apply-rock
  "Apply a rock to the inventory item."
  [state hotkey item]
  (log/info "applying rock to" item)
  (case (get item :item/id)
    :unhusked-coconut
    (if (>= (+ (rp/player-hunger state) 30) (rp/player-max-hunger state))
      (rc/append-log state "You're too hungry to do this.")
      (-> state
        (ri/dec-item-count (get item :hotkey))
        (ri/dec-item-count hotkey)
        (rp/player-update-hunger (partial + 30))
        (ri/add-to-inventory [(ig/gen-item :coconut)])
        (rc/append-log "You bash the husk off the coconut.")))
    state))

(defn apply-fruit-to-skin
  "Apply fruit to the skin. If it is poisonous, display a message in the future."
  [state item]
  (log/info "skin-identifiable" (get-in state [:world :fruit]))
  (as-> state state
    (rc/append-log state (format "You touch the %s to your skin." (get item :name)))
    (if (ig/skin-identifiable? state item)
      (assoc-in state [:world :player :skin-identify-activate-time]
                (apply min (remove nil?
                                   [(get-in state [:world :player :skin-identify-activate-time])
                                    (+ (rw/get-time state) (rr/uniform-int 5 10))])))
      state)))

(defn apply-fruit-to-tongue
  "Apply fruit to the tongue If it is poisonous, display a message in the future."
  [state item]
  (log/info "skin-identifiable" (get-in state [:world :fruit]))
  (as-> state state
    (rc/append-log state (format "You touch the %s to your tongue." (get item :name)))
    (if (ig/tongue-identifiable? state item)
      (assoc-in state [:world :player :tongue-identify-activate-time]
                (apply min (remove nil?
                                   [(get-in state [:world :player :tongue-identify-activate-time])
                                    (+ (rw/get-time state) (rr/uniform-int 5 10))])))
      state)))

(defn apply-frog-corpse
  "Apply the secretions of a frog corpse to an inventory item."
  [state frog-corpse target-item]
  (log/info "poisonous frogs" (get-in state [:world :frogs]))
  (as-> state state
    (rc/append-log state (format "You touch the %s to the %s." (get frog-corpse :name) (get target-item :name)))
    (if (= :arrow (get target-item :item/id))
      (-> state
        (ri/dec-item-count (get target-item :hotkey))
        (ri/dec-item-count (get frog-corpse :hotkey))
        (ri/add-to-inventory (ig/id->item (-> (get target-item :name)
                                           (clojure.string/split #"-")
                                           first
                                           (str "-tipped-arrow")
                                           keyword))))
      (rc/append-log state "You rub it all over but nothing happens."))))

(defmulti apply-item-multi
  "Applies the selected item."
  (fn [state translate-directions keyin]
    (let [item (get-apply-item state)
          trans->dir? (comp rc/is-direction? translate-directions)]
      (log/info "apply-item" [item keyin])
      (log/info "is-direction?" ((comp rc/is-direction? translate-directions) keyin))
      [(get item :item/id)
       ((comp rc/is-direction? translate-directions) keyin)])))

(defmethod apply-item-multi [:fishing-pole :direction] [state translate-directions keyin] state)

(defn apply-pen [state item] state)
(defn apply-item-message [state keyin] state)
(defn apply-flare-gun [state item] state)
(defn apply-locator-beacon [state item] state)
(defn apply-signal-mirror [state item] state)

(defn apply-tarp [state item]
  (let [xys (cons
              (rp/player-xy state)
              (rw/adjacent-xys-ext (rp/player-pos state)))]
  (reduce (fn [state [[x y] item-type]]
            (rw/conj-cell-items
              state x y
              (assoc 
                (ig/id->item :tarp-hung)
                :tarp-type item-type
                :sibling-xys xys)))
    (ri/dec-item-count state (get item :hotkey))
    (map vector
      xys
      [:tarp-hung-mid
       :tarp-hung
       :tarp-hung
       :tarp-hung-mid
       :tarp-hung-mid
       :tarp-hung-corner
       :tarp-hung-corner
       :tarp-hung-corner
       :tarp-hung-corner]))))

(defn apply-log [state item]
  (let [[x y] (rp/player-xy state)]
    (-> state
      (ri/dec-item-count (get item :hotkey))
      (rw/assoc-cell
        x y
        :type :palisade
        :prev-type (-> state (rw/get-cell x y) :type)))))

(defn apply-item [state translate-directions keyin]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [item (get-apply-item state)
        trans->dir? (comp rc/is-direction? translate-directions)]
    (log/info "apply-item" [item keyin])
    (log/info "is-direction?" ((comp rc/is-direction? translate-directions) keyin))
    (rm/first-vec-match [(get item :item/id) keyin]
      [:fishing-pole    trans->dir?] (apply-fishing-pole state (translate-directions keyin))
      [:match           trans->dir?] (-> state
                                       (apply-match keyin (translate-directions keyin))
                                       (rw/assoc-current-state :normal))
      [:fire-plough     trans->dir?] (-> state
                                       (apply-fire-plough (translate-directions keyin))
                                       (rw/assoc-current-state :normal))
      [:hand-drill      trans->dir?] (-> state
                                       (apply-hand-drill (translate-directions keyin))
                                       (rw/assoc-current-state :normal))
      [:bow-drill       trans->dir?] (-> state
                                       (apply-bow-drill (translate-directions keyin))
                                       (rw/assoc-current-state :normal))
      [:plant-guide     :*         ] (if-let [item (ri/inventory-hotkey->item state keyin)]
                                       (-> state
                                         (apply-plant-guide item)
                                         (rw/assoc-current-state  :normal))
                                       state)
      [:stick           \>         ] (-> state
                                       (dig-hole)
                                       (rw/assoc-current-state :normal))
      [:flint-axe       trans->dir?] (-> state
                                       (saw (translate-directions keyin) keyin)
                                       (rw/assoc-current-state :normal))
      [:obsidian-axe    trans->dir?] (-> state
                                       (saw (translate-directions keyin) keyin)
                                       (rw/assoc-current-state :normal))
      [:saw             trans->dir?] (-> state
                                       (saw (translate-directions keyin) keyin)
                                       (rw/assoc-current-state :normal))
      [:flint           :*         ] (if-let [item (ri/inventory-hotkey->item state keyin)]
                                       (apply-flint state item)
                                       state)
      [:rock            :*         ] (if-let [item (ri/inventory-hotkey->item state keyin)]
                                       (-> state
                                         (apply-rock keyin item)
                                         (rw/assoc-current-state :normal))
                                       state)
      [:flint-and-steel trans->dir?] (-> state
                                       (start-fire (translate-directions keyin))
                                       (rw/assoc-current-state :normal))
      [:pen             :*         ] (apply-pen state item)
      [:flare-gun       \<         ] (apply-flare-gun state item)
      [:locator-beacon  :*         ] (apply-locator-beacon state item)
      [:signal-mirror   :*         ] (apply-signal-mirror state item)
      [ig/id-is-sharp?  :*         ] (if-let [item (ri/inventory-hotkey->item state keyin)]
                                       (-> state
                                         (apply-sharp-item item)
                                         (rw/assoc-current-state :normal))
                                       state)
      ;; apply fruit to body
      [ig/id-is-fruit?  \a         ] (-> state
                                       (apply-fruit-to-skin item)
                                       (rw/assoc-current-state :normal))
      [ig/id-is-fruit?  \b         ] (-> state
                                       (apply-fruit-to-tongue item)
                                       (rw/assoc-current-state :normal))

      [:tarp            :*         ] (-> state
                                       (apply-tarp item)
                                       (rw/assoc-current-state :normal)) 
      [:bamboo          :*         ] (-> state
                                       (apply-log item)
                                       (rw/assoc-current-state :normal)) 
      [:log             :*         ] (-> state
                                       (apply-log item)
                                       (rw/assoc-current-state :normal)) 

      [#{:red-frog-corpse
         :orange-frog-corpse
         :yellow-frog-corpse
         :green-frog-corpse
         :blue-frog-corpse
         :purple-frog-corpse}
                      :*          ] (if-let [target-item (ri/inventory-hotkey->item state keyin)]
                                      (-> state
                                        (apply-frog-corpse (get item :item/id) target-item)
                                        (rw/assoc-current-state  :normal))
                                      state)
      [:*              :*         ] (do
                                      (log/info 
                                      (-> state
                                        (rc/ui-hint "You're not sure how to apply it to that.")
                                        (rw/assoc-current-state :normal)))))))

