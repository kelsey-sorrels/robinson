;; Functions that manipulate state when applying/using items
(ns robinson.apply-item
  (:require
            [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.itemgen  :as ig]
            [robinson.monstergen :as mg]
            robinson.macros
            #?@(:clj (
                [robinson.macros :as rm]
                [taoensso.timbre :as log])
                :cljs (
                [robinson.macros :as rm :include-macros true]
                [taoensso.timbre :as log :include-macros true]))))



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
      (rp/add-to-inventory state [(ig/gen-corpse (mg/gen-random-monster 1 :water))])
      :else
      state)))


(defn start-fire
  "Light something on fire, creating chaos."
  [state direction]
  (let [[target-x
         target-y] (rw/player-adjacent-xy state direction)
        target-cell   (rw/get-cell state target-x target-y)]
    (cond
      (rw/type->flammable? (get target-cell :type))
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
  [state direction]
  (-> state
    (rp/dec-item-count :match)
    (start-fire direction)))

(defn apply-fire-plough
  "Light something on fire, creating chaos."
  [state direction]
  (-> state
    (rp/player-update-thirst inc)
    (rp/dec-item-utility :fire-plough)
    (start-fire direction)))
(defn apply-hand-drill
  "Light something on fire, creating chaos."
  [state direction]
  (-> state
    (rp/player-update-hunger inc)
    (rp/dec-item-utility :hand-drill)
    (start-fire direction)))

(defn apply-bow-drill
  "Light something on fire, creating chaos."
  [state direction]
  (-> state
    (rp/dec-item-utility :bow-drill)
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
        (rp/dec-item-utility :saw)
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
      (rc/conj-in [:world :fruit :identified] (get item :id))
      (rc/append-log (format "Identified %s." (name (get item :id)))))
    (rc/append-log state (format "You're not able to identify the %s." (name (get item :id))))))


(defn apply-flint
  "Apply flint to the inventory item."
  [state item]
  (log/info "applying flint to" item)
  (if (ig/is-metal? item)
    (-> state
      (assoc-apply-item {:id :flint-and-steel})
      (rw/assoc-current-state :apply-item-normal)
      (rc/ui-hint "Pick a direction to start a fire."))
    (-> state
      (rc/append-log "You're not sure how to apply that")
      (rw/assoc-current-state :normal))))

(defn apply-sharp-item
  "Apply a sharp item to the inventory item."
  [state item]
  (log/info "applying sharp item to" item)
  (case (get item :id)
    :unhusked-coconut
    (-> state
      (rp/dec-item-count (get item :id))
      (rp/add-to-inventory [(ig/gen-item :coconut)]))
    :stick
    (-> state
      (rp/dec-item-count (get item :id))
      (rp/add-to-inventory [(ig/gen-item :sharpened-stick)]))
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
    (if (= :arrow (get target-item :id))
      (-> state
        (rp/dec-item-count :arrow)
        (rp/dec-item-count (get frog-corpse :id))
        (rp/add-to-inventory (ig/id->item (-> (get target-item :name)
                                           (clojure.string/split #"-")
                                           first
                                           (str "-tipped-arrow")
                                           keyword))))
      (rc/append-log state "You rub it all over but nothing happens."))))
(defn apply-item
  "Applies the selected item."
  [state translate-directions keyin]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [item (get-apply-item state)
        trans->dir? (comp rc/is-direction? translate-directions)]
    (log/info "apply-item" [item keyin])
    (log/info "is-direction?" ((comp rc/is-direction? translate-directions) keyin))
    (rm/first-vec-match [(get item :id) keyin]
      [:fishing-pole    trans->dir?] (apply-fishing-pole state (translate-directions keyin))
      [:match           trans->dir?] (-> state
                                       (apply-match (translate-directions keyin))
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
      [:plant-guide     :*         ] (if-let [item (rp/inventory-hotkey->item state keyin)]
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
      [:flint           :*         ] (if-let [item (rp/inventory-hotkey->item state keyin)]
                                       (apply-flint state item)
                                       state)
      [:flint-and-steel trans->dir?] (-> state
                                       (start-fire (translate-directions keyin))
                                       (rw/assoc-current-state :normal))
      [ig/id-is-sharp?  :*         ] (if-let [item (rp/inventory-hotkey->item state keyin)]
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

      [#{:red-frog-corpse
         :orange-frog-corpse
         :yellow-frog-corpse
         :green-frog-corpse
         :blue-frog-corpse
         :purple-frog-corpse}
                      :*          ] (if-let [target-item (rp/inventory-hotkey->item state keyin)]
                                      (-> state
                                        (apply-frog-corpse (get item :id) target-item)
                                        (rw/assoc-current-state  :normal))
                                      state)
      [:*              :*         ] (-> state
                                      (rc/ui-hint "You're not sure how to apply it to that.")
                                      (rw/assoc-current-state :normal)))))

