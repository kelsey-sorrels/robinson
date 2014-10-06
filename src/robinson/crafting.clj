;; Functions for generating random items.
(ns robinson.crafting
  (:use     robinson.common
            robinson.itemgen
            robinson.player)
  (:require [pallet.thread-expr :as tx]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(def recipes
  {:weapons [
     {:name "obsidian spear"         :hotkey \a :recipe {:exhaust [:obsidian-blade :stick :rope] :add [:obsidian-spear]}}
     {:name "obsidian axe"           :hotkey \b :recipe {:exhaust [:obsidian-blade :stick :rope] :add [:obsidian-axe]}}
     {:name "obsidian knife"         :hotkey \c :recipe {:exhaust [:obsidian-blade :stick :rope] :add [:obsidian-knife]}}]
   :survival [
     {:name "obsidian blade"         :hotkey \a :recipe {:exhaust [:rock :obsidian]              :add [:obsidian-blade]}}
     {:name "rope"                   :hotkey \b :recipe {:exhaust [:plant-fiber]                 :add [:rope]}}
     {:name "sharpened stick"        :hotkey \c :recipe {:exhaust [:stick]
                                                         :have-or [:obsidian-knife
                                                                   :obsidian-spear
                                                                   :obsidian-axe
                                                                   :knife]
                                                         :add     [:sharpened-stick]}}
     {:name "bamboo water collector" :hotkey \d :recipe {:exhaust [:rope :bamboo :stick]         :add [:bamboo-water-collector]}}]
   :shelter [
     {:name "shelter"                :hotkey \a :recipe {:exhaust [:rope :leaves :stick]         :add [:shelter]}}]
   :traps [
     {:name "snare"                  :hotkey \a :recipe {:exhaust [:rope :stick]                 :add [:snare]}}
     {:name "deadfall trap"          :hotkey \b :recipe {:exhaust [:rope :stick :rock]           :add [:deadfall-trap]}}]})

(defn has-prerequisites?
  "Return true if the player has the ability to make the recipe."
  [state recipe]
  (let [inventory        (get-in state [:world :player :inventory])
        and-requirements (frequencies (concat (get-in recipe [:recipe :exhaust] [])
                                              (get-in recipe [:recipe :have-and] [])))
        or-requirements  (get-in recipe [:recipe :have-or] [])]
    (and
      (every? (fn [[requirement n]] (some (fn [item]
                                            (and (= (get item :id) requirement)
                                              (>= (get item :count 1) n)))
                                          inventory))
              and-requirements)
      (or (some   (fn [requirement] (some requirement
                                          (map :id inventory)))
                  or-requirements)
          (empty? or-requirements)))))
                                                
(defn get-recipes
  "Return recipes tagged with :applicable true if the recipe has the required pre-requisites."
  [state]
  (apply hash-map
    (mapcat
      (fn [[group-name group]]
        [group-name (map (fn [recipe] (if (has-prerequisites? state recipe)
                                        (assoc recipe :applicable true)
                                        recipe))
                         group)])
      recipes)))

(defn- exhaust-by-ids
  [state ids]
  (reduce (fn [state id]
            (do 
              (info "removing" id)
              (remove-from-inventory state id)))
          state
          ids))

(defn- add-by-ids
  [state ids]
  (reduce (fn [state id]
            (let [item (id->item id)]
            (info "adding" item)
            (add-to-inventory state [item])))
          state
          ids))
  
(defn craft-recipe
  "Perform the recipe."
  [state recipe]
  (let [exhaust (get-in recipe [:recipe :exhaust])
        add     (get-in recipe [:recipe :add])
        _ (info "crafting" recipe)]
    (if (has-prerequisites? state recipe)
      (let [state (-> state
                    (add-by-ids add)
                    (exhaust-by-ids exhaust)
                    ((fn [state] (reduce update-crafted state (map (fn [id] {:id id}) add)))))]
        state)
      (ui-hint state "You don't have the necessary items to make this recipe."))))

