(ns robinson.crafting.weapon-gen
  (:require [robinson.common :as rc]
            [robinson.color :as rcolor]
            [robinson.random :as rr]
            [robinson.storylet :as rs]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.mod-protocol :as rcmp]
            [robinson.crafting.effects :as rce]
            [robinson.itemgen :as rig]
            [taoensso.timbre :as log]
            [clojure.core.strint :as i]))

(def weapon-gen-ns *ns*)
      
(def contact-types
  #{:blunt :edged :piercing :flexible})

(def wield-types
  #{:melee :thrown :ranged})

(defn contact-type?
  [t]
  (contains? contact-types t))

(defn wield-type?
  [t]
  (contains? wield-types t))

(defn other-contact-type
  [t]
  {:pre [(contact-type? t)]}
  (rand-nth (vec (disj contact-types t))))

(defn other-wield-type
  [t]
  {:pre [(wield-type? t)]}
  (rand-nth (vec (disj wield-types t))))

;;; Storylet prerequisite predicates

;; Sequencing
(defn after-any-success?
  [state]
  (-> state
    rcrafting/current-recipe 
    :check
    (= :success)))

(defn after-success?
  [skill-id]
  (fn [state]
    (-> state
      rcrafting/current-recipe 
      skill-id
      (= :success))))
  
(defn after-any-failure?
  [state]
  (-> state
    rcrafting/current-recipe 
    :check
    (= :failure)))
  
(defn after-failure?
  [skill-id]
  (fn [state]
    (-> state
      rcrafting/current-recipe 
      skill-id
      (= :failure))))

;; Items which have yet to be used for complications
(defn unused-items
  [state]
  (let [recipe (rcrafting/current-recipe state)]
    (->> recipe
      :$/items
      ; remove items where item is already in :$/complication-items
      (remove (partial contains? (get recipe :$/complication-items))))))

(defn rand-unused-item
  [state]
  (let [unused-items (unused-items state)]
    (when (seq unused-items)
      (rand-nth unused-items))))

(defn unused-item?
  [state]
  (seq (unused-items state)))

(defn wooden-unused-items
  [state]
  (->> state
    unused-items
    (filter rcrafting/wooden?)))

(defn wooden-unused-item?
  [state]
  (->> state
    wooden-unused-items
    not-empty))

(defn edged-unused-items
  [state]
  (->> state
    unused-items
    (filter rcrafting/edged?)))

(defn edged-unused-item?
  [state]
  (->> state
    edged-unused-items
    not-empty))

(defn flexible-unused-items
  [state]
  (->> state
    unused-items
    (filter rcrafting/flexible?)))

(defn flexible-unused-item?
  [state]
  (->> state
    flexible-unused-items
    not-empty))

(defn handled-unused-items
  [state]
  (->> state
    unused-items
    (filter rcrafting/handled?)))

(defn handled-unused-item?
  [state]
  (->> state
    handled-unused-items
    not-empty))

;; Complication events which have yet to be used for remedies
(defn unused-complications
  [state]
  (let [recipe (rcrafting/current-recipe state)]
    (->> recipe
      :$/complications
      (remove (partial contains? (get recipe :$/remedied-complications))))))

(defn unused-complication?
  [state]
  (-> state
    unused-complications
    seq))

(defn rand-unused-complication
  [state]
  (let [unused-complications (unused-complications state)]
    (when (seq unused-complications)
      (rand-nth unused-complications))))

(defn wrap-up-event
  [recipe valid-recipes]
  {:description "You feel like the weapon is starting to take shape. Commit to a design?"
   :event/choices 
     (concat
       (map (fn [valid-recipe hotkey]
              (log/info "recipe choice" valid-recipe)
              (let [choice-name (rcrafting/recipe-name valid-recipe false)
                    dominate-item (rcrafting/dominate-item
                                    (filter #(rcrafting/item-satisfies-any-clause? %1 valid-recipe)
                                            (get recipe :$/items #{})))
                    recipe-name (rcrafting/recipe-name (-> valid-recipe
                                                         (merge recipe)
                                                         (assoc :recipe/dominate-item dominate-item))
                                                        false)]
                (assert dominate-item (str "No dominate item found" (get recipe :$/items)))
                (assert recipe-name "No recipe name found")
                (log/info "dominate item" dominate-item)
                {:hotkey hotkey
                 :name choice-name
                 :recipe/name recipe-name
                 :recipe/types (get valid-recipe :recipe/types)
                 :recipe/dominate-item dominate-item
                 :choice/events [{
                   :description (str "You wrap up the " recipe-name ".")
                    :event/choices [
                      {:name "finish"
                       :done true}]}]}))
              valid-recipes
              [\a \b \c \d \e \f \g])
       (if (< (-> recipe :$/items count) 6)
         [{:name "keep working"
           :choice/events [:material-event]}]
         []))})

(defn check-done
  [state recipe]
  (if-let [valid-recipes (not-empty (rcrafting/valid-recipes
                                                  (get recipe :$/items)
                                                  (rcrafting/get-recipes-by-category :weapon)))]
    ; if item would complete recipe, add choice/events which gives done option
    (wrap-up-event recipe valid-recipes)
    ; if item does not complete recipe, pick next event
    {:title "Weapon Recipe"
     :description "Let's see what else we can add to this weapon."
     :event/choices [
       {:choice/events [:material-event]}]}))

(defn difficulty-name
  [difficulty]
  (cond
    (< 11 difficulty) "Legendary"
    (< 10 difficulty) "Difficult"
    (<  8 difficulty) "Hard"
    (<  6 difficulty) "Medium"
    :else "Easy"))

(defn rand-skill-id-name
  []
  (rand-nth [[:skill.id/ingenuity "Ingenuity"]
             [:skill.id/cunning "Cunning"]
             [:skill.id/awareness "Awareness"]]))

(defn skill-grad
  [skill-id]
  (let [blackest (rcolor/color->rgb :blackest)
        orange (rcolor/color->rgb :orange)]
  (case skill-id
    :skill.id/ingenuity {:fg-start blackest
                         :fg-end blackest
                         :bg-start orange
                         :bg-end orange}
    :skill.id/cunning  {:fg-start blackest
                         :fg-end blackest
                         :bg-start orange
                         :bg-end orange}
    :skill.id/awareness  {:fg-start blackest
                         :fg-end blackest
                         :bg-start orange
                         :bg-end orange}
    {:bg-start (rcolor/color->rgb :blackest)
     :bg-end (rcolor/color->rgb :blackest)})))

(defn passive-check
  [title description skill-id skill-name at-least success-event failure-event]
  (let [difficulty (difficulty-name at-least)
        roll (+ (inc (rand-int 6)) (inc (rand-int 6)))
        outcome (<= at-least roll)]
    {:title title
     :description description
     :event/choices [
       (merge
         (skill-grad skill-id)
         {:choice/events [
            {:description [(i/<< "~{skill-name} [~{difficulty}: ~(if outcome \"Success\" \"Failure\")]")]
             :event/choices [
               {:choice/events [(if outcome success-event failure-event)]}]}]})]}))

(defn active-check
  [choice skill-id skill-name at-least success-event fail-event]
  (let [roll (+ (inc (rand-int 6)) (inc (rand-int 6)))
        outcome (<= at-least roll)]
    (-> choice
      (update :name (fn [n] (i/<< "~{n} [~{skill-name}: ~(difficulty-name at-least)]")))
      (merge (skill-grad skill-id))
      (assoc :choice/events [(if outcome success-event fail-event)]))))

(defn material-event
  [state recipe]
  (log/info (get recipe :recipe/types))
  (let [example-items (rcrafting/get-example-items-by-types (get recipe :recipe/types))
        _ (log/info "example-items" (vec example-items))
        items (take 2 (shuffle example-items))
        item-id (or (first items) :stick)
        amount (+ 1 (rand-int 4))
        item-name (or (if (< 1 amount)
                        (rig/id->name-plural item-id)
                        (rig/id->name item-id))
                      "unknown")
        _ (log/info "items:" items "item-id:" item-id "item-name:" item-name)
        choices (map (fn [item]
                       {:hotkey (get item :hotkey)
                        :name (get item :name)
                        :choice/events [{
                          :title "Weapon Recipe"
                          :description [(i/<< "You take the ~(get item :name). Add this to the weapon?")]
                          :event/choices [
                            {:name "add it"
                             :effects [(rce/mod-dec-inventory (get item :hotkey))]
                             :$/items `(fn [items#] (conj (or items# []) ~item))
                             :choice/events [(let [[skill-id skill-name] (rand-skill-id-name)
                                                   action-word (case skill-id
                                                                 :skill.id/ingenuity "consider how to use the"
                                                                 :skill.id/cunning "think about how to use the"
                                                                 :skill.id/awareness "examine the")]
                                               (passive-check
                                                 (get item :name)
                                                 (i/<< "You ~{action-word} the ~(get item :name). Add this to the weapon?") 
                                                 skill-id
                                                 skill-name
                                                 5
                                                 (rs/rand-storylet
                                                   (rcrafting/assoc-current-recipe
                                                     state
                                                     :check :success
                                                     skill-id :success)
                                                   recipe
                                                   (rs/ns-storylets weapon-gen-ns)
                                                   :none)
                                                 (rs/rand-storylet
                                                   (rcrafting/assoc-current-recipe
                                                     state
                                                     :check :failure
                                                     skill-id :failure)
                                                   recipe
                                                   (rs/ns-storylets weapon-gen-ns)
                                                   :none)))]}
                            {:name "back"
                             :choice/events [:material-event]}]}]})
                  (rcrafting/inventory-crafting-components state))]
    (if (empty? choices)
      {:title "Weapon Recipe"
       :description "You seem to have run out of items. Try again with more different items next time."
       :event/choices [
         {:name "no item - skip"
          :effects [(rce/assoc-current-state-immediate :normal)]
          :abort true}]}
      {:title "Weapon Recipe"
       :description "Use an item from your inventory as a crafting component."
       :event/choices
        (concat
          choices
          [#_{:name "no item - skip"}])})))

(rs/def-storylet default
  rs/any
  {:title "Weapon Recipe"
   :description "At this point you're suprised that worked."
   :event/choices [
     {:choice/events [:check-done]}]})

;; Complications
(rs/def-storylet complication-material
  (rs/and unused-item?
          after-any-failure?)
  (let [item (rand-unused-item state)]
      ; Material complications
      {:event/id :material
       :title (get item :name)
       :description [(i/<< "You fiddle with the ~(get item :name).")]
       :event/choices [
         {:choice/events [
           {:description [(cond
                            (rcrafting/wooden? item) "Crack"
                            (rcrafting/flexible? item) "Snap"
                            :else "Crunch")]
            :event/choices [{
              :name "..."
              :choice/events [
                {:title (get item :name)
                 :description [(i/<< "The ~(get item :name) breaks. It's destroyed.")]
                 :event/choices [{
                   :name "..."
                   :choice/events [
                     {:title (i/<< "Broken ~(get item :name)")
                      :description ["How do you want to fix it?"]
                      :$/complication-items '(fn [items] (conj (or items {}) item))
                      :event/choices [
                        {:hotkey \a
                         :name "Search for something better"
                         :event.complication.item/id (get item :item/id)
                         :choice/events [
                           {:description "You give up after spending hours looking for the right materials."
                            :event/choices [{
                              :effects [(rand-nth [(rce/mod-thirst -10 -5)
                                                   (rce/mod-hunger -10 -5)
                                                   (rce/mod-wtl -10 -5)])]
                              :choice/events [:check-done]}]}
                           {:description "You spend hours looking for the right material."
                            :event/choices [{
                              :effects [(rand-nth [(rce/mod-wtl 5 7)
                                                   (rce/mod-thirst -10 -5)
                                                   (rce/mod-hunger -10 -5)])]
                              :choice/events [:check-done]}]}
                           {:description "After hours of searching, you find the right material."
                            :event/choices [{
                              :effects [(rand-nth [(rce/mod-wtl 5 7)
                                                   (rce/mod-thirst -10 -5)
                                                   (rce/mod-hunger -10 -5)])]
                              :choice/events [:check-done]}]}]}
                        {:hotkey \b
                         :name "Give up"
                         :event.complication.item/id (get item :item/id)
                         :choice/events [
                           {:description "You learn to make do without the right parts."
                            :event/choices [{
                              :effects [(rand-nth [(rce/mod-durability -3 -1)
                                                   (rce/mod-wtl -10 -5)])]
                              :choice/events [:check-done]}]}
                           {:description "You stop and take a break. After a few minutes inspiration strikes."
                            :event/choices [{
                              :effects [(rand-nth [(rce/mod-durability 1 3)
                                                   (rce/mod-wtl 1 5)])]
                              :choice/events [:check-done]}]}]}]}]}]}]}]}]}]}))

      ; Bodily Limit
      #_{:event/id :body-constraint
       :description "You are overexerted. Slow down survivor. Take your time."
       :event/choices [{
         :effects [
           (rce/mod-thirst 11 21)]
         :choice/events [:check-done]}]}

      ; Edged
      #_{:event/id :edged
       :description ["The edge keeps warping when it should be straight."
                     "The edge should be smooth, not full of chips."
                     "The edge you were woking with has rolled causing your to start over."]
       :requirements #{:edged}
       :event/choices [{
         :effects [
           (rce/mod-damage -7 -3)]
         :choice/events [:check-done]}]}

      ; Too much or too little in size, shape, weight
      #_{:event/id :physical-property
       :description ["Too heavy. The balance is all off on this. It really should be lighter."
                     "Too light. You need something heavier to work with. This is too light."
                     "Too long!"
                     "Too short!"
                     "Not tapered enough!"]
       :requirements #{:low-weight :high-weight}
       :event/choices [{
         :effects [
           (rce/mod-thirst 11 21)]
         :choice/events [:check-done]}]}

      ; Handled weapon complications
      #_{:event/id :handle
       :description "You can hardly hold onto this. You need to find something easier to hold."
       :requirements #{:handle}
       :event/choices [{
         :effects [
           (rce/mod-thirst 11 21)]
         :choice/events [:check-done]}]}

      ; Flexible weapon complications
      #_{:event/id :flexibe
       :description "There should be a little more give to this. You know it's going to break."
       :requirements #{:flexible}
       :event/choices [{
         :effects [
           (rce/mod-thirst 11 21)]
         :choice/events [:check-done]}]}

(rs/def-storylet wooden-item-complication
  (rs/and wooden-unused-item?
          after-any-failure?)
  (let [item (-> state wooden-unused-items rand-nth)]
    {:event/id :event.id/wooden
     :title (get item :name)
     :description [(i/<< "The wood making up the ~(get item :name) is bent. It's just not going to work.")
                   (i/<< "The wood making up the ~(get item :name) is green. It should be more seasoned.")
                   (i/<< "The wood making up the ~(get item :name) is full of holes caused by unending seasons of wind and rain.")
                   (i/<< "The wood making up the ~(get item :name) is cracked.")
                   (i/<< "A crack is forming in the wood of the ~(get item :name).")
                   (i/<< "The wood in ~(get item :name) is full of pin knots.")
                   (i/<< "The wood in the ~(get item :name) is too weatherworn to use.")]
     :event/choices [{
       :effects [(rand-nth [(rce/mod-durability -3 -1)
                            (rce/mod-damage -3 -1)
                            (rce/mod-accuracy -3 -1)])]
       :choice/events [{
         :title (i/<< "~(get item :name) wood")
         :description "You need to find a way to fix it."
         :event/choices [{
           :choice/id :choice.id/wooden-find-replacement
           :hotkey \a
           :name "find replacement"
           :choice/events [:check-done]
          }{
           :choice/id :choice.id/wooden-change-design
           :hotkey \b
           :name "change design"
           :choice/events [:check-done]
          }{
           :choice/id :choice.id/wooden-use-anyway
           :hotkey \c
           :name "use anyway"
           :choice/events [:check-done]}]}]}]}))

(rs/def-storylet wooden-find-replacement
  (rs/and (rs/in-history? :choice.id/wooden-find-replacement)
          after-any-failure?
          rs/once)
  ; Wooden - find replacement - complication
  {:event/id :event.id/wooden-find-replacement-complication
   :description ["Your search for a replacement fails. You cannot find the part you need."]
   :event/choices [{
     :effects [(rand-nth [(rce/mod-hunger -10 -5)
                           (rce/mod-thirst -10 -5)])]
     :choice/events [:check-done]
    }]})

(rs/def-storylet wooden-dimension-complication
  (rs/and wooden-unused-item?
          after-any-failure?)
  (let [item (rand-nth (wooden-unused-items state))]
    ; Dimensional
    {:event/id :event.id/dimensional
     :description [(format "The %s you have is too big. You can't get it to work without finding something smaller."
                     (get item :name))
                   (format "The %s you have is too wide It should be more narrow."
                     (get item :name))
                   (format "The  you are trying to use is too thick. Something flatter would work better."
                     (get item :name))]
     :event/choices [{
       :effects [(rand-nth [(rce/mod-durability -3 -1 item)
                            (rce/mod-damage -3 -1 item )
                            (rce/mod-accuracy -3 -1 item)])]
       :choice/events [{
         :description "How do you want to try to to fix it?"
         :event/choices [{
           :choice/id :choice.id/dimensional-find-replacement
           :hotkey \a
           :name "find replacement"
           :choice/events [:check-done]
          }{
           :choice/id :choice.id/dimensional-change-design
           :hotkey \b
           :name "change design"
           :choice/events [:check-done]
          }{
           :choice/id :choice.id/dimensional-use-anyway
           :hotkey \c
           :name "use anyway"
           :choice/events [:check-done]}]}]}]}))

(rs/def-storylet wooden-dimensional-find-replacement
  (rs/and (rs/in-history? :choice.id/dimensional-find-replacement)
          after-any-failure?
          rs/once)
  ; Dimensional - find replacement - complication
  {:event/id :event.id/dimensional-find-replacement-complication
   :description ["Your search for a replacement fails. You cannot find the part you need."]
   :event/choices [{
     :effects [(rand-nth [(rce/mod-hunger -10 -5)
                          (rce/mod-thirst -10 -5)])]
     :choice/events [:check-done]
    }]})

(rs/def-storylet edged-complication
  (rs/and edged-unused-item?
          after-any-failure?)
  ; Edged
  {:event/id :event.id/edged
   :description ["The edge you are working with is chipped. You need a clean edge to move on."
                 "The edge you have is dull. It needs to be sharp."]
   :event/choices [{
     :effects [
       (rce/mod-damage -5 -3)]
     :choice/events [{
       :description "How do you want to try to to fix it?"
       :event/choices [{
         :choice/id :choice.id/edged-find-replacement
         :hotkey \a
         :name "find replacement"
         :choice/events [:check-done]
        }{
         :choice/id :choice.id/edged-change-design
         :hotkey \b
         :name "change design"
         :choice/events [:check-done]
        }{
         :choice/id :choice.id/edged-use-anyway
         :hotkey \c
         :name "use anyway"
         :choice/events [:check-done]}]}]}]})

(rs/def-storylet edged-complication-find-replacement
  (rs/and (rs/in-history? :choice.id/edged-find-replacement)
          after-any-failure?
          rs/once)
      ; Edged - find replacement - complication
      {:event/id :event.id/edged-find-replacement-complication
       :description ["Your search for a replacement edge fails. You cannot find the edged item you need."]
       :event/choices [{
         :effects [(rand-nth [(rce/mod-hunger -10 -5)
                              (rce/mod-thirst -10 -5)])]
         :choice/events [:check-done]
        }]})

(rs/def-storylet flexible-complication
  (rs/and flexible-unused-item?
          after-any-failure?)
  (let [item (rand-nth (flexible-unused-items state))]
    {:event/id :event.id/flexible
     :title (get item :name)
     :description [(i/<< "The ~(get item :name) you are working with is too rigid You need a more supple part to move on.")
                   (i/<< "The ~(get item :name)  you are using is not strong enough under tension. It needs to be stronger.")]
     :event/choices [{
       :effects [
         (rce/mod-damage -5 -3)]
       :choice/events [{
         :title (get item :name)
         :description "How do you want to try to to fix it?"
         :event/choices [{
           :choice/id :choice.id/flexible-find-replacement
           :hotkey \a
           :name "find replacement"
           :choice/events [:flexible-find-replacement]
          }{
           :choice/id :choice.id/flexible-change-design
           :hotkey \b
           :name "change design"
           :choice/events [:check-done]
          }{
           :choice/id :choice.id/flexible-use-anyway
           :hotkey \c
           :name "use anyway"
           :choice/events [:check-done]}]}]}]}))

(defn flexible-find-replacement
  [state recipe item]
  (-> state
    (material-event recipe)
    (assoc :description (i/<< "Find a replace for the ~(get item :name)."))))

(rs/def-storylet flexible-complication-find-replacement
  (rs/and (rs/in-history? :choice.id/flexible-find-replacement)
          after-any-failure?
          rs/once)
      ; Flexible - find replacement - complication
      {:event/id :event.id/flexible-find-replacement-complication
       :description ["Your search for a replacement flexible item fails. You cannot find the flexible item you need."]
       :event/choices [{
         :effects [(rand-nth [(rce/mod-hunger -10 -5)
                              (rce/mod-thirst -10 -5)])]
         :choice/events [:check-done]
        }]})

(rs/def-storylet handled-complication
  (rs/and handled-unused-item?
          after-any-failure?
          rs/once)
  (let [item (rand-nth (handled-unused-items state))]
    {:event/id :event.id/handled
     :title (get item :name)
     :description ["The handle you are designing keeps coming off. You need to find a way to secure it."
                   "The handle you are making has burns. You need to fix it."]
     :event/choices [{
       :effects [
         (rce/mod-damage -5 -3)]
       :choice/events [{
         :description "How do you want to try to to fix it?"
         :event/choices [{
           :choice/id :choice.id/handled-find-replacement
           :hotkey \a
           :name "find replacement"
           :choice/events [:check-done]
          }{
           :choice/id :choice.id/handled-change-design
           :hotkey \b
           :name "change design"
           :choice/events [:check-done]
          }{
           :choice/id :choice.id/handled-use-anyway
           :hotkey \c
           :name "use anyway"
           :choice/events [:check-done]}]}]}]}))

(rs/def-storylet handled-complication-find-replacement
  (rs/and (rs/in-history? :choice.id/handled-find-replacement)
          after-any-failure?
          rs/once)
  ; Handled - find replacement - complication
  {:event/id :event.id/handled-find-replacement-complication
   :description ["Your search for a replacement handled item fails. You cannot find the handled item you need."]
   :event/choices [{
     :effects [(rand-nth [(rce/mod-hunger -10 -5)
                          (rce/mod-thirst -10 -5)])]
     :choice/events [:check-done]
    }]})

#_(rs/def-storylet ranged-complication
  (rs/and ranged-unused-item?
          after-any-failure?
          rs/once)
  (let [item (rand-nth (ranged-unused-items state))]
    {:event/id :event.id/ranged
     :title (get item :name)
     :description ["The prototype isn't accurate enough. You keep missing the target."
                   "The prototype you are making doesn't have enough range. You need to make it reach farther."]
     :event/choices [{
       :effects [
         (rce/mod-damage -5 -3)]
       :choice/events [{
         :description "How do you want to try to to fix it?"
         :event/choices [{
           :choice/id :choice.id/ranged-find-replacement
           :hotkey \a
           :name "find replacement"
           :choice/events [:check-done]
          }{
           :choice/id :choice.id/ranged-change-design
           :hotkey \b
           :name "change design"
           :choice/events [:check-done]
          }{
           :choice/id :choice.id/ranged-use-anyway
           :hotkey \c
           :name "use anyway"
           :choice/events [:check-done]}]}]}]}))

(rs/def-storylet ranged-replacement
  (rs/and (rs/in-history? :choice.id/ranged-find-replacement)
          after-any-failure?
          rs/once)
  ; Ranged - find replacement - complication
  {:event/id :event.id/ranged-find-replacement-complication
   :description ["Your search for a replacement ranged item fails. You cannot find the ranged item you need."]
   :event/choices [{
     :effects [(rand-nth [(rce/mod-hunger -10 -5)
                          (rce/mod-thirst -10 -5)])]
     :choice/events [:check-done]
    }]})


(defn replace-contact
  [old-types new-types]
  (-> old-types
    (clojure.set/difference contact-types)
    (clojure.set/union new-types)))

(defn replace-wield
  [old-types new-types]
  (-> old-types
    (clojure.set/difference wield-types)
    (clojure.set/union new-types)))

(defn merge-contact
  [contact-type]
  (with-meta #{contact-type} {:merge 'replace-contact}))

(defn merge-wield
  [wield-type]
  (with-meta #{wield-type} {:merge 'replace-wield}))

(defn replace
  [old-x new-x]
  new-x)

(defn merge-replace
  [x]
  (with-meta x {:merge 'replace}))

(def contact-choices
  {:blunt
     {:name "Blunt"
      :hotkey \a
      :recipe/types (merge-contact :blunt)}
   :edged
     {:name "Edged"
      :hotkey \b
      :recipe/types (merge-contact :edged)}
   :piercing
     {:name "Piercing"
      :hotkey \c
      :recipe/types (merge-contact :piercing)}
   :flexible
     {:name "Flexible"
      :hotkey \d
      :recipe/types (merge-contact :flexible)}})

(def wield-choices
  {:melee
    {:name "Melee"
     :hotkey \a
     :recipe/types (merge-wield :melee)}
   :thrown
     {:name "Thrown"
      :hotkey \b
      :recipe/types (merge-wield :thrown)}
   :ranged
     {:name "Ranged"
      :hotkey \c
      :recipe/types (merge-wield :ranged)}})

(rs/def-storylet change-type
  (rs/and
    (rs/or (rs/in-history? :choice.id/wooden-change-design)
           (rs/in-history? :choice.id/edged-change-design))
    rs/once)
  ; Change Type
  {:event/id :event.id/change-type
   :description ["The design change took a wrong turn. You find yourself making something completely different."]
   :event/choices (let [types (get recipe :recipe/types)]
     (log/info "change-type" types)
     (map (fn [recipe-type]
       (log/info "recipe-type" recipe-type)
       (cond
         (contact-type? recipe-type)
           (let [contact-type (other-contact-type recipe-type)]
             (get contact-choices contact-type))
         (wield-type? recipe-type)
           (let [wield-type (other-wield-type recipe-type)]
             (get wield-choices wield-type))))
       types))})

#_(rs/def-storylet wooden-handle-complication
  (rs/and handled?
          wooden?
          rs/once)
    ; Wooden handle complications
    {:event/id :wooden-handle
     :description ["Handle burns!"
                   "Handle uneven!"
                   "Handle popping out!"]
     :event/choices [{
       :effects [
         (rce/mod-thirst 11 21)]
       :choice/events [:check-done]}]})

      ; Special Boomerange Complications
      #_{:event/id :boomerang
       :description "Not bent enough!"
       :requirements #{:boomerang}
       :event/choices [{
         :effects [
           (rce/mod-thirst 11 21)]
         :choice/events [:check-done]}]}

      ; Splinter
      #_{:event/id :splinter
       :description ["Ouch! What was that? It's a splinter in your hand."
                     "Oof! A splinter wedged its way into your finger."]
       :requirements #{:wooden}
       :event/choices [{
         :effects [
           (rce/mod-thirst 11 21)]
         :choice/events [:check-done]}]}

      ; Wooden joints
      #_{:event/id :weak-joinery
       :description "Weak joinery!"
       :requirements #{:wooden}
       :event/choices [{
         :effects [
           (rce/mod-thirst 11 21)]
         :choice/events [:check-done]}]}

      ; Poisonous material
      #_{:event/id :allergic
       :description ["Poisonous material!"
                     "Allergic to material!"]
       :requirements #{:wooden}
       :event/choices [{
         :effects [
           (rce/mod-thirst 11 21)]
         :choice/events [:check-done]}]}

      ; Hunger
      {:event/id :hunger
       :description ["You feel so hungry you can't think straight."
                     "Your rumbling stomach distracts you from your task."]
       :requirements #{:player.state/hungry}
       :event/choices [{
         :effects [
           (rce/mod-wtl -10 -5)]
         :choice/events [:check-done]}]}

      ; Thirst
      #_{:event/id :thirst
       :description "Distracted by thirst!"
       :event/choices [{
         :effects [
           (rce/mod-thirst 11 21)]
         :choice/events [:check-done]}]}

      ; Low light
      #_{:event/id :low-light
       :description "Low light!"
       :event/choices [{
         :effects [
           (rce/mod-thirst 11 21)]
         :choice/events [:check-done]}]}

      ; Sound
      #_{:event/id :sound
       :description ["Interrupted by sound!"
                     "Loud work environment!"]
       :event/choices [{
         :effects [
           (rce/mod-thirst 11 21)]
         :choice/events [:check-done]}]}

      ; Empty prototype -----------------
      #_{:title ""
       :description ""
       :event/choices [{
         :effects [
           (rce/mod-thirst 11 21)]
         :choice/events [:check-done]}]}

      ; Spider
      #_{:event/id :spider
       :description "You notice a spider on your leg."
       :event/choices [
         {:hotkey \a
          :name "slap it"
          :choice/events [
            {:description "You pull you hand back and the spider runs up your leg."
             :event/choices [
               {:name "attack"
                :choice/events [
                  {:description "You hit the spider and your thigh."
                   :effects [(rce/mod-hp 5 7)]
                   :choice/events [:check-done]}]}]}
             {:description "Before you can hit it the spider sinks its fangs into your leg."
              :effects [(rce/mod-hp 5 7)]
              :choice/events [:check-done]}
             {:description "You slap the spider. There is not enough meat to eat."}]}
         {:hotkey \b
          :name "brush it off"
          :choice/events [
            {:description "You brush the spider off and it disappears into the ground."}
            {:description "As you brush off the spider thousands of spider babies pour onto your leg"
             :event/choices [
               {:hotkey \a
                :name "just give up and die"
                :effects [(rce/mod-hp 10 50)]
                :choice/events [:check-done]}
               {:hotkey \b
                :name "slap all of them"
                :effects [(rce/mod-hp 5 7)]
                :choice/events [:check-done]}
               {:hotkey \c
                :name "roll on the ground"
                :choice/events [:check-done]}]}]}]}


(defn negative-effect?
  [effect]
  (and (rcmp/normative? effect)
       (neg? (rcmp/utility effect))))

(defn debuffs 
  [recipe]
  (->> recipe
    :effects
    (filter negative-effect?)
    (remove
      (fn [effect]
        (and
          (satisfies? rcmp/ModCause effect)
          (contains?
            (-> recipe :event.complication.item/id)
            (-> effect rcmp/cause :item/id)))))))

(rs/def-storylet remedies
  (rs/and unused-complication?
          after-any-success?)
  ; Find debuffs which have not been remedied
  (let [debuffs (debuffs recipe)]
    (log/info "remedies debuffs" debuffs)
    ; slowdown
    ; push through
    (if (seq debuffs)
      (if-let [debuff (when (seq debuffs) (rand-nth debuffs))]
        [{#_#_:title "Remedy"
          :description "You think you can fix a problem with this item." 
          :event/choices [
            {:hotkey \a
             :name (str "fix " (rcmp/full-name debuff))
             :effects [(rce/mod-remove-effect debuff)]}
            {:name "skip"}]}]
        [])
      [])))

;; Enhancements
(rs/def-storylet accuracy-enhancement
  (rs/and unused-item?
          after-any-success?)
  (let [item (rand-nth (unused-items state))]
    {:title (get item :name)
     :description [(let [verb (rand-nth ["found" "used"])
                         adj  (rand-nth ["great" "perfect" "amazing" "awesome"])]
                     (i/<< "The ~(get item :name) you ~{verb} is ~{adj}."))]
     :event/choices [{
       :choice/events [{
         :title (get item :name)
         :description ["This weapon is going to be a lot more accurate."]
         :effects [(rce/mod-accuracy 1 2)]
         :event/choices [{
           :choice/events [:check-done]}]}]}]}))

(rs/def-storylet damage-enhancement
  (rs/and unused-item?
          after-any-success?)
  (let [item (rand-nth (unused-items state))]
    {:title (get item :name)
     :description [(format "The %s you %s was %s. The weapon is going to do a lot more damage."
                     (:name item)
                     (rand-nth ["found" "used"])
                     (rand-nth ["great" "perfect" "amazing" "awesome"]))]
     :event/choices [{
       :event.complication.item/id (get item :item/id)
       :effects [(rce/mod-damage 1 2)]
       :choice/events [:check-done]}]}))

(rs/def-storylet wooden-find-replacement-buff
  (rs/and (rs/in-history? :choice.id/wooden-find-replacement)
          after-any-success?
          rs/once)
        ; wooden - find replacement
 (let [debuff-mod (rand-nth [
                     (rce/mod-accuracy -5 -1)
                     (rce/mod-damage -6 -2)
                     (rce/mod-durability -6 -2)])]
    {:title "Finding a replacement"
     :description "In a flash of insight, you decide you can cannibalize another item for parts."
     :event/choices
       (let [wooden-inventory (take 2 (shuffle (filter rcrafting/wooden? (ri/player-inventory state))))
             choices (map (fn [item]
                            {:hotkey (get item :hotkey)
                             :name (get item :name)
                             :effects [(rce/mod-dec-inventory (get item :hotkey))]
                             :choice/events [{
                               :description "Select a buff to bring along."
                               :event/choices (let [choices (map (fn [buff hotkey]
                                                                   {:hotkey hotkey
                                                                    :name (rcmp/full-name buff)
                                                                    :effects [buff]})
                                                               (get item :effects))]
                                                (concat choices [{:name "no buffs - skip"}]))}]})
                          wooden-inventory)]
         (if (empty? choices)
           [{:name "no items - skip"
            :effects [debuff-mod]
             :choice/events [:check-done]}]
           choices))}))

(rs/def-storylet wooden-find-replacement-buff
  (rs/and (rs/in-history? :choice.id/wooden-find-replacement)
          after-any-success?
          rs/once)
  ; wooden - find-replacement 
  (let [debuff-mod (rand-nth [
                     (rce/mod-accuracy -5 -1)
                     (rce/mod-damage -6 -2)
                     (rce/mod-durability -6 -2)])]
    {:description "You can turn firre harden this weapon if you find a fire."
     :event/choices [
         {:name "Use fire"
          :hotkey :enter
          :requirements {:adjacent-to-fire true}
          :effects [(rce/mod-firehardened)]
          :choice/events [:check-done]}
         {:name "skip"
          :effects [debuff-mod]
          :choice/events [:check-done]}]}))

(rs/def-storylet wooden-change-design
  (rs/and (rs/in-history? :choice.id/wooden-change-design)
          after-any-success?
          rs/once)
    ; wooden - change design
    {:description "You mull the design in your mind."
     :event/choices [
       {:name "continue"
        :choice/events [{
          :description "Yes, you can change this to include a piercing component."
          :event/choices [{
            :effects [(rce/mod-piercing 0.1 0.3)]
            :choice/events [:check-done]}]}]}]})

  
; dimensional - find-replacement 
#_{:description "You can use parts from another item to fix this."
 :requirements #{:choice.id/dimensional-find-replacement}
 :event/choices
   (let [inventory (take 2 (shuffle (ri/player-inventory state)))
         choices (map (fn [item]
                        {:hotkey (get item :hotkey)
                         :name (get item :name)
                         :effects [(rce/mod-dec-inventory (get item :hotkey))]
                         :choice/events [{
                           :description "Select an effect to copy"
                           :event/choices (let [choices (map (fn [effect hotkey]
                                                               {:hotkey hotkey
                                                                :name (rcmp/full-name effect)
                                                                :effects [effect]
                                                                :choice/events [:check-done]})
                                                           (get item :effects))]
                                            (concat choices [{:name "no buffs - skip"
                                                              :choice/events [:check-done]}]))}]})
                      inventory)]
     (if (empty? choices)
       [{:name "no applicable items - skip"
         :effects [debuff-mod]
         :choice/events [:check-done]}]
       choices))}

(rs/def-storylet change-design
  (rs/and unused-item?
          after-any-success?
          rs/once)
  (let [debuff-mod (rand-nth [
                     (rce/mod-accuracy -5 -1)
                     (rce/mod-damage -6 -2)
                     (rce/mod-durability -6 -2)])
        item (rand-nth (unused-items state))]
    ; dimensional - change design
    {:description "You mull the design in your mind. Examining shapes and interconnections."
     :requirements #{:choice.id/dimensional-change-design}
     :event/choices [
       {:choice/events [{
          :description "Yes, you can change this to make this weapon much more durable."
          :event/choices [{
            :effects [(rce/mod-durability 5 10)]
            :choice/events [:check-done]}]}]}]}))

; edged - find-replacement - cannibalize
#_{:description "You can use parts from another edged item to fix this."
 :requirements #{:choice.id/edged-find-replacement}
 :event/choices
   (let [inventory (take 2 (shuffle (filter rcrafting/edged (ri/player-inventory state))))
         choices (map (fn [item]
                        {:hotkey (get item :hotkey)
                         :name (get item :name)
                         :effects [(rce/mod-dec-inventory (get item :hotkey))]
                         :choice/events [{
                           :description "Select an effect to copy"
                           :event/choices (let [choices (map (fn [effect hotkey]
                                                               {:hotkey hotkey
                                                                :name (rcmp/full-name effect)
                                                                :effects [effect]
                                                                :choice/events [:check-done]})
                                                           (get item :effects))]
                                            (concat choices [{:name "no buffs - skip"
                                                              :choice/events [:check-done]}]))}]})
                      inventory)]
     (if (empty? choices)
       [{:name "no applicable items - skip"
         :effects [debuff-mod]
         :choice/events [:check-done]}]
       choices))}

(rs/def-storylet edged-change-design-dismembering
  (rs/and (rs/in-history? :choice.id/edged-change-design)
          after-any-success?
          rs/once)
  (let [item (rand-nth (unused-items state))]
        ; edged - change design - dismembering
        {:description "You think about how to make the edge sharper."
         :event/choices [
             {:choice/events [{
                :description "In a flash of insight you understand how to make the edge so sharp it can dismember limbs."
                :event/choices [{
                  :effects [(rce/mod-dismembering 0.1 0.5)]
                  :choice/events [:check-done]}]}]}]}))

; flexible - find-replacement - cannibalize
#_{:description "You can use parts from another flexible item to fix this."
 :requirements #{:choice.id/flexible-find-replacement}
 :event/choices
   (let [inventory (take 2 (shuffle (filter rcrafting/flexible (ri/player-inventory state))))
         choices (map (fn [item]
                        {:hotkey (get item :hotkey)
                         :name (get item :name)
                         :effects [(rce/mod-dec-inventory (get item :hotkey))]
                         :choice/events [{
                           :description "Select an effect to copy"
                           :event/choices (let [choices (map (fn [effect hotkey]
                                                               {:hotkey hotkey
                                                                :name (rcmp/full-name effect)
                                                                :effects [effect]
                                                                :choice/events [:check-done]})
                                                           (get item :effects))]
                                            (concat choices [{:name "no buffs - skip"
                                                              :choice/events [:check-done]}]))}]})
                      inventory)]
     (if (empty? choices)
       [{:name "no applicable items - skip"
         :effects [debuff-mod]
         :choice/events [:check-done]}]
       choices))}

(rs/def-storylet flexible-change-design-stunning
  (rs/and (rs/in-history? :choice.id/edged-change-design)
          after-any-success?
          rs/once)
  ; flexible - change design - stunning
  {:description "You think about how to make the flex flexier"
   :event/choices [
       {:choice/events [{
          :description "In a flash of insight you understand how to make it so flexible it can stun opponents."
          :event/choices [{
            :effects [(rce/mod-stunning 0.1 0.5)]
            :choice/events [:check-done]}]}]}]})

; handled - find-replacement - cannibalize
#_{:description "You can use parts from another handled item to fix this."
 :requirements #{:choice.id/handled-find-replacement}
 :event/choices
   (let [inventory (take 2 (shuffle (filter rcrafting/handled? (ri/player-inventory state))))
         choices (map (fn [item]
                        {:hotkey (get item :hotkey)
                         :name (get item :name)
                         :effects [(rce/mod-dec-inventory (get item :hotkey))]
                         :choice/events [{
                           :description "Select an effect to copy"
                           :event/choices (let [choices (map (fn [effect hotkey]
                                                               {:hotkey hotkey
                                                                :name (rcmp/full-name effect)
                                                                :effects [effect]
                                                                :choice/events [:check-done]})
                                                           (get item :effects))]
                                            (concat choices [{:name "no buffs - skip"
                                                              :choice/events [:check-done]}]))}]})
                      inventory)]
     (if (empty? choices)
       [{:name "no applicable items - skip"
         :effects [debuff-mod]
         :choice/events [:check-done]}]
       choices))}

(rs/def-storylet handled-change-design-knockback
  (rs/and (rs/in-history? :choice.id/handled-change-design)
          after-any-success?
          rs/once)
  ; handled - change design - knockback
  {:description "You think about how to improve the handle."
   :requirements #{:choice.id/handled-change-design}
   :event/choices [
       {:choice/events [{
          :description "In a flash of insight you understand how to make the handle easier to grip."
          :event/choices [{
            :effects [(rce/mod-knockback 0.1 0.5)]
            :choice/events [:check-done]}]}]}]})


(rs/def-storylet psychological-event
  after-any-failure?
  ; Mental complications
  {:event/id :mental
   #_#_:title "Emotional Challenge"
   :description ["You tried to make the item work, but in the end it was too frustrating."
                 "As you conintue crafting you start feeling down."
                 "You're feeling depressed. None of this is working out right."]
   :requirements #{}
   :event/choices [{
     :effects [
       (rce/mod-wtl -10 -5)]
     :choice/events [{
       :description "You try to find a way to deal with these feelings."
       :event/choices [{
         :choice/id :choice.id/mental-persevere
         :hotkey \a
         :name "persevere"
         :choice/events [
           {:description ["You hold your head up and clench your fist. You'll make it through this."]
            :event/choices [{
              :choice/events [:check-done]}]}]
        }{
         :choice/id :choice.id/mental-optimism
         :hotkey \b
         :name "be optimistic"
         :choice/events [
           {:description ["You try to push out the negative thoughts in your head. You'll make it through this."]
            :event/choices [{
              :choice/events [:check-done]}]}]
        }{
         :choice/id :choice.id/mental-self-control
         :hotkey \c
         :name "practice self control"
         :choice/events [
           {:description ["*Deep breath* You'll make it through this."]
            :event/choices [{
              :choice/events [:check-done]}]}]}]}]}]})

(rs/def-storylet mental-persevere-success
  (rs/and (rs/in-history? :choice.id/mental-persevere)
          after-any-success?
          rs/once)
  ; mental - perseverence
  {:description "You think about how to much you've struggeled so far and how far you have come."
   :requirements #{:choice.id/handled-change-design}
   :event/choices [
       {:choice/events [{
          :description "You start to feel better about your chances of survival."
          :event/choices [{
            :effects [(rce/mod-wtl 1 5)]
            :choice/events [:check-done]}]}]}]})

(rs/def-storylet mental-event-persevere-failure
  (rs/and (rs/in-history? :choice.id/mental-persevere)
          after-any-failure?
          rs/once)
  ; Mental - persevere - complication
  {:event/id :event.id/mental-persevere-complication
   :description ["You tried to persevere, but you're starting to fray at the edges. You feel burntout."]
   :event/choices [{
     :effects [
       (rce/mod-wtl -10 -5)]
     :choice/events [:check-done]}]})

(rs/def-storylet psychological-event-optimism
  (rs/and (rs/in-history? :choice.id/mental-optimism)
          after-any-failure?)
  ; Mental - optimism - complication
  {:event/id :event.id/mental-optimism-complication
   :description ["You tried to be optimistic, but setback after setback leaves you feeling depressed."]
   :event/choices [{
     :effects [
       (rce/mod-wtl -10 -5)]
     :choice/events [:check-done]}]})

(rs/def-storylet psychological-event-self-control
  (rs/and (rs/in-history? :choice.id/mental-self-control)
          after-any-failure?)
  ; Mental - self-control - complication
  {:event/id :event.id/mental-self-control-complication
   :description ["You tried to keep it together, but you're starting to panic. You feel like the world is closing in."]
   :event/choices [{
     :effects [
       (rce/mod-thirst -10 -5)]
     :choice/events [:check-done]}]})

(defn start
  [state recipe]
  {
    :description (str "You begin crafting a " (name (get recipe :recipe/type)) ". You'll need to start with an item.")
    :event/choices [{
      :choice/events [:check-done]}]})


(defn gen-random [state recipe]
  ; first event? Do start event
  (start state recipe))
