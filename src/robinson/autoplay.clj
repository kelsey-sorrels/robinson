(ns robinson.autoplay
  (:require [robinson.common :as rc]
            [robinson.world :as rw]
            [robinson.npc :as rnpc]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [taoensso.timbre :as log]))

(defmulti auto-input
  (fn [state]
    (println (rw/current-state state))
    (rw/current-state state)))

(defmethod auto-input
  :start-inventory
  [_]
  (rand-nth [\a \b \c \d \e \f \g \h \i \j \k :enter]))

(def last-key (atom \l))
(defmethod auto-input
  :normal
  [state]
  (let [adj-npcs (->> state
                   rnpc/npcs-in-viewport
                   (filter (fn [npc] (rw/adjacent-to-player? state (get npc :pos)))))]
    (loop []
      (let [next-key
        ; small chance to not attack monster
        (if (and (< (rand) 0.9)
                 (seq adj-npcs))
          (let [npc (first adj-npcs)
                dir (rc/find-point-relation-ext (rp/player-xy state) (rc/pos->xy (get npc :pos)))
                keyin (case dir
                        :right \l
                        :up-right \u
                        :up \k
                        :up-left \y
                        :left \h
                        :down-left \b
                        :down \j
                        :down-right \n)]
            (log/info "npc-pos" (get npc :pos))
            (log/info "player-pos" (rp/player-pos state))
              keyin)
          (rand-nth [\y \u \h \j \k \l \b \n \. :space
                     \a \e \i \f \z \q \w \W \C \@ \?
                     \t \d \m \/ \* \S \> \< \;]))]
      (if-not (= next-key @last-key)
        (do (reset! last-key next-key)
          next-key)
        (recur))))))

(defmethod auto-input
  :direction-select
  [_]
  (rand-nth [\y \u \h \j \k \l \b \n \.]))

(defmethod auto-input
  :action-select
  [_]
  (rand-nth [\a \b \c \d]))

(defmethod auto-input
  :select-ranged-target
  [_]
  (rand-nth [:escape :tab \n \p \f]))

(defmethod auto-input
  :throw-inventory
  [state]
  (rand-nth (concat [:space :escape] (ri/inventory-hotkeys state))))

(defmethod auto-input
  :inventory
  [_]
  (rand-nth [:escape]))

(defmethod auto-input
  :player-stats
  [_]
  (rand-nth [:escape]))

(defmethod auto-input
  :describe
  [_]
  (rand-nth [\y \u \h \j \k \l \b \n :escape]))

(defmethod auto-input
  :apply
  [state]
  (rand-nth (cons :escape (ri/inventory-hotkeys state))))

(defmethod auto-input
  :apply-item-normal
  [state]
  (rand-nth [\y \u \h \j \k \l \b \n :escape]))

(defmethod auto-input
  :apply-item-inventory
  [state]
  (rand-nth (cons :escape (ri/inventory-hotkeys state))))

(defmethod auto-input
  :apply-item-body
  [state]
  (rand-nth [\a \b :escape]))

(defmethod auto-input
  :apply-item-message
  [state]
  (rand-nth [:enter :escape]))

(defmethod auto-input
  :eat
  [state]
  (rand-nth (cons :escape (ri/inventory-hotkeys state))))

(defmethod auto-input
  :quaff-adj
  [_]
  (rand-nth [\y \u \h \j \k \l \b \n \.]))

(defmethod auto-input
  :quaff-popover
  [_]
  (rand-nth [\y \n]))

(defmethod auto-input
  :recipes
  [_]
  (rand-nth [\a \b \c \n \c :escape]))

(defmethod auto-input
  :select-recipe-type
  [_]
  (rand-nth [\w \b :escape]))

(defmethod auto-input
  :in-progress-recipe
  [_]
  (rand-nth [\a \b \c :space :escape]))

(defmethod auto-input
  :gain-level
  [_]
  (rand-nth [\a \b \c :space :escape]))

(defmethod auto-input
  :help-controls
  [_]
  (rand-nth [\n \p :escape]))

(defmethod auto-input
  :help-ui
  [_]
  (rand-nth [\n \p :escape]))

(defmethod auto-input
  :help-gameplay
  [_]
  (rand-nth [\n \p :escape]))

(defmethod auto-input
  :game-over-dead
  [_]
  \y)

(defmethod auto-input
  :connection-failed
  [_]
  \y)

(defmethod auto-input
  :share-score
  [_]
  \y)

(defmethod auto-input
  :default
  [_]
  :space)
