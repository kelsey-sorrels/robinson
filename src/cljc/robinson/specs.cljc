(ns robinson.specs
  (:require [clojure.spec :as s]))

(s/def ::id keyword?)
(s/def ::xy (s/coll-of integer? :kind vector? :count 2))
(s/def ::x integer?)
(s/def ::y integer?)
(s/def ::pos (s/keys :req-un [::x ::y]))
(s/def ::time (s/and integer? (partial < 25)))
(s/def ::seed (s/and integer? pos?))
(s/def ::width (s/and integer? pos?))
(s/def ::height (s/and integer? pos?))
(s/def ::block-size (s/keys :req-un [::width ::height]))
(s/def ::viewport (s/keys :req-un [::width ::height ::pos]))
(s/def ::cursor ::pos)

;; item specs
(s/def ::fuel (s/and number? pos?))
(s/def ::utility (s/and number? pos?))
(s/def ::item (s/keys :req-un [::id ::name ::name-plural ::fuel ::utility]))
(s/def ::apply-item ::item)

;;
(s/def :cell/type #{:dirt :swamp :tree :tall-grass :short-grass :bamboo :palm-tree :ocean :water :mountain :sand :surf})
(s/def ::items (s/coll-of ::item :kind :vector?))
(s/def ::cell (s/keys :req-un [:cell/type ::items]))
(s/def ::cells (s/coll-of (s/coll-of ::cell :kind vector? :count 80) :kind vector? :count 23))
(s/def ::place (s/keys :req-un [::cells]))
(s/def ::places (s/map-of ::xy ::place))
(s/def ::current-place ::xy)
(s/def ::volcano-pos ::pos)
(s/def ::lava-points (s/coll-of ::xy :kind vector? :gen-max 5))
(s/def ::selected-hotkeys (s/coll-of char? :kind vector? :gen-max 5))
(s/def ::remaining-hotkeys (s/coll-of char? :kind set? :gen-max 5))
(s/def ::log-msg (s/keys :req-un []))
(s/def ::log (s/coll-of ::log-msg :kind vector? :gen-max 3))
(s/def ::log-idx #{0})
(s/def ::c char?)
(s/def ::rgb (s/coll-of (s/and integer? pos?) :kind vector? :count 3))
(s/def ::fg ::rgb)
(s/def ::bg ::rgb)
(s/def ::char (s/keys :req-un [::c ::x ::y ::fg ::bg]))
(s/def ::chars (s/coll-of ::char :gen-max 5))
(s/def ::ui-hint (s/or :no-ui-hint nil? :ui-hint-chars string?))
;;
;; common specs
(s/def ::name string?)
(s/def ::name-plural string?)

;; player specs
(s/def ::inventory (s/coll-of ::item :kind vector? :gen-max 3))
(s/def ::dexterity (s/and integer? pos?))
(s/def ::speed (s/and integer? pos?))
(s/def ::size (s/and integer? pos?))
(s/def ::strength (s/and integer? pos?))
(s/def ::toughness (s/and integer? pos?))
(s/def ::hp (s/and number? pos?))
(s/def :dead/hp number?)
(s/def ::max-hp (s/and integer? pos?))
(s/def ::will-to-live (s/and number? pos?))
(s/def :dead/will-to-live number?)
(s/def ::max-will-to-live (s/and integer? pos?))
(s/def ::xp (s/and integer? pos?))
(s/def ::level (s/and integer? pos?))
(s/def ::hunger (s/and number? pos?))
(s/def :dead/hunger number?)
(s/def ::max-hunger (s/and integer? pos?))
(s/def ::thirst (s/and number? pos?))
(s/def :dead/thirst number?)
(s/def ::max-thirst (s/and integer? pos?))
(s/def ::starting-pos ::pos)
(s/def ::place ::xy)
(s/def ::body-parts (s/coll-of keyword? :kind set? :gen-max 5))
(s/def ::attacks (s/coll-of keyword? :kind set? :gen-max 3))
(s/def ::status (s/coll-of keyword? :kind set? :gen-max 3))
(s/def ::buffs (s/map-of keyword? keyword? :gen-max 2))
(s/def ::stats (s/keys :req-un [])) ;; TODO
(s/def ::wounds (s/map-of keyword? keyword? :gen-max 3))
(s/def ::abilities (s/coll-of keyword? :kind vector? :gen-max 3))
(s/def ::player (s/keys :req-un [::name ::inventory ::dexterity ::speed ::size ::strength ::toughness
                                 ::hp ::max-hp ::will-to-live ::max-will-to-live ::xp ::level ::hunger ::max-hunger
                                 ::thirst ::max-thirst ::pos ::starting-pos ::place ::body-parts ::attacks ::status
                                 ::buffs ::stats ::wounds ::abilities]))
(s/def :dead/player (s/keys :req-un [::name ::inventory ::dexterity ::speed ::size ::strength ::toughness
                                     :dead/hp ::max-hp :dead/will-to-live ::max-will-to-live ::xp ::level :dead/hunger ::max-hunger
                                     :dead/thirst ::max-thirst ::pos ::starting-pos ::place ::body-parts ::attacks ::status
                                     ::buffs ::stats ::wounds ::abilities]))
(s/def ::poisonous boolean?)
(s/def ::skin-identifiable boolean?)
(s/def ::tounge-identifiable boolean?)
(s/def ::identified boolean?)
(s/def ::fruit (s/keys :req-un [::poisonous ::skin-identifiable ::tounge-identifiable ::identified]))
(s/def ::frogs (s/keys :req-un [::poisonous]))
;; npc specs
(s/def ::npc (s/keys req-un [::race ::level ::base-xp ::name ::name-plural ::pos ::hp ::energy ::speed ::size
                             ::strength ::toughness ::body-parts ::attacks ::temperment ::movement-policy
                             ::range-threshold ::status]))
(s/def ::npcs (s/coll-of ::npc :kind vector?))
;; world specs
;; FIXME
(s/def ::dialog-log number?)
(s/def ::quests number?)
(s/def ::normal-like-state
  (s/keys :req-un [::time
                   ::seed
                   ::block-size
                   ::width
                   ::height
                   ::viewport
                   ::places
                   ::current-place
                   ::volcano-pos
                   ::lava-points
                   ::selected-hotkeys
                   ::remaining-hotkeys
                   ::log
                   ::log-idx
                   ::ui-hint
                   ::dialog-log
                   ::player
                   ::fruit
                   ::frogs
                   ::quests
                   ::npcs]))
(defmulti current-state :current-state)
(defmethod current-state :start [_]
  (s/keys :req-un [::time]))
(defmethod current-state :normal [_]
  ::normal-like-state)
(defmethod current-state :direction-select [_]
  ::normal-like-state)
(defmethod current-state :action-select [_]
  ::normal-like-state)
(defmethod current-state :inventory [_]
  ::normal-like-state)
(defmethod current-state :abilities [_]
  ::normal-like-state)
(defmethod current-state :player-stats [_]
  ::normal-like-state)
(defmethod current-state :describe [_]
  ::normal-like-state)
(defmethod current-state :quests [_]
  ::normal-like-state)
(defmethod current-state :describe-inventory [_]
  ::normal-like-state)
(defmethod current-state :drop [_]
  ::normal-like-state)
(defmethod current-state :apply [_]
  ::normal-like-state)
(defmethod current-state :apply-item-normal [_]
  (s/merge ::normal-like-state
    (s/keys :req-un [::apply-item])))
(defmethod current-state :apply-item-inventory [_]
  (s/merge ::normal-like-state
    (s/keys :req-un [::apply-item])))
(defmethod current-state :apply-item-body [_]
  (s/merge ::normal-like-state
    (s/keys :req-un [::apply-item])))
(defmethod current-state :pickup [_]
  ::normal-like-state)
(defmethod current-state :pickup-selection [_]
  ::normal-like-state)
(defmethod current-state :eat [_]
  ::normal-like-state)
(defmethod current-state :quaff-adj-or-inv [_]
  ::normal-like-state)
(defmethod current-state :quaff-adj [_]
  ::normal-like-state)
(defmethod current-state :quaff-popover [_]
  ::normal-like-state)
(defmethod current-state :open [_]
  ::normal-like-state)
(defmethod current-state :talk [_]
  ::normal-like-state)
(defmethod current-state :harvest [_]
  ::normal-like-state)
(defmethod current-state :weild [_]
  ::normal-like-state)
(defmethod current-state :wield-ranged [_]
  ::normal-like-state)
(defmethod current-state :select-ranged-target [_]
  (s/merge ::normal-like-state
           (s/keys :req-un [::cursor])))
(defmethod current-state :throw-inventory [_]
  ::normal-like-state)
(defmethod current-state :select-throw-target [_]
  (s/merge ::normal-like-state
           (s/keys :req-un [::cursor])))
(defmethod current-state :craft [_]
  ::normal-like-state)
(defmethod current-state :craft-weapon [_]
  ::normal-like-state)
(defmethod current-state :craft-survival [_]
  ::normal-like-state)
(defmethod current-state :craft-shelter [_]
  ::normal-like-state)
(defmethod current-state :craft-transportation [_]
  ::normal-like-state)
(defmethod current-state :fishing-left [_]
  ::normal-like-state)
(defmethod current-state :fishing-left [_]
  ::normal-like-state)
(defmethod current-state :fishing-up [_]
  ::normal-like-state)
(defmethod current-state :fishing-down [_]
  ::normal-like-state)
(defmethod current-state :sleep [_]
  ::normal-like-state)
(defmethod current-state :gain-level [_]
  ::normal-like-state)
(defmethod current-state :help-controls [_]
  ::normal-like-state)
(defmethod current-state :help-ui [_]
  ::normal-like-state)
(defmethod current-state :help-gameplay [_]
  ::normal-like-state)
(defmethod current-state :close [_]
  ::normal-like-state)
(defmethod current-state :log [_]
  ::normal-like-state)
(defmethod current-state :popover [_]
  ::normal-like-state)
(defmethod current-state :dead [_]
  (s/keys :req-un [::time
                   ::seed
                   ::block-size
                   ::width
                   ::height
                   ::viewport
                   ::current-place
                   ::volcano-pos
                   ::lava-points
                   ::selected-hotkeys
                   ::remaining-hotkeys
                   ::log
                   ::log-idx
                   ::ui-hint
                   ::dialog-log
                   :dead/player
                   ::fruit
                   ::frogs
                   ::quests
                   ::npcs]))
(defmethod current-state :player-stats [_]
  ::normal-like-state)
(s/def ::world (s/multi-spec current-state :current-state))
(s/def ::version uuid?)
(s/def ::user-id uuid?)
;; TODO make these better specs
(s/def ::atmo (s/coll-of ::rgb :kind vector? :count 100))
(s/def ::data (s/keys :req-un [::atmo]))
(s/def ::settings keyword?)
(s/def ::scores keyword?)
(s/def ::fonts keyword?)
(s/def ::events keyword?)
(s/def ::state (s/keys :req-un [::world
                                ::version
                                ::user-id
                                ::data
                                ::settings
                                ::scores
                                ::fonts
                                ::events]))

