(ns robinson.specs
  (:require [clojure.spec :as s]))

(s/def ::id keyword?)
(s/def ::xy (s/coll-of integer? :kind vector? :count 2))
(s/def ::x integer?)
(s/def ::y integer?)
(s/def ::pos (s/keys :req-un [::x ::y]))
(s/def ::time (s/and integer? pos?))
(s/def ::seed (s/and integer? pos?))
(s/def ::width integer?)
(s/def ::height integer?)
(s/def ::block-size (s/keys :req-un [::width ::height]))
(s/def ::viewport (s/keys :req-un [::width ::height ::pos]))
;; FIXME
(s/def ::place number?)
(s/def ::places (s/map-of ::xy ::place))
(s/def ::current-place ::xy)
(s/def ::volcano-pos ::pos)
(s/def ::lava-points (s/coll-of ::xy :kind vector?))
(s/def ::selected-hotkeys (s/coll-of char? :kind vector?))
(s/def ::remaining-hotkeys (s/coll-of char? :kind set?))
(s/def ::log-msg (s/keys :req-un []))
(s/def ::log (s/coll-of ::log-msg :kind vector?))
;; FIXME
(s/def ::ui-hint number?)
;; common specs
(s/def ::name string?)
(s/def ::name-plural string?)
;; item specs
(s/def ::fuel (s/and number? pos?))
(s/def ::utility (s/and number? pos?))
(s/def ::item (s/keys :req-un [::id ::name ::name-plural ::fuel ::utility]))
;; player specs
(s/def ::inventory (s/coll-of ::item :kind vector?))
(s/def ::dexterity (s/and integer? pos?))
(s/def ::speed (s/and integer? pos?))
(s/def ::size (s/and integer? pos?))
(s/def ::strength (s/and integer? pos?))
(s/def ::toughness (s/and integer? pos?))
(s/def ::hp (s/and integer? pos?))
(s/def ::max-hp (s/and integer? pos?))
(s/def ::will-to-live (s/and integer? pos?))
(s/def ::max-will-to-live (s/and integer? pos?))
(s/def ::xp (s/and integer? pos?))
(s/def ::level (s/and integer? pos?))
(s/def ::hunger (s/and integer? pos?))
(s/def ::max-hunger (s/and integer? pos?))
(s/def ::thirst (s/and integer? pos?))
(s/def ::max-thirst (s/and integer? pos?))
(s/def ::pos (s/and integer? pos?))
(s/def ::starting-pos (s/and integer? pos?))
(s/def ::place (s/and integer? pos?))
(s/def ::body-parts (s/coll-of keyword? :kind set?))
(s/def ::attacks (s/coll-of keyword? :kind set?))
(s/def ::status (s/coll-of keyword? :kind set?))
(s/def ::buffs (s/map-of keyword? keyword?))
(s/def ::stats (s/keys :req-un [])) ;; TODO
(s/def ::wounds (s/map-of keyword? keyword?))
(s/def ::abilities (s/coll-of keyword? :kind vector?))
(s/def ::player (s/keys :req-un [::name ::inventory ::dexterity ::speed ::size ::strength ::toughness
                                 ::hp ::max-hp ::will-to-live ::max-will-to-live ::xp ::level ::hunger ::max-hunger
                                 ::thirst ::max-thirst ::pos ::starting-pos ::place ::body-parts ::attacks ::status
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
(defmulti current-state :current-state)
(defmethod current-state :start [_]
  (s/keys :req-un [::time]))
(defmethod current-state :normal [_]
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
                   ::ui-hint
                   ::dialog-log
                   ::player
                   ::fruit
                   ::frogs
                   ::quests
                   ::npcs]))
(s/def ::world (s/multi-spec current-state :current-state))
(s/def ::version string?)
(s/def ::user-id string?)
;; TODO make these better specs
(s/def ::data keyword?)
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

