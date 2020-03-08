(ns robinson.specs
  (:require [clojure.spec.alpha :as s]))

(s/def ::width integer?)
(s/def ::height integer?)
(s/def ::x integer?)
(s/def ::y integer?)
(s/def ::pos (s/keys :req-un [::x ::y]))
(s/def ::name string?)
(s/def ::name-plural string?)
(s/def ::hp number?)

(s/def ::race keyword?)
(s/def ::npc
  (s/keys :req-un [::race
                   ::pos
                   ::name
                   ::name-plural
                   ::hp]))

(s/def ::npcs
  (s/coll-of ::npc))

(s/def :item/id keyword?)

(s/def ::hotkey (s/or :char char?
                      :keyword keyword?))

(s/def ::item
  (s/keys :req [:item/id]
          :req-un [::name ::name-plural]
          :opt-un [::hotkey]))

(s/def ::inventory
  (s/coll-of ::item))

(s/def ::player
  (s/keys :opt-un [::inventory ::hp]))

(s/def ::current-state keyword?)

(s/def ::block-size
  (s/keys :req-un [::width ::height]))

(s/def ::viewport
  (s/keys :req-un [::width ::height ::pos]))

(s/def ::time integer?)
(s/def ::seed integer?)

(s/def ::spawned-monsters
  (s/map-of keyword? integer?))

;; cell type - this needs to be :cell/type in the future
(s/def ::type keyword?)

(s/def ::items
  (s/coll-of ::item))

(s/def ::cell
  (s/keys :req-un [::type]
          :opt-un [::items]))

(s/def ::cell-line
  (s/coll-of ::cell))

(s/def ::cells
  (s/coll-of ::cell-line))

(s/def ::place
  (s/keys :req-un [::seed ::pos ::spawned-monsters ::cells]))

(s/def ::places
  (s/map-of (constantly true) ::place))

(s/def :event/id keyword?)
(s/def :choice/id keyword?)
(s/def :recipe/id keyword?)
(s/def ::done boolean?)

(s/def ::description (s/or :string string?
                           :strings (s/coll-of string?)))

(s/def ::event nil)
(s/def :choice/events
  (s/coll-of ::event))

(s/def ::terminal-choice
  (s/keys :req-un [::name ::done]
          :opt [:choice/id]
          :opt-un [::hotkey]))

(s/def ::intersitial-choice
  (s/keys :req [:choice/events]
          :req-un [::name]
          :opt [:choice/id]
          :opt-un [::hotkey]))

(s/def :event/choices
  (s/coll-of (s/or :intersitial-choice ::intersitial-choice
                   :terminal-choice ::terminal-choice)))

(s/def ::event
  (s/or :event-ref keyword?
        :event-literal (s/keys :req [:event/choices]
                               :req-un [::description]
                               :opt [:event/id])))

(s/def ::events
  (s/coll-of ::event))

(s/def ::current-stage ::event)

(s/def :recipe/type keyword?)
(s/def ::recipe
  (s/keys :req [:recipe/type]
          :opt-un [::name
                   ::events
                   ::current-stage]))

(s/def ::selected-recipe-hotkey char?)
(s/def ::recipes
  (s/map-of char? ::recipe))

(s/def ::world
  (s/keys :req-un [::current-state]
          :opt-un [::block-size
                   ::width
                   ::height
                   ::viewport
                   ::places
                   ::time
                   ::player
                   ::npcs
                   ::selected-recipe-hotkey
                   ::recipes]))

(s/def ::state
  (s/keys :req-un [::world]))

#_(defn state?
  [state]
  (let [valid (s/valid? ::state state)]
    (when-not valid
      (s/explain ::state state))
    valid))
