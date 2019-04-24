;; Functions for generating random items.
(ns robinson.materials
  (:require [taoensso.timbre :as log]
            [taoensso.timbre :as log]
            [datascript.core :as d]))

(def material-schema {
  ::isa        {:db/valueType :db.type/ref
                :db/cardinality :db.cardinality/many}
  ::properties {:db/cardinality :db.cardinality/many}})

(def item-schema {
  :item/id {:db/unique :db.unique/identity}
  :item/materials {:db/cardinality :db.cardinality/many
                   :db/type :db.type/ref}
  :item/components {:db/cardinality :db.cardinality/many
                    :db/type :db.type/ref}
  :item/weapon-types {:db.cardinality :db.cardinality/many}})


(def material-heirarchy [
  {:db/ident :solid                               ::properties #{:wieldable}}
  {:db/ident :liquid                              ::properties #{:containable}}
  {:db/ident :container     ::isa #{:solid}}
  {:db/ident :mineral       ::isa #{:solid}}
  {:db/ident :plant-based   ::isa #{:solid}       ::properties #{:flammable}}
  {:db/ident :animal-based  ::isa #{:solid}}
  {:db/ident :wood          ::isa #{:plant-based}}
  {:db/ident :water         ::isa #{:liquid}}
  {:db/ident :lava          ::isa #{:liquid}      ::properties #{:hot}}
  {:db/ident :rock          ::isa #{:solid}}
  {:db/ident :obsidian      ::isa #{:solid}       ::properties #{:brittle}}
  {:db/ident :flint         ::isa #{:mineral}}
  {:db/ident :metal         ::isa #{:solid}}
  {:db/ident :cloth         ::isa #{:plant-based} ::properties #{:flexible}}
  {:db/ident :paper-based   ::isa #{:plant-based} ::properties #{:flexible}}
  {:db/ident :glass         ::isa #{:solid}       ::properties #{:brittle :reflective}}
  {:db/ident :metal                               ::properties #{:mallable}}
  {:db/ident :fruit         ::isa #{:plant-based} ::properties #{:edible}}
  {:db/ident :bone          ::isa #{:animal-based
                                    :solid}}
  {:db/ident :leather       ::isa #{:animal-based} ::properties #{:flexible}}
])

(def db (-> (d/empty-db (merge material-schema item-schema))
            (d/db-with material-heirarchy)))

(def all-properties
  '[[(all-properties ?material ?properties)
       [?material ::properties ?properties]]
    [(all-properties ?material ?properties)
       [?material ::isa ?parents]
       (all-properties ?parents ?properties)]])

(defn -main [& args]
  (let [test-item {:item/id :obsidian-knife :item/materials #{:obsidian :wood}}
        q '[:find ?properties #_?material-ident
            :in $ %
            :where
            [_ :item/materials ?material]
            [?material :db/ident ?material-ident]
            [all-properties ?material ?properties]]]
    ;; execute query: item
    (log/info (->> (d/q q
                       (d/db-with db [test-item])
                       all-properties)
                  (map first)
                  set))))
(-main)
