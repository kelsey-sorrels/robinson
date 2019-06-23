(require
  '[robinson.player :as rp]
  '[robinson.itemgen :as ig]
  '[robinson.monstergen :as mg])

(let [item-ids [:knife
                :club
                :throwing-hammer
                :sling
                :dagger
                :throwing-axe
                :boomerang
                :spear
                :throwing-spear
                :bow
                :blowgun
                :garrote
                :bolas
                :whip
                :rock
                :rock
                :rock
                :rock
                :arrow
                :arrow
                :arrow
                :arrow
                :arrow
                :blowdart
                :blowdart
                :blowdart
                :blowdart
                :blowdart]]
  (rp/add-to-inventory *state*
    (map ig/gen-item item-ids)))