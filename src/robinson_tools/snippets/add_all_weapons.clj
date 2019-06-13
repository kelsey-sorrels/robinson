(require
  '[robinson.player :as rp]
  '[robinson.itemgen :as ig]
  '[robinson.monstergen :as mg])

(let [item-ids [:rope
                :stick
                :feather
                :knife
                :bamboo
                :rock]
      rat (mg/id->monster :rat)
      rat-corpse (ig/gen-corpse rat)
      rat-bones (ig/gen-bones rat-corpse)]
  (rp/add-to-inventory *state*
    (concat [rat-corpse
             rat-bones]
          (map ig/gen-item item-ids))))

