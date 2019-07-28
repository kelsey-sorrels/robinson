(require
  '[robinson.itemgen :as ig]
  '[robinson.monstergen :as mg])

(let [item-ids [:rope
                :stick
                :feather
                :knife
                :bamboo
                :rock
                :rock
                :rock
                :rock
               ]
      rat (mg/id->monster :rat)
      rat-corpse (ig/gen-corpse rat)
      rat-bones (ig/gen-bones rat-corpse)
      rat-hide (ig/gen-hide rat-corpse)]
  (ri/add-to-inventory *state*
    (concat [rat-corpse
             rat-bones
             rat-hide]
          (map ig/gen-item item-ids))))

