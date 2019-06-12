(require
  '[robinson.player :as rp]
  '[robinson.itemgen :as ig])

(let [item-ids [:rope
                :stick
                :feather
                :knife
                :rock]]
  (rp/add-to-inventory state
    (map ig/gen-item item-ids)))

