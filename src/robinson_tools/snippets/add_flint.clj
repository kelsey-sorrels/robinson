(require
  '[robinson.player :as rp]
  '[robinson.itemgen :as ig])

(rp/add-to-inventory state [(ig/gen-item :flint)])
