(require '[robinson.world :as rw])
(rw/assoc-cells *state* (zipmap (for [x (range 80)
                                      y (range 23)]
                                  [x y])
                                (repeat {:discovered (rw/get-time state)})))
