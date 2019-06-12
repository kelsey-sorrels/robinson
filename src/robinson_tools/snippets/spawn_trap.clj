(let [{:keys [x y]} (rp/player-pos *state*)
      trap {:type :wall-darts-trigger
            :difficulty 1
            :direction :right
            :src-pos {:x (+ x 5) :y y}}]
  (log/info "spawning trap " trap)
  (rw/update-cell *state* x (inc y) (fn [cell] (merge cell trap))))
