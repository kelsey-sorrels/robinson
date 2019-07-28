(require '[robinson.world :as rw]
         '[taoensso.timbre :as log])

(let [length-of-day (count (get-in *state* [:data :atmo]))]
  (update-in *state* [:world :time]
    (fn [t]
      (- length-of-day (rem t length-of-day)))))
