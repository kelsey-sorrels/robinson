(require
  '[robinson.player :as rp]
  '[taoensso.timbre :as log])

(doseq [item (rp/player-inventory *state*)]
  (log/info (pr-str item)))

*state*
