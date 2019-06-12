(require
  '[robinson.player :as rp]
  '[taoensso.timbre :as log])

(log/info (pr-str (rp/player-inventory *state*)))
*state*
