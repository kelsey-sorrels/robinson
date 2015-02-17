;; Functions for rendering state to screen
(ns robinson.aterminal)

(defprotocol ATerminal
  (get-size [this])
  (put-string [this x y string]
              [this x y string fg bg]
              [this x y string fg bg style])
  (put-chars [this characters])
  (wait-for-key [this])
  (set-cursor [this xy])
  (refresh [this])
  (clear [this]))
