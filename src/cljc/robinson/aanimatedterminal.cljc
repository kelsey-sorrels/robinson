;; Functions for rendering animated state to screen
(ns robinson.aanimatedterminal)

(defprotocol AEffect
  (has-mask? [this])
  (swap-mask! [this f])
  (reset-mask! [this mask])
  (apply-effect! [this terminal]))

(defprotocol AAnimatedTerminal
  (swap-effect-seq! [this f])
  (swap-matching-effect! [this p f])
  (start! [this fps]))

;; namespace with only a protocol gets optimized out, causing missing dependencies.
;; add a dummp def to prevent this ns from being optimized away.
#?(:cljs
(def x 1))
