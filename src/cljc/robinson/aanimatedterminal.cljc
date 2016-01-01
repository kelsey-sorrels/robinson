;; Functions for rendering animated state to screen
(ns robinson.aanimatedterminal)

(defprotocol AEffect
  (id=? [this is])
  (apply-effect! [this terminal]))

(defprotocol AMask
  (swap-mask! [this f])
  (reset-mask! [this mask]))

(defprotocol ACellOpts
  ;; cell opts is a collection of elements, each of which has at least {:x int :y int} keys.
  (set-cell-opts! [this opts]))

(defprotocol APalette
  (update-palette [this f]))

(defprotocol AAnimatedTerminal
  (swap-effect-seq! [this f])
  (swap-matching-effect! [this p f])
  (start! [this fps]))

;; namespace with only a protocol gets optimized out, causing missing dependencies.
;; add a dummp def to prevent this ns from being optimized away.
#?(:cljs
(def x 1))
