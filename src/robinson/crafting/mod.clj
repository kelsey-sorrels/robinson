(ns robinson.crafting.mod)

(defprotocol Mod
  (mod-name [this])
  (mod-short-name [this])
  (mod-type [this])
  (mod-apply [this item]))

