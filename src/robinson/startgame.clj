;; Functions for helping with start of game
(ns robinson.startgame
  (:use     robinson.common)
  (:require [robinson.monstergen :as mg]
            [robinson.itemgen :as ig]
            [taoensso.timbre :as timbre]
            [pallet.thread-expr :as tx]))

(timbre/refer-timbre)

(defn start-inventory []
  (let [inventory              [(ig/gen-rope 1)
                                (ig/gen-obsidian-axes 1)
                                (ig/gen-obsidian-knives 1)
                                (ig/gen-obsidian-spears 1)
                                (ig/gen-bamboo 10)
                                (ig/gen-rations 2)]
        hotkeys                (vec (seq "abcdefghijklmnopqrstuvwxyzABCdEFGHIJKLMNOPQRSTUVWQYZ"))
        inventory-with-hotkeys (vec (map #(assoc %1 :hotkey %2) inventory hotkeys))]
    inventory-with-hotkeys))

(defn start-text []
  (let [mode-of-transport (rand-nth ["boat" "airplane" "train" "blimp" "hovercraft" "bicycle"])
        natural-disaster  (rand-nth ["hurricane" "tornado" "dark storm"])]
    (format "While traveling by %s, a %s engulfs you.\nYou awake on an island."
             mode-of-transport
             natural-disaster)))
