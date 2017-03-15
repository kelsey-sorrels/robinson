;; Utility functions for rendering state
(ns robinson.fx
  (:require 
            [robinson.common :as rc]
            [robinson.renderutil :as rutil]
            [robinson.math :as rmath]
            [robinson.color :as rcolor]
            [robinson.world :as rw]
            [taoensso.timbre :as log]
            [zaffre.animation.wrapper :as zaw]))


(defn conj-fx-transform
  [state from to item]
  {:pre [(vector? from)
         (vector? to)
         (= (count from) (count to) 2)]}
  (let [new-state (rc/conj-in state [:fx :transform] {:from-xy from
                                     :to-xy   to
                                     :ch      (rutil/item->char item)
                                     :fg      (rutil/item->fg item)})]
    (log/info "conj-fx-transform" (keys new-state) (get new-state :fx))
    new-state))

(defn clear-fx-transform
  [state]
  (assoc-in state [:fx :transform] []))


(defn conj-fx-blink [state xy]
  (rc/conj-in state [:fx :blink] {:xy xy}))

(defn clear-fx-blink
  [state]
  (assoc-in state [:fx :blink] []))

(defn conj-fx-blip [state xy instants]
  (rc/conj-in state [:fx :blip] {:xy xy :instants instants}))

(defn clear-fx-blip
  [state]
  (assoc-in state [:fx :blip] []))

(defn clear-fx
  [state]
  (assoc state :fx {}))

(defn make-loading-effect
  ([layer-id x y]
    (make-loading-effect layer-id x y [255 255 255] [0 0 0] [\| \/ \- \\]))
  ([layer-id x y fg bg chs]
    (let [dt 100]
      (apply interleave
             (map (fn [c]
                    (repeat [dt
                             {:layer-id layer-id
                              :characters [{:x x :y y :c c :fg fg :bg bg}]}]))
                  chs)))))

(defn silenceable [events enabled-ref]
  (map (fn [event]
         (if @enabled-ref
           event
           (assoc-in event [1 :characters] [])))
       events))

(defn make-name-entry-blink-effect
  [layer-id pos-ref]
  (letfn [(characters [] (if-let [pos @pos-ref]
                           [(merge {:c \u2592 :fg [255 255 255] :bg [0 0 0]}
                                   pos)]
                           []))
          (put-char-args [n]
            (if (> (mod n 30) 15)
              {:layer-id layer-id :characters (characters)}
              {:layer-id layer-id :characters []}))
          (args-stream [n]
              (lazy-seq (cons (put-char-args n) (args-stream (inc n)))))]
    (map vector
         (repeat 11)
         (args-stream 0))))
 
(defn effects-state []
  {:name-entry-pos (atom nil)
   :loading?       (atom false)})

(defn effects [effects-state]
  [(silenceable (make-loading-effect :uifx 40 18) (get effects-state :loading?))
   (make-name-entry-blink-effect :uifx (get effects-state :name-entry-pos))])

(defn update-effects-state! [effects-state state]
  (cond
    (= (rw/current-state state) :loading)
      (do
        (update effects-state :loading? #(reset! % true))
        (update effects-state :name-entry-pos #(reset! % nil)))
    (= (rw/current-state state) :enter-name)
      (do
        (update effects-state :loading? #(reset! % false))
        (update effects-state :name-entry-pos #(reset! % {:x (+ 25 (count (get-in state [:world :player :name])))
                                                          :y 7})))
    :else
      (do
        (update effects-state :loading? #(reset! % false))
        (update effects-state :name-entry-pos #(reset! % nil)))))
  
  

