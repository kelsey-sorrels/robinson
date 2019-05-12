(ns robinson.ui.mouse
  (:require [taoensso.timbre :as log]))

(defn leaf-elements
  [dom]
  (let [dom-type (get dom :type )]
    (when (contains? #{:view :text :img} dom-type)
        (log/info (update dom :props (fn [props] (dissoc props :children)))))
  (when-let [children (-> dom :props :children)]
    (doseq [child children]
      (leaf-elements child)))))
    
(defn in-element?
  [col row element]
  (let [minx (-> element second :zaffre/layout :x)
        miny (-> element second :zaffre/layout :y)
        width (-> element second :zaffre/layout :width)
        height (-> element second :zaffre/layout :height)
        maxx (+ minx width)
        maxy (+ miny height)]
  (and (<= minx col (inc maxx))
       (<= miny row (inc maxy)))))

(defn handle-click [state col row dom]
  (log/info col row)
  #_(log/info (meta dom))
  (let [layout-elements (-> dom meta :layout-elements)
        hits (filter (partial in-element? col row) (reverse layout-elements))]
    (reduce (fn [state hit]
              (if-let [on-click (-> hit second :on-click)]
                (on-click {:target hit :game-state state})
                state))
            state
            (take 10 hits))))

(defn handle-mouse-move [state col row last-col last-row dom]
  (log/trace col row last-col last-row)
  (let [layout-elements (-> dom meta :layout-elements)
        hits (filter (partial in-element? col row) (reverse layout-elements))
        last-hits (filter (partial in-element? last-col last-row) (reverse layout-elements))
        new-hits (clojure.set/difference (set hits) (set last-hits))
        old-hits (clojure.set/difference (set last-hits) (set hits))]
    (log/trace (count new-hits) "new-hits")
    (log/trace (count old-hits) "old-hits")
    (as-> state state
      ; send on-mouse-enter
      (reduce (fn [state hit]
                (if-let [on-mouse-enter (-> hit second :on-mouse-enter)]
                  (on-mouse-enter {:target hit :game-state state})
                  state))
              state
              (take 10 new-hits))
      ; send-on-mouse-leave
      (reduce (fn [state hit]
                (if-let [on-mouse-leave (-> hit second :on-mouse-leave)]
                  (on-mouse-leave {:target hit :game-state state})
                  state))
              state
              (take 10 old-hits)))))

