;; Functions for rendering state to screen
(ns robinson.webglterminal
  (:require ;[robinson.common :as rc :refer [error warn info debug trace]]
            [vec3]
            [mat4]
            [robinson.aterminal :as rat]
            [cljs.core.async :as async]
            [goog.dom :as dom]
            [goog.events :as events]
            [goog.events.EventType :as event-type]
            [goog.events.KeyCodes :as key-codes]
            [shodan.console :as log :include-macros true]
            [monet.canvas :as canvas]
            [cljs-webgl.context :as context]
            [cljs-webgl.constants.capability :as capability]
            [cljs-webgl.texture :as texture]
            [cljs-webgl.constants.texture-parameter-name :as texture-parameter-name]
            [cljs-webgl.constants.texture-filter :as texture-filter]
            [cljs-webgl.shaders :as shaders]
            [cljs-webgl.constants.draw-mode :as draw-mode]
            [cljs-webgl.constants.data-type :as data-type]
            [cljs-webgl.constants.buffer-object :as buffer-object]
            [cljs-webgl.constants.shader :as shader]
            [cljs-webgl.constants.webgl :as webgl]
            [cljs-webgl.buffers :as buffers]
            [cljs-webgl.typed-arrays :as ta])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))


(log/info "getting character-canvas")

(def key-chan (async/chan))

(events/listen js/window event-type/KEYPRESS (fn [ev]
  (let [kc (.-keyCode ev)]
    (when-let [kc (cond
                    (= kc key-codes/ENTER)      :enter
                    (= kc key-codes/ESCAPE)     :escape
                    (= kc key-codes/SPACE)      :space
                    (= kc key-codes/BACK_SPACE) :backspace
                    (= kc key-codes/NUMPAD1)    :numpad1
                    (= kc key-codes/NUMPAD2)    :numpad2
                    (= kc key-codes/NUMPAD3)    :numpad3
                    (= kc key-codes/NUMPAD4)    :numpad4
                    (= kc key-codes/NUMPAD5)    :numpad5
                    (= kc key-codes/NUMPAD6)    :numpad6
                    (= kc key-codes/NUMPAD7)    :numpad7
                    (= kc key-codes/NUMPAD8)    :numpad8
                    (= kc key-codes/NUMPAD9)    :numpad9
                    true (when (<= \a (.toLowerCase (char kc)) \z)
                           (.toLowerCase (char kc))))]
      (go
        (async/>! key-chan kc))))))

(defn by-id [id]
  (dom/getElement (name id)))


(defn next-pow-2 [v]
  (int (.pow js/Math 2 (count (.toString v 2)))))

(def character-canvas-dom (by-id :character-canvas))
(def character-canvas (:ctx (canvas/monet-canvas character-canvas-dom "2d")))

(def terminal-canvas-dom (by-id :terminal-canvas))
;(def terminal-canvas (canvas/init terminal-canvas-dom "2d"))

;(timbre/refer-timbre)

(def characters
  (map identity 
       (concat "abcdefghijklmnopqrstuvwxyz"
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
               " ~!@#$%^&*()_-+=[]{}|:;,./<>?'\""
               "\u00A7"
               "\u039B"
               "\u03B1"
               "\u03C2"
               "\u2206"
               "\u2225"
               "\u2240"
               "\u2248"
               "\u2500"
               "\u2502"
               "\u250C"
               "\u2514"
               "\u2518"
               "\u2534"
               "\u2584"
               "\u2592"
               "\u2648"
               "\u2665")))


;; A 2d nested sequence of [\character x y] where [x y] is the position in the character atlas.
(def character-layout
  (let [character-matrix (partition-all (int (inc (.sqrt js/Math (count characters)))) characters)]
        (mapv concat
        (map-indexed (fn [y line]
                       (map-indexed (fn [x c]
                                      [c (* x 12) (* y 16)])
                                    line))
                     character-matrix))))

(log/info "character-layout" (str character-layout))
(log/info "cl" (str (mapcat concat character-layout)))

;; A map from \character to [u0 v0 u1 v1] in the character atlas where
;;    [u0 v0]
;;    +--------+
;;    |        |
;;    |        |
;;    +--------+
;;              [u1 v1]
(def character->uvs
  (reduce (fn [m [c x y]]
            (assoc m c [x y (+ x 12) (+ y 16)]))
          {}
          (mapcat concat character-layout)))

(log/info "character->uvs" (str character->uvs))

(defn draw-character-canvas!
  []
  ;; Adjust canvas to fit character atlas size
  (let [width  (next-pow-2 (* 12 (count (first character-layout))))
        height (next-pow-2 (* 16 (count character-layout)))
        ctx    character-canvas]
    (log/info "width" width "height" height)
    (dom/setProperties 
      character-canvas-dom
      (clj->js {:width width
                :height height}))
    (-> ctx
      (canvas/fill-style "#191d21")
      (canvas/fill-rect {:x 0 :y 0 :w 600 :h 600}))
    (doseq [line character-layout]
      (doseq [[c x y] line]
      (log/info c x y)
      (-> ctx
        (canvas/fill-style "#f9fdf1")
        (canvas/text {:text (str c) :x (+ 2 x) :y (+ 9 y)}))))))

(defn init-shaders [gl]
  (let [fragment-shader (shaders/get-shader gl "shader-fs")
        vertex-shader (shaders/get-shader gl "shader-vs")]
  (shaders/create-program gl fragment-shader vertex-shader)))

(defn get-ortho-matrix [gl]
  (let [viewport-width (context/get-drawing-buffer-width gl)
        viewport-height (context/get-drawing-buffer-height gl)]
    (mat4/ortho
      (mat4/create)
      0
      viewport-width
      viewport-height
      0
      0.1
      100.0)))


(defn get-position-matrix [v]
  (let [m (mat4/create)]
    (mat4/identity m)
    (mat4/translate m m (clj->js v))))

;; Normally this would be a record, but until http://dev.clojure.org/jira/browse/CLJ-1224 is fixed
;; it is not performant to memoize records because hashCode values are not cached and are recalculated
;; each time.
(defn make-terminal-character
  [character fg-color bg-color style]
  {:character character
   :fg-color  fg-color
   :bg-color  bg-color
   :style     style})

(defn make-terminal
  ([]
    (make-terminal 80 24))
  ([columns rows]
    (make-terminal columns rows [255 255 255] [0 0 0]))
  ([columns rows default-fg-color default-bg-color]
    (make-terminal columns rows default-fg-color default-bg-color nil))
  ([columns rows default-fg-color default-bg-color on-key-fn]
    (make-terminal columns rows default-fg-color default-bg-color on-key-fn "Courier New" "Monospaced" 14))
  ([columns rows [default-fg-color-r default-fg-color-g default-fg-color-b]
                 [default-bg-color-r default-bg-color-g default-bg-color-b]
                 on-key-fn
                 windows-font
                 else-font
                 font-size]
    (let [normal-font      "20px Georgia"
          _                (log/info "Using font" normal-font)
          default-fg-color [(long default-fg-color-r) (long default-fg-color-g) (long default-fg-color-b)]
          default-bg-color [(long default-bg-color-g) (long default-bg-color-g) (long default-bg-color-b)]
          ;; create texture atlas
          _                (draw-character-canvas!)
          character-map    (atom (vec (repeat rows (vec (repeat columns (make-terminal-character \space default-fg-color default-bg-color #{}))))))
          cursor-xy        (atom nil)
          ;; adjust terminal canvas height
          _                (dom/setProperties 
                             terminal-canvas-dom
                             (clj->js {:width (* 80 12)
                                       :height (* 26 16)}))
          ;; create gl context
          gl                  (context/get-context terminal-canvas-dom)
          ;; create texture atlas as gl texture
          characters-texture  (texture/create-texture gl
                                                      :image character-canvas-dom
                                                      :parameters {texture-parameter-name/texture-mag-filter texture-filter/nearest
                                                                   texture-parameter-name/texture-min-filter texture-filter/nearest})
          ;; init shaders
          shader-prog         (init-shaders gl)
          ;; We just need one vertex buffer, a texture-mapped quad will suffice for drawing a character.
          square-vertex-buffer
            (buffers/create-buffer gl
              (ta/float32 [12.0, 16.0, 0.0,
                           0.0,  16.0, 0.0,
                           12.0, 0.0,  0.0,
                           0.0,  0.0,  0.0])
              buffer-object/array-buffer
              buffer-object/static-draw
              3)
          ;; We'll need a set of texture coords -- one for each character, but we'll memoize on a 4-tuple of uvs.
          square-texture-buffer-fn (memoize (fn [[u0 v0 u1 v1]]
                                              (let [u0 (/ u0 256)
                                                    v0 (/ v0 256)
                                                    u1 (/ u1 256)
                                                    v1 (/ v1 256)]
                                            (buffers/create-buffer gl
                                              (ta/float32 [u1 v1
                                                           u0 v1
                                                           u1 v0
                                                           u0 v0])
                                              buffer-object/array-buffer
                                              buffer-object/static-draw
                                              2))))
          vertex-position-attribute (shaders/get-attrib-location gl shader-prog "aVertexPosition")
          texture-coord-attribute   (shaders/get-attrib-location gl shader-prog "aTextureCoord")

          ;; Draws character c at position [x y] on the terminal canvas using gl.
          draw-character   (fn [c x y]
                             (let [uvs (character->uvs (get c :character))]
                             ;(log/info "character->pos" (str character->pos))
                             ;(log/info "Drawing character" (str c) "uvs" (str uvs))
                             (buffers/draw!
                               gl
                               :shader shader-prog
                               :draw-mode draw-mode/triangle-strip
                               :count (.-numItems square-vertex-buffer)
                               :attributes [{:buffer square-vertex-buffer :location vertex-position-attribute}
                                            {:buffer (square-texture-buffer-fn uvs) :location texture-coord-attribute}]
                               :uniforms [{:name "uPMatrix" :type :mat4 :values (get-ortho-matrix gl)}
                                          {:name "uMVMatrix" :type :mat4 :values (get-position-matrix [x y -1.0])}]
                               :textures [{:name "uSampler" :texture characters-texture}]))
                             #_(let [glyph-image                       (.createImage component char-width char-height)
                                   offscreen-graphics-2d ^Graphics2D (.getGraphics glyph-image)
                                   x                                 0
                                   y                                 (long (- char-height (.getDescent font-metrics)))
                                   fg-color                          (if highlight
                                                                       (get c :bg-color)
                                                                       (get c :fg-color))
                                   bg-color                          (if  highlight
                                                                       (get c :fg-color)
                                                                       (get c :bg-color))
                                   s                                 (str (get c :character))
                                   style                             (get c :style)]
                               ;(println "filling rect" (* col char-width) (* row char-height) char-width char-height bg-color)
                               (when (not= s " ")
                                 ;(println "drawing" s "@" x y fg-color bg-color)
                                 (doto offscreen-graphics-2d
                                   (.setColor fg-color)
                                   (.drawString s x y)))
                               (when (contains? style :underline)
                                 (let [y (dec  char-height)]
                                   (doto offscreen-graphics-2d
                                     (.setColor fg-color)
                                     (.drawLine 0
                                                y
                                                char-width
                                                y))))))]
      (reify rat/ATerminal
        (get-size [this]
          [columns rows])
        (put-string [this col row string]
          (rat/put-string this col row string [255 255 255] [0 0 0] #{}))
        (put-string [this col row string fg bg]
          (rat/put-string this col row string fg bg #{}))
        (put-string [this col row string fg bg style]
          (when (< -1 row rows)
            (let [fg-color fg
                  bg-color bg
                  s ^String string
                  string-length (.-length s)
                  line           (transient (get @character-map row))]
              (swap! character-map
                (fn [cm]
                  (assoc cm row (persistent!
                                  (reduce
                                    (fn [line [i c]]
                                      (let [x (+ i col)]
                                        (if (< -1 x columns)
                                          (let [character (make-terminal-character c fg-color bg-color style)]
                                            (assoc! line (+ i col) character))
                                          line)))
                                    line
                                    (map-indexed vector s)))))))))
        (put-chars [this characters]
          (swap! character-map
            (fn [cm]
              (reduce (fn [cm [row row-characters]]
                        (if (< -1 row rows)
                          (assoc cm
                                 row
                                 (persistent!
                                   (reduce
                                     (fn [line c]
                                       (if (< -1 (get c :x) columns)
                                           (let [fg        (get c :fg)
                                                 bg        (get c :bg)
                                                 fg-color  fg
                                                 bg-color  bg
                                                 character (make-terminal-character (first (get c :c)) fg-color bg-color {})]
                                             (assoc! line (get c :x) character))
                                           line))
                                     (transient (get cm row))
                                     row-characters)))
                          cm))
                      cm
                      (group-by :y characters)))))
        (wait-for-key [this]
          (go-loop []
            (let [c (async/<! key-chan)]
              (if c
                c
                (recur)))))
        (set-cursor [this xy]
          (reset! cursor-xy xy))
        (refresh [this]
          (.requestAnimationFrame js/window (fn []
            (buffers/clear-color-buffer gl 0.0 0.0 0.0 1.0)
            (doseq [row (range rows)
                    col (range columns)]
              (let [c         (get-in @character-map [row col])
                    x         (long (* col 12))
                    y         (long (* (inc row) 16))
                    highlight (= @cursor-xy [col row])]
                (draw-character c x (- y 16)))))))
        (clear [this]
          (let [c (make-terminal-character \space default-fg-color default-bg-color #{})]
          (doseq [row (range rows)
                  col (range columns)]
            (reset! character-map (assoc-in @character-map [row col] c)))))))))


(defn -main
  "Show a terminal and echo input."
  [& args]
  (let [terminal (make-terminal 80 20)]
    (rat/clear terminal)
    (rat/put-string terminal 5 5 "Hello world")
    (rat/refresh terminal)
    (go-loop []
      (let [key-in (async/<! key-chan)]
      ;(let [key-in (rat/wait-for-key terminal)]
        (log/info "got key" key-in)
        (rat/clear terminal)
        (rat/put-string terminal 5 5 "Hello world")
        (rat/put-string terminal 5 10 (str key-in))
        (rat/refresh terminal))
        (recur))))

(-main)
