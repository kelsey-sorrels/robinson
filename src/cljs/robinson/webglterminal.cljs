;; Functions for rendering state to screen
(ns robinson.webglterminal
  (:require ;[robinson.common :as rc :refer [error warn info debug trace]]
            [vec3]
            [mat4]
            [robinson.log :as log]
            ;[shodan.console :as log :include-macros true]
            [robinson.aterminal :as rat]
            [cljs.core.async :as async]
            [goog.dom :as dom]
            [goog.events :as events]
            [goog.events.EventType :as event-type]
            [goog.events.KeyCodes :as key-codes]
            [monet.canvas :as canvas]
            [cljs-webgl.context :as context]
            [cljs-webgl.constants.capability :as capability]
            [cljs-webgl.texture :as texture]
            [cljs-webgl.constants.pixel-format :as pixel-format]
            [cljs-webgl.constants.texture-parameter-name :as texture-parameter-name]
            [cljs-webgl.constants.texture-filter :as texture-filter]
            [cljs-webgl.constants.texture-target :as texture-target]
            [cljs-webgl.constants.texture-unit :as texture-unit]
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
(def char-width 9)
(def char-height 16)

(events/listen js/document event-type/KEYPRESS (fn [ev]
  (let [kc (.-keyCode ev)]
    (log/info "Got key" kc)
    (when-let [kc (cond
                    (= kc key-codes/ENTER)      :enter
                    (= kc key-codes/ESC)        :escape
                    (= kc key-codes/SPACE)      :space
                    (= kc key-codes/BACKSPACE)  :backspace
                    (= kc key-codes/NUMPAD1)    :numpad1
                    (= kc key-codes/NUMPAD2)    :numpad2
                    (= kc key-codes/NUMPAD3)    :numpad3
                    (= kc key-codes/NUMPAD4)    :numpad4
                    (= kc key-codes/NUMPAD5)    :numpad5
                    (= kc key-codes/NUMPAD6)    :numpad6
                    (= kc key-codes/NUMPAD7)    :numpad7
                    (= kc key-codes/NUMPAD8)    :numpad8
                    (= kc key-codes/NUMPAD9)    :numpad9
                    true (when (or (<= \a (char kc) \z)
                                   (<= \A (char kc) \Z)
                                   (<= \0 (char kc) \9)
                                   (contains? #{\, \. \? \;} (char kc)))
                           (char kc)))]
      (go
        (async/>! key-chan kc))))))

(defn by-id [id]
  (dom/getElement (name id)))


(defn next-pow-2 [v]
  (int (.pow js/Math 2 (count (.toString v 2)))))

;(timbre/refer-timbre)

;; A sequence of [character underline?]
(def characters
  (let [c (concat
            "abcdefghijklmnopqrstuvwxyz"
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
            "\u2665")
       c (concat (map vector c (repeat false))
                 (map vector c (repeat true)))]
    c))

;; A sequence of [[\character underline?] x y] where [x y] is the column,row in the character atlas.
(def character-idxs
  ;(let [character-matrix (partition-all (int (inc (Math/sqrt (count characters)))) characters)]
  (let [character-matrix (partition-all (int (inc (.sqrt js/Math (count characters)))) characters)]
        (mapcat concat
        (map-indexed (fn [row line]
                       (map-indexed (fn [col c-underline?]
                                      [c-underline? col row])
                                    line))
                     character-matrix))))

(log/info "character-layout" (str character-idxs))

;; Map [\character underline?] to [col row]
(def character->col-row
  (reduce (fn [m [c-underline? x y]]
            (assoc m c-underline? [x y]))
          {}
          character-idxs))

(defn draw-character-canvas!
  [character-canvas-dom character-canvas]
  ;; Adjust canvas to fit character atlas size
  (let [cwidth  (next-pow-2 (* char-width (int (inc (.sqrt js/Math (count characters))))))
        cheight (next-pow-2 (* char-height (int (inc (.sqrt js/Math (count characters))))))
        width   (max cwidth cheight)
        height  (max cwidth cheight)
        ctx     character-canvas]
    (log/info "width" width "height" height)
    (dom/setProperties 
      character-canvas-dom
      (clj->js {:width width
                :height height}))
    (-> ctx
      (canvas/fill-style "#000000")
      (canvas/fill-rect {:x 0 :y 0 :w 600 :h 600})
      (canvas/font-style "15.0px monospace"))
    (doseq [[[c underline?] col row]  character-idxs]
      (let [x  (* col char-width)
            y  (* row char-height)
            cx (+ 0 x)
            cy (+ 12 y)]
        (log/info c x y)
        (-> ctx
          (canvas/fill-style "#ffffff")
          (canvas/save)
          ((fn [ctx]
            (log/info "rect" x y char-width char-height)
            (.rect ctx x y char-width char-height)
            ctx))
          (canvas/clip)
          (canvas/text {:text (str c) :x cx :y cy})
          (as-> ctx
            (if underline?
              (-> ctx
                (canvas/begin-path)
                (canvas/stroke-style :white)
                (canvas/move-to x (+ cy 3.5))
                (canvas/line-to (+ x char-width) (+ cy 3.5))
                (canvas/stroke))
              ctx))
          (canvas/restore))))
    [width height]))

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
    (let [normal-font          "20px Georgia"
          _                    (log/info "Using font" normal-font)
          default-fg-color     [(long default-fg-color-r) (long default-fg-color-g) (long default-fg-color-b)]
          default-bg-color     [(long default-bg-color-g) (long default-bg-color-g) (long default-bg-color-b)]
          ;; create texture atlas
          character-map-cleared (vec (repeat rows (vec (repeat columns (make-terminal-character \space default-fg-color default-bg-color #{})))))
          character-map         (atom character-map-cleared)
          cursor-xy             (atom nil)
          character-canvas-dom  (by-id :character-canvas)
          glyph-canvas-dom      (by-id :glyph-canvas)
          fg-canvas-dom         (by-id :fg-canvas)
          bg-canvas-dom         (by-id :bg-canvas)
          terminal-canvas-dom   (by-id :terminal-canvas)

          character-canvas      (:ctx (canvas/monet-canvas character-canvas-dom "2d"))

          [font-texture-width
           font-texture-height] (draw-character-canvas! character-canvas-dom character-canvas)
          ;; Set glyph canvas and texture to power of 2 dimensions.
          glyph-texture-width  (next-pow-2 (max columns rows))
          glyph-texture-height (next-pow-2 (max columns rows))
          _                    (dom/setProperties 
                                 glyph-canvas-dom
                                 (clj->js {:width  glyph-texture-width
                                           :height glyph-texture-height})) 
          _                    (dom/setProperties 
                                 fg-canvas-dom
                                 (clj->js {:width  glyph-texture-width
                                           :height glyph-texture-height})) 
          _                    (dom/setProperties 
                                 bg-canvas-dom
                                 (clj->js {:width  glyph-texture-width
                                           :height glyph-texture-height})) 
          glyph-canvas         (:ctx (canvas/monet-canvas glyph-canvas-dom "2d"))
          fg-canvas            (:ctx (canvas/monet-canvas fg-canvas-dom "2d"))
          bg-canvas            (:ctx (canvas/monet-canvas bg-canvas-dom "2d"))

          ;; adjust terminal canvas height
          _                (dom/setProperties 
                             terminal-canvas-dom
                             (clj->js {:width  (* columns char-width)
                                       :height (* rows    char-height)}))
          ;; create gl context
          gl                  (context/get-context terminal-canvas-dom)
          ;; create texture atlas as gl texture
          font-texture  (texture/create-texture gl
                                                      :image character-canvas-dom
                                                      :parameters {texture-parameter-name/texture-mag-filter texture-filter/nearest
                                                                   texture-parameter-name/texture-min-filter texture-filter/nearest})
          ;; create width*height texture that gets updated each frame that determines which character to draw in each cell
          _ (log/info "Creating glyph array")
          ;glyph-array    (ta/unsigned-int8 (repeat (* columns rows) 0))
          glyph-image-data (.createImageData glyph-canvas glyph-texture-width glyph-texture-height)
          fg-image-data    (.createImageData fg-canvas    glyph-texture-width glyph-texture-height)
          bg-image-data    (.createImageData bg-canvas    glyph-texture-width glyph-texture-height)
          glyph-texture    (texture/create-texture gl
                                                   :image glyph-image-data
                                                   :parameters {texture-parameter-name/texture-mag-filter texture-filter/nearest
                                                                texture-parameter-name/texture-min-filter texture-filter/nearest})
          fg-texture       (texture/create-texture gl
                                                   :image fg-image-data
                                                   :parameters {texture-parameter-name/texture-mag-filter texture-filter/nearest
                                                                texture-parameter-name/texture-min-filter texture-filter/nearest})
          bg-texture       (texture/create-texture gl
                                                   :image bg-image-data
                                                   :parameters {texture-parameter-name/texture-mag-filter texture-filter/nearest
                                                                texture-parameter-name/texture-min-filter texture-filter/nearest})
          ;; init shaders
          shader-prog         (init-shaders gl)
          ;; We just need one vertex buffer, a texture-mapped quad will suffice for drawing the terminal.
          vx                  (* columns char-width)
          vy                  (* rows char-height)
          square-vertex-buffer
            (buffers/create-buffer gl
              (ta/float32 [vx,  vy,  0.0,
                           0.0, vy,  0.0,
                           vx,  0.0, 0.0,
                           0.0, 0.0, 0.0])
              buffer-object/array-buffer
              buffer-object/static-draw
              3)
          square-texture-buffer
            (buffers/create-buffer gl
              (ta/float32 [1.0 1.0
                           0.0 1.0
                           1.0 0.0
                           0.0 0.0])
              buffer-object/array-buffer
              buffer-object/static-draw
              2)
          vertex-position-attribute (shaders/get-attrib-location gl shader-prog "aVertexPosition")
          texture-coord-attribute   (shaders/get-attrib-location gl shader-prog "aTextureCoord")
          uMVMatrix                 (.getUniformLocation gl shader-prog "uMVMatrix")
          uPMatrix                  (.getUniformLocation gl shader-prog "uPMatrix")
          uFont                     (.getUniformLocation gl shader-prog "uFont")
          uGlyphs                   (.getUniformLocation gl shader-prog "uGlyphs")
          uFg                       (.getUniformLocation gl shader-prog "uFg")
          uBg                       (.getUniformLocation gl shader-prog "uBg")
          font-size                 (.getUniformLocation gl shader-prog "fontSize")
          term-dim                  (.getUniformLocation gl shader-prog "termDimensions")
          font-tex-dim              (.getUniformLocation gl shader-prog "fontTextureDimensions")
          glyph-tex-dim             (.getUniformLocation gl shader-prog "glyphTextureDimensions")]
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
        ;; characters is a list of {:c \character :x col :y row :fg [r g b] :bg [r g b]}
        (put-chars [this characters]
          #_(log/info "characters" (str characters))
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
                                                 character (make-terminal-character (first (get c :c)) fg-color bg-color #{})]
                                             (assoc! line (get c :x) character))
                                           line))
                                     (transient (get cm row))
                                     row-characters)))
                          cm))
                      cm
                      (group-by :y characters))))
            #_(log/info "character-map" (str @character-map)))
        (get-key-chan [this]
          key-chan)
        (set-cursor [this xy]
          (reset! cursor-xy xy))
        (refresh [this]
          (.requestAnimationFrame js/window (fn []
            (buffers/clear-color-buffer gl 0.0 0.0 0.0 1.0)
            (let [_                (.uniformMatrix4fv gl uPMatrix nil (get-ortho-matrix gl))
                  _                (.uniformMatrix4fv gl uMVMatrix, nil, (get-position-matrix [0 0 -1.0]))
                  ; Setup vertex buffer
                  _                (.bindBuffer gl buffer-object/array-buffer, square-vertex-buffer)
                  _                (.enableVertexAttribArray gl 0)
                  _                (.vertexAttribPointer gl 0, 3, data-type/float, false, 0, 0)
                  ; Setup uv buffer
                  _                (.bindBuffer gl buffer-object/array-buffer, square-texture-buffer)
                  _                (.enableVertexAttribArray gl 1)
                  _                (.vertexAttribPointer gl 1, 2, data-type/float, false, 0, 0)
                  ; Setup font texture
                  _                (.activeTexture gl texture-unit/texture0)
                  _                (.bindTexture gl texture-target/texture-2d font-texture)
                  _                (.uniform1i gl uFont, 0)
                  ; Setup uniforms for glyph, fg, bg, textures
                  _                (.uniform1i gl uGlyphs 1)
                  _                (.uniform1i gl uFg 2)
                  _                (.uniform1i gl uBg 3)
                  _                (.uniform2f gl font-size, char-width char-height)
                  _                (.uniform2f gl term-dim columns rows)
                  _                (.uniform2f gl font-tex-dim font-texture-width font-texture-height)
                  _                (.uniform2f gl glyph-tex-dim glyph-texture-width glyph-texture-height)
                  _                (.disable gl capability/sample-coverage)
                  _                (.disable gl capability/blend)
                  _                (.disable gl capability/cull-face)
                  _                (.disable gl capability/stencil-test)
                  _                (.enable gl capability/dither)
                  _                (.disable gl capability/scissor-test)
                  _                (.disable gl capability/depth-test)
                  _                (.disable gl capability/polygon-offset-fill)
                  _                (.disable gl capability/sample-alpha-to-coverage)
                  glyph-data       (.-data glyph-image-data)
                  fg-data          (.-data fg-image-data)
                  bg-data          (.-data bg-image-data)]
                ;; Update glyph texture in memory
                (doseq [row (range rows)
                        col (range columns)]
                   (let [c         (get-in @character-map [row col])
                         chr       (get c :character)
                         highlight (= @cursor-xy [col row])
                         [fg-r fg-g fg-b] (if highlight
                                            (get c :bg-color)
                                            (get c :fg-color))
                         [bg-r bg-g bg-b] (if highlight
                                            (get c :fg-color)
                                            (get c :bg-color))
                         s         (str (get c :character))
                         style     (get c :style)
                         i         (* 4 (+ (* glyph-texture-width row) col))
                         [x y]     (get character->col-row [chr (contains? style :underline)])]
                     ;(log/info "Drawing at col row" col row "character from atlas col row" x y c)
                     (aset glyph-data (+ i 0) x)
                     (aset glyph-data (+ i 1) y)
                     (aset glyph-data (+ i 2) 0)
                     (aset glyph-data (+ i 3) 0)
                     (aset fg-data    (+ i 0) fg-r)
                     (aset fg-data    (+ i 1) fg-g)
                     (aset fg-data    (+ i 2) fg-b)
                     (aset fg-data    (+ i 3) 0)
                     (aset bg-data    (+ i 0) bg-r)
                     (aset bg-data    (+ i 1) bg-g)
                     (aset bg-data    (+ i 2) bg-b)
                     (aset bg-data    (+ i 3) 0)))
                ; Send updated glyph texture to gl
                (.activeTexture gl texture-unit/texture1)
                (.bindTexture gl texture-target/texture-2d glyph-texture)
                (.texImage2D
                  gl
                  texture-target/texture-2d
                  0
                  pixel-format/rgba
                  pixel-format/rgba
                  data-type/unsigned-byte
                  glyph-image-data)
                ; Send updated fg texture to gl
                (.activeTexture gl texture-unit/texture2)
                (.bindTexture gl texture-target/texture-2d fg-texture)
                (.texImage2D
                  gl
                  texture-target/texture-2d
                  0
                  pixel-format/rgba
                  pixel-format/rgba
                  data-type/unsigned-byte
                  fg-image-data)
                ; Send updated bg texture to gl
                (.activeTexture gl texture-unit/texture3)
                (.bindTexture gl texture-target/texture-2d bg-texture)
                (.texImage2D
                  gl
                  texture-target/texture-2d
                  0
                  pixel-format/rgba
                  pixel-format/rgba
                  data-type/unsigned-byte
                  bg-image-data)
                (.drawArrays gl draw-mode/triangle-strip, 0, 4)))))
        (clear [this]
          (reset! character-map character-map-cleared))))))


(defn -main
  "Show a terminal and echo input."
  [& args]
  (let [terminal (make-terminal 80 20)]
    (rat/clear terminal)
    (rat/put-string terminal 5 5 "Hello world")
    (rat/refresh terminal)
    (go-loop []
      (let [key-in (async/<! (rat/get-key-chan terminal))]
      ;(let [key-in (rat/wait-for-key terminal)]
        (log/info "got key" key-in)
        (rat/clear terminal)
        (rat/put-string terminal 5 5 "Hello world")
        (rat/put-string terminal 5 10 (str key-in))
        (rat/refresh terminal))
        (recur))))

