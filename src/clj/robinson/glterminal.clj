;; Functions for rendering state to screen
(ns robinson.glterminal
  (:require ;[robinson.common :as rc :refer [error warn info debug trace]]
            ;[robinson.log :as log]
            [robinson.aterminal :as rat]
            [robinson.math :as rmath]
            [taoensso.timbre :as log]
            [clojure.core.async :as async :refer [go go-loop]]
            [clojure-watch.core :as cwc]
            [clojure.core.async :as async])
  (:import
    (java.lang.reflect Field)
    (java.awt Color
              BorderLayout
              Canvas
              Color
              Font
              FontMetrics
              Graphics
              RenderingHints Toolkit)
    (java.awt.event InputEvent
                    KeyListener
                    KeyEvent)
    (org.lwjgl BufferUtils)
    (java.nio FloatBuffer ByteBuffer)
    (org.lwjgl.opengl Display ContextAttribs
                      PixelFormat DisplayMode
                      GL11 GL12 GL13 GL15 GL20)
    (org.lwjgl.input Keyboard)
    (org.lwjgl.util.vector Matrix4f Vector3f)
    (java.io File)
    (java.awt.image BufferedImage DataBufferByte)
    (robinson.aterminal ATerminal)))

(def char-width 9)
(def char-height 16)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn convert-key-code [e on-key-fn]
  (let [numpad? (= (.getKeyLocation e) KeyEvent/KEY_LOCATION_NUMPAD)]
    (when-let [k (cond
                   (= (.getKeyCode e) KeyEvent/VK_ENTER)         :enter
                   (= (.getKeyCode e) KeyEvent/VK_ESCAPE)        :escape
                   (= (.getKeyCode e) KeyEvent/VK_SPACE)         :space
                   (= (.getKeyCode e) KeyEvent/VK_BACK_SPACE)    :backspace
                   (= (.getKeyCode e) KeyEvent/VK_TAB)           :tab
                   (= (.getKeyCode e) KeyEvent/VK_F1)            :f1
                   (= (.getKeyCode e) KeyEvent/VK_F2)            :f2
                   (= (.getKeyCode e) KeyEvent/VK_F3)            :f3
                   (= (.getKeyCode e) KeyEvent/VK_F4)            :f4
                   (= (.getKeyCode e) KeyEvent/VK_F5)            :f5
                   (= (.getKeyCode e) KeyEvent/VK_F6)            :f6
                   (= (.getKeyCode e) KeyEvent/VK_F7)            :f7
                   (= (.getKeyCode e) KeyEvent/VK_F8)            :f8
                   (= (.getKeyCode e) KeyEvent/VK_F9)            :f9
                   (= (.getKeyCode e) KeyEvent/VK_F10)           :f10
                   (= (.getKeyCode e) KeyEvent/VK_F11)           :f11
                   (= (.getKeyCode e) KeyEvent/VK_F12)           :f12
                   (and numpad?
                        (= (.getKeyCode e) KeyEvent/VK_NUMPAD1)) :numpad1
                   (and numpad?
                        (= (.getKeyCode e) KeyEvent/VK_NUMPAD2)) :numpad2
                   (and numpad?
                        (= (.getKeyCode e) KeyEvent/VK_NUMPAD3)) :numpad3
                   (and numpad?
                        (= (.getKeyCode e) KeyEvent/VK_NUMPAD4)) :numpad4
                   (and numpad?
                        (= (.getKeyCode e) KeyEvent/VK_NUMPAD5)) :numpad5
                   (and numpad?
                        (= (.getKeyCode e) KeyEvent/VK_NUMPAD6)) :numpad6
                   (and numpad?
                        (= (.getKeyCode e) KeyEvent/VK_NUMPAD7)) :numpad7
                   (and numpad?
                        (= (.getKeyCode e) KeyEvent/VK_NUMPAD8)) :numpad8
                   (and numpad?
                        (= (.getKeyCode e) KeyEvent/VK_NUMPAD9)) :numpad9
                   (contains? #{KeyEvent/VK_0
                                KeyEvent/VK_1
                                KeyEvent/VK_2
                                KeyEvent/VK_3
                                KeyEvent/VK_4
                                KeyEvent/VK_5
                                KeyEvent/VK_6
                                KeyEvent/VK_7
                                KeyEvent/VK_8
                                KeyEvent/VK_9}
                              (.getKeyCode e))
                     (char (.getKeyCode e))
                   #_#_true (let [altDown (not= (bit-and (.getModifiersEx e) InputEvent/ALT_DOWN_MASK) 0)
                              ctrlDown (not= (bit-and (.getModifiersEx e) InputEvent/CTRL_DOWN_MASK) 0)]
                          ;(println "processing non-enter non-escape keypress")
                          (when (and altDown ctrlDown (<= \A (.getKeyCode e) \Z))
                            (.toLowerCase (char (.getKeyCode e))))))]

      (log/info "KeyPressed" k e)
      (on-key-fn k))))

(defn make-font
  [name-or-path style size]
  (let [font-file ^File (clojure.java.io/as-file name-or-path)]
    (if (.exists font-file)
      ;; Load font from file
      (.deriveFont (Font/createFont Font/TRUETYPE_FONT font-file) (int style) (float size))
      ;; Load font from font registry
      (Font. name-or-path style size))))

(defn next-pow-2 [v]
  (int (Math/pow 2 (inc (Math/floor (/ (Math/log v) (Math/log 2)))))))

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
  (let [characters-per-line (int (inc (Math/sqrt (count characters))))
        character-matrix    (partition-all characters-per-line characters)]
    (log/info "arranging character with" characters-per-line "per line")
    (mapcat concat
    (map-indexed (fn [row line]
                   (map-indexed (fn [col c-underline?]
                                  [c-underline? col row])
                                line))
                 character-matrix))))

(log/info "character-layout" (str (vec character-idxs)))

;; Map [\character underline?] to [col row]
(def character->col-row
  (reduce (fn [m [c-underline? x y]]
            (assoc m c-underline? [x y]))
          {}
          character-idxs))

(defn make-glyph-image
  [normal-font]
  ;; Adjust canvas to fit character atlas size
  (let [cwidth  (next-pow-2 (* char-width (int (inc (Math/sqrt (count characters))))))
        cheight (next-pow-2 (* char-height (int (inc (Math/sqrt (count characters))))))
        width   (max cwidth cheight)
        height  (max cwidth cheight)
        antialias true]
    (log/info "glyph image width" width "height" height)
    (let [texture-image    (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR)
          texture-graphics ^Graphics (.getGraphics texture-image)
          white            (Color. 255 255 255 255)]
      ;; Create and clear graphics
      (doto texture-graphics
        (.setFont @normal-font)
        (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING (if antialias
                                                                  RenderingHints/VALUE_TEXT_ANTIALIAS_GASP
                                                                  RenderingHints/VALUE_TEXT_ANTIALIAS_OFF))
        ;; set background to black
        (.setColor (Color. 0 0 0 0))
        (.fillRect 0 0 width height))
      ;; Loop through each character, drawing it
      (doseq [[[s underline?] col row]  character-idxs]
        (let [x ^int (* col char-width)
              y ^int (* (inc row) char-height)
              #_#_cx (+ 0 x)
              #_#_cy (+ 12 y)]
          ;(log/info s x y)
        (when (not= s " ")
          ;(println "drawing" s "@" x y "underline?" underline?)
          (doto texture-graphics
            (.setColor white)
            (.drawString (str s) x y)))
        (when underline?
          (let [y (dec y)]
            (doto texture-graphics
              (.setColor white)
              (.drawLine x
                         y
                         (+ x char-width)
                         y))))))
      ;; cleanup texture resource
      ;(ImageIO/write texture-image "jpg", (File. "glyph-texture.jpg"))
      (.dispose texture-graphics)
      {:font-texture-width width
       :font-texture-height height
       :font-texture-image texture-image})))

(defn- texture-id
  ([buffered-image]
  (let [width (.getWidth buffered-image)
        height (.getHeight buffered-image)
        texture-buffer (BufferUtils/createByteBuffer (* width height 4))
        data (-> buffered-image
               (.getRaster)
               (.getDataBuffer)
               (as-> db (cast DataBufferByte db))
               (.getData))]
    (.put texture-buffer data 0 (alength data))
    (.flip texture-buffer)
    (texture-id width height texture-buffer)))
  ([width height]
   (texture-id width height (BufferUtils/createByteBuffer (* width height 4))))
  ([^long width ^long height ^ByteBuffer texture-buffer]
   (let [texture-id (GL11/glGenTextures)]
      ;;(.order texture-buffer (ByteOrder/nativeOrder))
      (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
      (GL11/glPixelStorei GL11/GL_UNPACK_ALIGNMENT 1)
      (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
      (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
      (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA width height 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE texture-buffer)
      texture-id)))

(defn- get-fields [#^Class static-class]
  (. static-class getFields))

(defn- gl-enum-name
  "Takes the numeric value of a gl constant (i.e. GL_LINEAR), and gives the name"
  [enum-value]
  (if (= 0 enum-value)
    "NONE"
    (.getName #^Field (some
                       #(if (= enum-value (.get #^Field % nil)) % nil)
                       (mapcat get-fields [GL11 GL12 GL13 GL15 GL20])))))

(defn- except-gl-errors
  [msg]
  (let [error (GL11/glGetError)
        error-string (str "OpenGL Error(" error "):"
                          (gl-enum-name error) ": " msg)]
    (if (not (zero? error))
      (throw (Exception. error-string)))))

(defn- load-shader
  [^String shader-str ^Integer shader-type]
  (let [shader-id         (GL20/glCreateShader shader-type)
        _ (except-gl-errors "@ load-shader glCreateShader ")
        _                 (GL20/glShaderSource shader-id shader-str)
        _ (except-gl-errors "@ load-shader glShaderSource ")
        _                 (GL20/glCompileShader shader-id)
        _ (except-gl-errors "@ load-shader glCompileShader ")
        gl-compile-status (GL20/glGetShaderi shader-id GL20/GL_COMPILE_STATUS)
        _ (except-gl-errors "@ end of let load-shader")]
    (when (== gl-compile-status GL11/GL_FALSE)
      (println "ERROR: Loading a Shader:")
      (println (GL20/glGetShaderInfoLog shader-id 10000)))
    [gl-compile-status shader-id]))

(defn- init-shaders
  []
  (let [[ok? vs-id] (load-shader (slurp "resources/shader.vs")  GL20/GL_VERTEX_SHADER)
        _           (assert (== ok? GL11/GL_TRUE)) ;; something is really wrong if our vs is bad
        [ok? fs-id] (load-shader (slurp "resources/shader.fs") GL20/GL_FRAGMENT_SHADER)]
    (if (== ok? GL11/GL_TRUE)
      (let [pgm-id                (GL20/glCreateProgram)
            _ (except-gl-errors "@ let init-shaders glCreateProgram")
            _                     (GL20/glAttachShader pgm-id vs-id)
            _ (except-gl-errors "@ let init-shaders glAttachShader VS")
            _                     (GL20/glAttachShader pgm-id fs-id)
            _ (except-gl-errors "@ let init-shaders glAttachShader FS")
            _                     (GL20/glLinkProgram pgm-id)
            _ (except-gl-errors "@ let init-shaders glLinkProgram")
            gl-link-status        (GL20/glGetProgrami pgm-id GL20/GL_LINK_STATUS)
            _ (except-gl-errors "@ let init-shaders glGetProgram link status")
            _                     (when (== gl-link-status GL11/GL_FALSE)
                                    (println "ERROR: Linking Shaders:")
                                    (println (GL20/glGetProgramInfoLog pgm-id 10000)))
            _ (except-gl-errors "@ let before GetUniformLocation")
            ]
        (GL20/glBindAttribLocation pgm-id 0 "aVertexPosition")
        (GL20/glBindAttribLocation pgm-id 1 "aTextureCoord")
        pgm-id)
      (log/error "Error loading shaders"))))

(defn ortho-matrix-buffer [viewport-width viewport-height]
  (let [ortho-matrix (doto (Matrix4f.)
                       (.setIdentity))
        matrix-buffer (BufferUtils/createFloatBuffer 16)
        zNear   10
        zFar   -10
        m00     (/ 2 viewport-width)
        m11     (/ 2 viewport-height)
        m22     (/ -2 (- zFar zNear))
        m23     (/ (- (+ zFar zNear)) (- zFar zNear))
        m33     1]
    (set! (.m00 ortho-matrix) m00)
    (set! (.m11 ortho-matrix) m11)
    (set! (.m22 ortho-matrix) m22)
    (set! (.m23 ortho-matrix) m23)
    (set! (.m33 ortho-matrix) m33)
    (.store ortho-matrix matrix-buffer)
    (.flip matrix-buffer)
    (log/info "Using ortho matrix" ortho-matrix)
    matrix-buffer))

(defn position-matrix-buffer [v]
  (let [matrix (doto (Matrix4f.)
                       (.setIdentity))
        matrix-buffer (BufferUtils/createFloatBuffer 16)]
    (.translate matrix (Vector3f. (get v 0) (get v 1) (get v 2)))
    (.store matrix matrix-buffer)
    (.flip matrix-buffer)
    (log/info "Using position matrix" matrix)
    matrix-buffer))

(defn- init-buffers [vx vy]
  (let [vertices              (float-array [vx   vy  0.0,
                                            0.0, vy  0.0
                                            vx   0.0 0.0
                                            0.0  0.0 0.0,])
        texture-coords        (float-array [1.0 1.0
                                            0.0 1.0
                                            1.0 0.0
                                            0.0 0.0])
        vertices-buffer       (-> (BufferUtils/createFloatBuffer (count vertices))
                                  (.put vertices)
                                  (.flip))
        texture-coords-buffer (-> (BufferUtils/createFloatBuffer (count texture-coords))
                                  (.put texture-coords)
                                  (.flip))
        vertices-count        (count vertices)
        texture-coords-count  (count texture-coords)
        vertices-vbo-id       (GL15/glGenBuffers)
        _                     (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vertices-vbo-id)
        _                     (GL15/glBufferData GL15/GL_ARRAY_BUFFER ^FloatBuffer vertices-buffer GL15/GL_STATIC_DRAW)
        _                     (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false 0 0)
        _                     (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER 0)
        texture-coords-vbo-id (GL15/glGenBuffers)
        _                     (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER texture-coords-vbo-id)
        _                     (GL15/glBufferData GL15/GL_ARRAY_BUFFER ^FloatBuffer texture-coords-buffer GL15/GL_STATIC_DRAW)
        _                     (GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false 0 0)
        _                     (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER 0)]
    (except-gl-errors "end of init-buffers")
    {:vertices-vbo-id vertices-vbo-id
     :vertices-count vertices-count
     :texture-coords-vbo-id texture-coords-vbo-id
     :texture-coords-count texture-coords-count}))

;; Normally this would be a record, but until http://dev.clojure.org/jira/browse/CLJ-1224 is fixed
;; it is not performant to memoize records because hashCode values are not cached and are recalculated
;; each time.
(defn make-terminal-character
  [character fg-color bg-color style]
  {:character character
   :fg-color  fg-color
   :bg-color  bg-color
   :style     style})

(defrecord OpenGlTerminal [^int columns
                           ^int rows
                           ^int texture-columns
                           ^int texture-rows
                           character-map-cleared
                           character-map
                           cursor-xy
                           gl
                           key-chan]
  rat/ATerminal
  (get-size [_]
    [columns rows])
  ;; characters is a list of {:c \character :x col :y row :fg [r g b] :bg [r g b]}
  (put-chars! [_ characters]
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
  (get-key-chan [_]
    key-chan)
  (set-cursor! [_ xy]
    (reset! cursor-xy xy))
  (refresh! [_]
    (GL11/glClearColor 0.0 0.0 1.0 1.0)
    (let [{{:keys [vao-id vertices-vbo-id vertices-count texture-coords-vbo-id]} :buffers
           {:keys [font-texture glyph-texture fg-texture bg-texture]} :textures
           program-id :program-id
           {:keys [u-MVMatrix u-PMatrix u-font u-glyphs u-fg u-bg font-size term-dim font-tex-dim
            font-texture-width font-texture-height glyph-tex-dim glyph-texture-width glyph-texture-height ]} :uniforms
           {:keys [^ByteBuffer glyph-image-data
                   ^ByteBuffer fg-image-data
                   ^ByteBuffer bg-image-data]} :data
           :keys [p-matrix-buffer mv-matrix-buffer]} gl
          glyph-image-data glyph-image-data
          fg-image-data fg-image-data
          bg-image-data bg-image-data]

      (GL20/glUseProgram program-id)
      (GL20/glUniformMatrix4 u-PMatrix false p-matrix-buffer)
      (GL20/glUniformMatrix4 u-MVMatrix false mv-matrix-buffer)
      ; Setup vertex buffer
      (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER, vertices-vbo-id)
      (except-gl-errors (str "vbo bind - glBindBuffer" vertices-vbo-id))
      (GL20/glEnableVertexAttribArray 0);pos-vertex-attribute)
      (except-gl-errors "vbo bind - glEnableVertexAttribArray")
      (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false 0 0)
      (except-gl-errors "vbo bind - glVertexAttribPointer")
      ; Setup uv buffer
      (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER, texture-coords-vbo-id)
      (GL20/glEnableVertexAttribArray 1);texture-coords-vertex-attribute)
      (GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false 0 0)
      (except-gl-errors "texture coords bind")
      ; Setup font texture
      (GL13/glActiveTexture GL13/GL_TEXTURE0)
      (GL11/glBindTexture GL11/GL_TEXTURE_2D font-texture)
      (GL20/glUniform1i u-font, 0)
      (except-gl-errors "font texture bind")
      ; Setup uniforms for glyph, fg, bg, textures
      (GL20/glUniform1i u-glyphs 1)
      (GL20/glUniform1i u-fg 2)
      (GL20/glUniform1i u-bg 3)
      (except-gl-errors "uniformli bind")
      (GL20/glUniform2f font-size, char-width char-height)
      (GL20/glUniform2f term-dim columns rows)
      (GL20/glUniform2f font-tex-dim font-texture-width font-texture-height)
      (GL20/glUniform2f glyph-tex-dim glyph-texture-width glyph-texture-height)
      (except-gl-errors "uniform2f bind")
      (GL11/glDisable GL13/GL_SAMPLE_COVERAGE)
      (GL11/glDisable GL11/GL_BLEND)
      (GL11/glDisable GL11/GL_CULL_FACE)
      (GL11/glDisable GL11/GL_STENCIL_TEST)
      (GL11/glEnable GL11/GL_DITHER)
      (GL11/glDisable GL11/GL_SCISSOR_TEST)
      (GL11/glDisable GL11/GL_DEPTH_TEST)
      (GL11/glDisable GL11/GL_POLYGON_OFFSET_FILL)
      (GL11/glDisable GL13/GL_SAMPLE_ALPHA_TO_COVERAGE)
      (except-gl-errors "gl(en/dis)able")
      #_#_glyph-data       (.-data glyph-image-data)
      #_#_fg-data          (.-data fg-image-data)
      #_#_bg-data          (.-data bg-image-data)
      ;; Update glyph texture in memory
      (.clear glyph-image-data)
      (.clear fg-image-data)
      (.clear bg-image-data)
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
              ;s         (str (get c :character))
              style     (get c :style)
              i         (* 4 (+ (* texture-columns row) col))
              [x y]     (get character->col-row [chr (contains? style :underline)])]
          ;(log/info "Drawing at col row" col row "character from atlas col row" x y c "(index=" i ")")
          (when (zero? col)
            (.position glyph-image-data i)
            (.position fg-image-data i)
            (.position bg-image-data i))
          (.put glyph-image-data (unchecked-byte x))
          (.put glyph-image-data (unchecked-byte y))
          (.put glyph-image-data (unchecked-byte 0))
          (.put glyph-image-data (unchecked-byte 0))
          (.put fg-image-data    (unchecked-byte fg-r))
          (.put fg-image-data    (unchecked-byte fg-g))
          (.put fg-image-data    (unchecked-byte fg-b))
          (.put fg-image-data    (unchecked-byte 0))
          (.put bg-image-data    (unchecked-byte bg-r))
          (.put bg-image-data    (unchecked-byte bg-g))
          (.put bg-image-data    (unchecked-byte bg-b))
          (.put bg-image-data    (unchecked-byte 0))))
      (.position glyph-image-data (.limit glyph-image-data))
      (.position fg-image-data (.limit fg-image-data))
      (.position bg-image-data (.limit bg-image-data))
      (.flip glyph-image-data)
      (.flip fg-image-data)
      (.flip bg-image-data)
      ; Send updated glyph texture to gl
      (GL13/glActiveTexture GL13/GL_TEXTURE1)
      (GL11/glBindTexture GL11/GL_TEXTURE_2D glyph-texture)
      (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA texture-columns texture-rows 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE glyph-image-data)
      (except-gl-errors "glyph texture data")
      ; Send updated fg texture to gl
      (GL13/glActiveTexture GL13/GL_TEXTURE2)
      (GL11/glBindTexture GL11/GL_TEXTURE_2D fg-texture)
      (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA texture-columns texture-rows 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE fg-image-data)
      (except-gl-errors "fg color texture data")
      ; Send updated bg texture to gl
      (GL13/glActiveTexture GL13/GL_TEXTURE3)
      (GL11/glBindTexture GL11/GL_TEXTURE_2D bg-texture)
      (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA texture-columns texture-rows 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE bg-image-data)
      (GL11/glDrawArrays GL11/GL_TRIANGLE_STRIP 0 vertices-count)
      (except-gl-errors "bg color texture data")
      (except-gl-errors "end of refresh")
      ;(Display/sync 60)
      (Display/update)))
  (clear! [_]
    (reset! character-map character-map-cleared)))

(defn make-terminal
  ([]
    (make-terminal 80 24))
  ([columns rows]
   (make-terminal columns rows nil))
  ([columns rows  on-key-fn]
    (make-terminal columns rows on-key-fn "Courier New" "Monospaced" 14))
  ([columns rows on-key-fn windows-font else-font font-size]
    (let [title            "Robinson"
          is-windows       (>= (.. System (getProperty "os.name" "") (toLowerCase) (indexOf "win")) 0)
          normal-font      (atom (if is-windows
                                   (make-font windows-font Font/PLAIN font-size)
                                   (make-font else-font Font/PLAIN font-size)))
          _                    (log/info "Using font" normal-font)
          default-bg-color     [(long 0) (long 0) (long 0)]
          ;; create texture atlas
          character-map-cleared (vec (repeat rows (vec (repeat columns (make-terminal-character \space default-bg-color default-bg-color #{})))))
          character-map         (atom character-map-cleared)
          cursor-xy             (atom nil)

          key-chan         (async/chan)
          on-key-fn        (or on-key-fn
                               (fn default-on-key-fn [k]
                                 (async/put! key-chan k)))
          keyListener      (reify KeyListener
                             (keyPressed [_ e]
                               ;(println "keyPressed keyCode" (.getKeyCode e) "escape" KeyEvent/VK_ESCAPE "escape?" (= (.getKeyCode e) KeyEvent/VK_ESCAPE))
                               (convert-key-code e on-key-fn))
                             (keyReleased [_ _]
                               nil)
                             (keyTyped [_ e]
                               (let [character (.getKeyChar e)
                                     numpad?   (= (.getKeyLocation e) KeyEvent/KEY_LOCATION_NUMPAD)
                                     altDown   (not= (bit-and (.getModifiersEx e) InputEvent/ALT_DOWN_MASK) 0)
                                     ctrlDown  (not= (bit-and (.getModifiersEx e) InputEvent/CTRL_DOWN_MASK) 0)
                                     #_#_ignore    #{(char 10) (char 33) (char 27)}]
                                 (when (and (re-matches #"[!-~]" (str character))
                                            (not (re-matches #"[0-9]" (str character))))
                                   (if ctrlDown
                                     (on-key-fn (char (+ (int \a) -1 (int character))))
                                     (on-key-fn character))))))
          icon            (.getImage (Toolkit/getDefaultToolkit) "images/icon.png")

          terminal-renderer (proxy [Canvas] []
                              (addNotify []
                                (proxy-super addNotify)
                                #_(Display/setParent this)
                                #_(Display/create (PixelFormat.)))
                              (removeNotify []
                                (Display/destroy)
                                (proxy-super removeNotify)))
          font-metrics  ^FontMetrics (.getFontMetrics terminal-renderer @normal-font)
          screen-width               (* columns (.charWidth font-metrics \M))
          screen-height              (* rows (.getHeight font-metrics))
          _                  (log/info "screen size" screen-width "x" screen-height)
          _                  (doto terminal-renderer
                               (.setSize screen-width screen-height)
                               (.setVisible true)
                               (.setIgnoreRepaint true))
          pixel-format       (PixelFormat.)
          context-attributes (ContextAttribs. 2 1)
          _                  (Display/setDisplayMode (DisplayMode. screen-width screen-height))
          _                  (Display/setTitle title)
          _                  (Display/create pixel-format context-attributes)
          _                  (GL11/glViewport 0 0 screen-width screen-height)

          #_#__                (doto (JFrame. title)
                             (.. (getContentPane) (setLayout (BorderLayout.)))
                             (.addKeyListener keyListener)
                             (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                             (.setLocationByPlatform true)
                             (.setLocationRelativeTo nil)
                             (.setAlwaysOnTop true)
                             (.setAlwaysOnTop false)
                             (.setVisible true)
                             (.setIconImage icon)
                             (.setFocusTraversalKeysEnabled false)
                             (.setResizable false)
                             (.setSize screen-width screen-height)
                             (.. (getContentPane) (add terminal-renderer BorderLayout/CENTER))
                             (.pack))

          ;; create texture atlas as gl texture
          {:keys [font-texture-width
                  font-texture-height
                  font-texture-image]} (make-glyph-image normal-font)
          font-texture          (texture-id font-texture-image)
                                                      ;:image character-canvas-dom
                                                      ;:parameters {texture-parameter-name/texture-mag-filter texture-filter/nearest
                                                      ;             texture-parameter-name/texture-min-filter texture-filter/nearest})
          ;; create width*height texture that gets updated each frame that determines which character to draw in each cell
          _ (log/info "Creating glyph array")
          next-pow-2-columns (next-pow-2 columns)
          next-pow-2-rows    (next-pow-2 rows)
          glyph-texture-width  next-pow-2-columns
          glyph-texture-height next-pow-2-rows
          _ (log/info "creating buffers for glyph/fg/bg textures (" next-pow-2-columns "x" next-pow-2-rows ")")
          ;glyph-array    (ta/unsigned-int8 (repeat (* columns rows) 0))
          glyph-image-data (BufferUtils/createByteBuffer (* next-pow-2-columns next-pow-2-rows 4))
          fg-image-data    (BufferUtils/createByteBuffer (* next-pow-2-columns next-pow-2-rows 4))
          bg-image-data    (BufferUtils/createByteBuffer (* next-pow-2-columns next-pow-2-rows 4))
          glyph-texture    (texture-id next-pow-2-columns next-pow-2-rows glyph-image-data)
          fg-texture       (texture-id next-pow-2-columns next-pow-2-rows fg-image-data)
          bg-texture       (texture-id next-pow-2-columns next-pow-2-rows bg-image-data)
          ;; init shaders
          pgm-id ^long (init-shaders)
          pos-vertex-attribute (GL20/glGetAttribLocation pgm-id "aVertexPosition")
          texture-coords-vertex-attribute (GL20/glGetAttribLocation pgm-id "aTextureCoord")

          ;; We just need one vertex buffer, a texture-mapped quad will suffice for drawing the terminal.
          vx                  (* columns char-width)
          vy                  (* rows char-height)

          {:keys [vao-id
                  vertices-vbo-id
                  vertices-count
                  texture-coords-vbo-id]}
                           (init-buffers vx vy)
          u-MVMatrix                (GL20/glGetUniformLocation pgm-id "uMVMatrix")
          u-PMatrix                 (GL20/glGetUniformLocation pgm-id "uPMatrix")
          u-font                    (GL20/glGetUniformLocation pgm-id "uFont")
          u-glyphs                  (GL20/glGetUniformLocation pgm-id "uGlyphs")
          u-fg                      (GL20/glGetUniformLocation pgm-id "uFg")
          u-bg                      (GL20/glGetUniformLocation pgm-id "uBg")
          font-size                 (GL20/glGetUniformLocation pgm-id "fontSize")
          term-dim                  (GL20/glGetUniformLocation pgm-id "termDimensions")
          font-tex-dim              (GL20/glGetUniformLocation pgm-id "fontTextureDimensions")
          glyph-tex-dim             (GL20/glGetUniformLocation pgm-id "glyphTextureDimensions")]
      ;; Start font file change listener thread
      (cwc/start-watch [{:path "./fonts"
                         :event-types [:modify]
                         :bootstrap (fn [path] (println "Starting to watch " path))
                         :callback (fn [_ filename]
                                     (println "Reloading font" filename)
                                     (reset! normal-font
                                             (make-font filename Font/PLAIN font-size)))
                         :options {:recursive true}}])
      ;; Poll keyboard in background thread and offer input to key-chan
      (future (go-loop []
                (Display/processMessages)
                (when (Keyboard/next)
                  (when (Keyboard/getEventKeyState)
                    (async/>! key-chan (Keyboard/getEventCharacter))))
                (recur)))
      ;; Create and return terminal
      (OpenGlTerminal. columns
                       rows
                       next-pow-2-columns
                       next-pow-2-rows
                       character-map-cleared
                       character-map
                       cursor-xy
                       {:p-matrix-buffer (ortho-matrix-buffer screen-width screen-height)
                        :mv-matrix-buffer (position-matrix-buffer [(- (/ screen-width 2)) (- (/ screen-height 2)) -1.0 0.0])
                        :buffers {:vao-id vao-id
                                  :vertices-vbo-id vertices-vbo-id
                                  :vertices-count vertices-count
                                  :texture-coords-vbo-id texture-coords-vbo-id}
                        :textures {:glyph-texture glyph-texture
                                   :font-texture font-texture
                                   :fg-texture fg-texture
                                   :bg-texture bg-texture}
                        :attributes {:pos-vertex-attribute pos-vertex-attribute
                                     :texture-coords-vertex-attribute texture-coords-vertex-attribute}
                        :program-id pgm-id
                        :uniforms {:u-MVMatrix u-MVMatrix
                                   :u-PMatrix u-PMatrix
                                   :u-font u-font
                                   :u-glyphs u-glyphs
                                   :u-fg u-fg
                                   :u-bg u-bg
                                   :font-size font-size
                                   :term-dim term-dim
                                   :font-tex-dim font-tex-dim
                                   :font-texture-width font-texture-width
                                   :font-texture-height font-texture-height
                                   :glyph-tex-dim glyph-tex-dim
                                   :glyph-texture-width glyph-texture-width
                                   :glyph-texture-height glyph-texture-height}
                        :data {:glyph-image-data glyph-image-data
                               :fg-image-data fg-image-data
                               :bg-image-data bg-image-data}}
                       key-chan))))

(defn- put-string
  ([^ATerminal screen x y string]
   (put-string screen (int (rmath/ceil x)) (int (rmath/ceil y)) string [255 255 255] [0 0 0] #{}))
  ([^ATerminal screen x y string fg bg]
   (put-string screen (int (rmath/ceil x)) (int (rmath/ceil y)) string fg bg #{}))
  ([^ATerminal screen x y string fg bg styles]
   (put-string screen (int (rmath/ceil x)) (int (rmath/ceil y)) string fg bg #{} {}))
  ([^ATerminal screen x y string fg bg styles mask-opts]
   {:pre [(clojure.set/superset? #{:underline :bold} styles)]}
   (let [characters (map-indexed (fn [i c] {:c  (str c)
                                            :fg fg
                                            :bg bg
                                            :x  (+ x i)
                                            :y  y
                                            :opts {}})
                                 string)]
     (rat/put-chars! screen characters))))

(defn -main
  "Show a terminal and echo input."
  [& args]
  #_(loop [i 30]
    (log/info "Starting in " i)
    (Thread/sleep 1000)
    (when (pos? i)
      (recur (dec i))))
  (let [terminal (make-terminal 80 24)
        last-key (atom nil)]
    (future (go-loop []
              (reset! last-key (async/<! (rat/get-key-chan terminal)))
              (log/info "got key" @last-key)
              (recur)))

    (loop []
      (when (not (Display/isCloseRequested))
        (let [key-in (or @last-key \?)]
        ;(let [key-in (rat/wait-for-key terminal)]
          (rat/clear! terminal)
          (put-string terminal 0 0 "Hello world 0 0")
          (put-string terminal 0 1 "abcdefghijklmno")
          (put-string terminal 60 0 "Hello world 60 0")
          (put-string terminal 0 19 "Hello world 60 19")
          (put-string terminal 60 19 "Hello world 0 19")
          (put-string terminal 5 10 (str key-in))
          (rat/refresh! terminal)
          (recur)))))
  (Display/destroy)
  (System/exit 0))

