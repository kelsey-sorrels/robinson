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
    (java.awt Canvas
              Color
              Font
              FontMetrics
              Graphics
              RenderingHints)
    (org.lwjgl BufferUtils LWJGLUtil)
    (java.nio FloatBuffer ByteBuffer)
    (java.nio.charset Charset)
    (org.lwjgl.opengl Display ContextAttribs
                      PixelFormat DisplayMode Util
                      GL11 GL12 GL13 GL15 GL20 GL30)
    (org.lwjgl.input Keyboard)
    (org.lwjgl.util.vector Matrix4f Vector3f)
    (de.matthiasmann.twl.utils PNGDecoder PNGDecoder$Format)
    (java.io File FileInputStream FileOutputStream)
    (java.awt.image BufferedImage DataBufferByte)
    (javax.imageio ImageIO)
    (robinson.aterminal ATerminal))
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn convert-key-code [event-char event-key on-key-fn]
  ;; Cond instead of case. For an unknown reason, case does not match event-key to Keyboard/* constants.
  ;; Instead it always drops to the default case
  (when-let [key (condp = (int event-key)
                   Keyboard/KEY_RETURN  :enter
                   Keyboard/KEY_ESCAPE  :escape
                   Keyboard/KEY_SPACE   :space
                   (int Keyboard/KEY_BACK)    :backspace
                   Keyboard/KEY_TAB     :tab
                   Keyboard/KEY_F1      :f1
                   Keyboard/KEY_F2      :f2
                   Keyboard/KEY_F3      :f3
                   Keyboard/KEY_F4      :f4
                   Keyboard/KEY_F5      :f5
                   Keyboard/KEY_F6      :f6
                   Keyboard/KEY_F7      :f7
                   Keyboard/KEY_F8      :f8
                   Keyboard/KEY_F9      :f9
                   Keyboard/KEY_F10     :f10
                   Keyboard/KEY_F11     :f11
                   Keyboard/KEY_F12     :f12
                   Keyboard/KEY_UP      :up
                   Keyboard/KEY_DOWN    :down
                   Keyboard/KEY_LEFT    :left
                   Keyboard/KEY_RIGHT   :right
                   Keyboard/KEY_NUMPAD1 :numpad1
                   Keyboard/KEY_NUMPAD2 :numpad2
                   Keyboard/KEY_NUMPAD3 :numpad3
                   Keyboard/KEY_NUMPAD4 :numpad4
                   Keyboard/KEY_NUMPAD5 :numpad5
                   Keyboard/KEY_NUMPAD6 :numpad6
                   Keyboard/KEY_NUMPAD7 :numpad7
                   Keyboard/KEY_NUMPAD8 :numpad8
                   Keyboard/KEY_NUMPAD9 :numpad9
                   ;; event-key didn't match, default to event-char if it is printable, else nil
                   (if (<= (int (first " ")) (int event-char) (int \~))
                     event-char
                     nil))]
    (log/info "key" key)
    (on-key-fn key)))

(defn font-key [font] [(.getName font) (.getSize font)])

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
(defn displayable-characters [font]
  (let [chars (map char (filter (fn [c] (.canDisplay font (char c))) (range 0x0000 0xFFFF)))]
    (concat (map vector chars (repeat false))
            (map vector chars (repeat true)))))

;; A sequence of [[\character underline?] x y] where [x y] is the column,row in the character atlas.
(defn character-idxs
  [characters]
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


;; Map [\character underline?] to [col row]
(def character->col-row
  (memoize (fn [character-idxs]
             (reduce (fn [m [c-underline? x y]]
                       (assoc m c-underline? [x y]))
                     {}
                     character-idxs))))

(defn make-glyph-image
  [char-width char-height font]
  ;; Adjust canvas to fit character atlas size
  (let [characters (displayable-characters font)
        cwidth     (next-pow-2 (* char-width (int (inc (Math/sqrt (count characters))))))
        cheight    (next-pow-2 (* char-height (int (inc (Math/sqrt (count characters))))))
        width      (max cwidth cheight)
        height     (max cwidth cheight)
        antialias  true
        font-metrics  ^FontMetrics (.getFontMetrics (Canvas.) font)]
    (log/info "glyph image width" width "height" height)
    (log/info "characters" (vec characters))
    (log/info "character-idxs" (vec (character-idxs characters)))
    (let [texture-image    (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR)
          texture-graphics ^Graphics (.getGraphics texture-image)
          white            (Color. 255 255 255 255)]
      ;; Create and clear graphics
      (doto texture-graphics
        (.setFont font)
        (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING (if antialias
                                                                  RenderingHints/VALUE_TEXT_ANTIALIAS_GASP
                                                                  RenderingHints/VALUE_TEXT_ANTIALIAS_OFF))
        ;; set background to black
        (.setColor (Color. 0 0 0 0))
        (.fillRect 0 0 width height))
      ;; Loop through each character, drawing it
      (doseq [[[s underline?] col row]  (character-idxs characters)]
        (let [x ^int (* col char-width)
              y ^int (* (inc row) char-height)
              cx (+ 0 x)
              cy (- y (.getDescent font-metrics))]
          ;(log/info s x y)
        (when (not= s " ")
          ;(println "drawing" s "@" x y "underline?" underline?)
          (doto texture-graphics
            (.setColor white)
            (.setClip cx (+ (- cy char-height) (.getDescent font-metrics)) char-width char-height)
            (.drawString (str s) cx cy)))
        (when underline?
          (let [y (dec y)]
            (doto texture-graphics
              (.setColor white)
              (.drawLine x
                         y
                         (+ x char-width)
                         y))))))
      ;; cleanup texture resource
      (ImageIO/write texture-image "jpg", (File. "glyph-texture.jpg"))
      (.dispose texture-graphics)
      {:font-texture-width width
       :font-texture-height height
       :font-texture-image texture-image})))


(defn- buffered-image-byte-buffer [buffered-image]
  (let [width          (.getWidth buffered-image)
        height         (.getHeight buffered-image)
        texture-buffer (BufferUtils/createByteBuffer (* width height 4))
        data (-> buffered-image
                 (.getRaster)
                 (.getDataBuffer)
                 (as-> db (cast DataBufferByte db))
                 (.getData))]
    (.put texture-buffer data 0 (alength data))
    (.flip texture-buffer)
    texture-buffer))

(defn- buffered-image-rgba-byte-buffer [^BufferedImage buffered-image]
  (let [width          (.getWidth buffered-image)
        height         (.getHeight buffered-image)
        texture-buffer ^ByteBuffer (BufferUtils/createByteBuffer (* width height 4))
        channel        (.getChannel (FileOutputStream. (File. (format "bytes-%dx%d.raw.data" width height)) false))]
    (log/info "Gettting bytes for image with type" (.getType buffered-image))
    (doseq [y (range height)
            x (range width)
            :let [abgr   (.getRGB buffered-image x y)
                  g      (unsigned-bit-shift-right (bit-and 0x000000FF abgr) 0)
                  b      (unsigned-bit-shift-right (bit-and 0x0000FF00 abgr) 8)
                  a      (unsigned-bit-shift-right (bit-and 0x00FF0000 abgr) 16)
                  r      (unsigned-bit-shift-right (bit-and 0xFF000000 abgr) 24)
                  i ^int (int (+ (bit-shift-left r 24)
                                 (bit-shift-left g 16)
                                 (bit-shift-left b 8)
                                 (bit-shift-left a 0)))]]
      (.putInt texture-buffer i))
    (.flip texture-buffer)
    (.write channel texture-buffer)
    (.close channel)
    texture-buffer))

(defn- texture-id
  ([buffered-image]
  (let [width (.getWidth buffered-image)
        height (.getHeight buffered-image)]
    (texture-id width height (buffered-image-byte-buffer buffered-image))))
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

(defn- xy-texture-id [^long width ^long height ^ByteBuffer texture-buffer]
  (let [texture-id (GL11/glGenTextures)]
    ;;(.order texture-buffer (ByteOrder/nativeOrder))
    (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL30/GL_RGBA8UI width height 0 GL30/GL_RGBA_INTEGER GL11/GL_INT texture-buffer)
    texture-id))

(defn- get-fields [#^Class static-class]
  (. static-class getFields))

(defn- gl-enum-name
  "Takes the numeric value of a gl constant (i.e. GL_LINEAR), and gives the name"
  [enum-value]
  (if (= 0 enum-value)
    "NONE"
    (.getName #^Field (some
                       #(if (= enum-value (.get #^Field % nil)) % nil)
                       (mapcat get-fields [GL11 GL12 GL13 GL15 GL20 GL30])))))

(defn- except-gl-errors
  [msg]
  (let [error (GL11/glGetError)
        error-string (str "OpenGL Error(" error "):"
                          (gl-enum-name error) ": " msg " - "
                          (Util/translateGLErrorString error))]
    (if (not (zero? error))
      (throw (Exception. error-string)))))

(defn png-bytes [path]
  (let [input-stream (FileInputStream. (str path))
        decoder (PNGDecoder. input-stream)
        width (.getWidth decoder)
        height (.getHeight decoder)
        bytebuf (ByteBuffer/allocateDirect (* width height 4))]
    (.decode decoder bytebuf (* width 4) PNGDecoder$Format/RGBA)
    (.flip bytebuf)
    (.close input-stream)
    bytebuf))

(defn- init-display [title screen-width screen-height]
  (let [pixel-format       (PixelFormat.)
        context-attributes (ContextAttribs. 3 0)
        icon-image-16      (ImageIO/read (File. "images/icon-16x16.png"))
        icon-image-32      (ImageIO/read (File. "images/icon-32x32.png"))
        icon-image-128     (ImageIO/read (File. "images/icon-128x128.png"))
        icon-array         (condp = (LWJGLUtil/getPlatform)
                             LWJGLUtil/PLATFORM_LINUX (let [icon-array (make-array ByteBuffer 1)]
                                                        ;(aset icon-array 0 (buffered-image-rgba-byte-buffer icon-image-32))
                                                        (aset icon-array 0 (png-bytes "images/icon-32x32.png"))
                                                        icon-array)
                             LWJGLUtil/PLATFORM_MACOSX  (let [icon-array (make-array ByteBuffer 1)]
                                                          (aset icon-array 0 (buffered-image-rgba-byte-buffer icon-image-128))
                                                          icon-array)
                             LWJGLUtil/PLATFORM_WINDOWS (let [icon-array (make-array ByteBuffer 2)]
                                                          (aset icon-array 0 (buffered-image-rgba-byte-buffer icon-image-16))
                                                          (aset icon-array 1 (buffered-image-rgba-byte-buffer icon-image-32))
                                                          icon-array))]
     (Display/setDisplayMode (DisplayMode. screen-width screen-height))
     (Display/setTitle title)
     (Display/setIcon icon-array)
     (Display/create pixel-format context-attributes)
     (log/info "byte-buffer" icon-array)
     (GL11/glViewport 0 0 screen-width screen-height)))

(defn- shader-error-str [shader-id]
  (let [infoLogLength (BufferUtils/createIntBuffer 1)
        _             (GL20/glGetShader shader-id GL20/GL_INFO_LOG_LENGTH, infoLogLength)
        log-length    (.get infoLogLength 0)
        infoLog ^ByteBuffer        (BufferUtils/createByteBuffer log-length)
        _              (.clear infoLogLength)
        _              (GL20/glGetShaderInfoLog shader-id, infoLogLength, infoLog)
        infoLogBytes   (byte-array log-length)
        _              (.get infoLog infoLogBytes, 0, log-length)]
    (log/info "info length" log-length)
    (String. infoLogBytes (Charset/forName "UTF-8"))))

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

(defn ortho-matrix-buffer
  ([viewport-width viewport-height]
    (ortho-matrix-buffer viewport-width viewport-height (BufferUtils/createFloatBuffer 16)))
  ([viewport-width viewport-height matrix-buffer]
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
      matrix-buffer)))

(defn position-matrix-buffer
  ([v s]
   (position-matrix-buffer v s (BufferUtils/createFloatBuffer 16)))
  ([v s matrix-buffer]
    (let [matrix (doto (Matrix4f.)
                         (.setIdentity))]
      (.translate matrix (Vector3f. (get v 0) (get v 1) (get v 2)))
      (.scale matrix (Vector3f. (get s 0) (get s 1) (get s 2)))
      (.store matrix matrix-buffer)
      (.flip matrix-buffer)
      matrix-buffer)))

(defn- init-buffers []
  (let [vertices              (float-array [1.0   1.0  0.0,
                                            0.0   1.0  0.0
                                            0.0,  0.0 0.0
                                            1.0   0.0 0.0])
        texture-coords        (float-array [1.0 1.0
                                            0.0 1.0
                                            0.0 0.0
                                            1.0 0.0])
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

(defrecord GLCharacter [character fg-color bg-color style fx-character fx-fg-color fg-bg-color]
  Object
  (toString [this]
    (pr-str this)))

(defn make-terminal-character
  ([character fg-color bg-color style]
   (make-terminal-character character fg-color bg-color style nil nil nil))
  ([character fg-color bg-color style fx-character fx-fg-color fg-bg-color]
   (GLCharacter. character fg-color bg-color style fx-character fx-fg-color fg-bg-color)))

(defrecord OpenGlTerminal [^int columns
                           ^int rows
                           ^int texture-columns
                           ^int texture-rows
                           font-textures
                           normal-font
                           antialias
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
    (alter character-map
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
  (set-fg! [_ x y fg]
    {:pre [(vector? fg)
           (= (count fg) 3)]}
      (alter character-map
             (fn [cm] (assoc-in cm [y x :fg-color] fg))))
  (set-bg! [_ x y bg]
    {:pre [(vector? bg)
           (= (count bg) 3)]}
      (alter character-map
             (fn [cm] (assoc-in cm [y x :bg-color] bg))))
  (get-key-chan [_]
    key-chan)
  (apply-font! [this windows-font else-font size smooth]
    (locking this
      (reset! normal-font
              (if (= (LWJGLUtil/getPlatform) LWJGLUtil/PLATFORM_WINDOWS)
                (make-font windows-font Font/PLAIN size)
                (make-font else-font Font/PLAIN size)))
      (let [{:keys [screen-width
                    screen-height
                    character-width
                    character-height
                    font-texture-width
                    font-texture-height
                    font-texture-image]} (get @font-textures (font-key @normal-font))]
      (log/info "screen size" screen-width "x" screen-height)
        (Display/makeCurrent)
        (Display/setDisplayMode (DisplayMode. screen-width screen-height))
        (swap! font-textures update (font-key @normal-font) (fn [m] (assoc m :font-texture (texture-id font-texture-image))))
        (Display/releaseContext))
      (reset! antialias smooth))
      ;TODO: resize screen (and bind texture?)
      )
  (set-cursor! [_ xy]
    (reset! cursor-xy xy))
  (refresh! [this]
    (locking this
      (let [{{:keys [vertices-vbo-id vertices-count texture-coords-vbo-id]} :buffers
             {:keys [font-texture glyph-texture fg-texture bg-texture]} :textures
             program-id :program-id
             {:keys [u-MVMatrix u-PMatrix u-font u-glyphs u-fg u-bg font-size term-dim font-tex-dim
                     font-texture-width font-texture-height glyph-tex-dim glyph-texture-width glyph-texture-height]} :uniforms
             {:keys [^ByteBuffer glyph-image-data
                     ^ByteBuffer fg-image-data
                     ^ByteBuffer bg-image-data]} :data
             :keys [p-matrix-buffer mv-matrix-buffer character-width character-height character->col-row]} gl
            glyph-image-data glyph-image-data
            fg-image-data fg-image-data
            bg-image-data bg-image-data
            {:keys [screen-width
                    screen-height
                    character-width
                    character-height
                    font-texture-width
                    font-texture-height
                    font-texture]} (get @font-textures (font-key @normal-font))]
        (assert (not (nil? font-texture-width)) "font-texture-width nil")
        (assert (not (nil? font-texture-height)) "font-texture-height")
        (assert (not (nil? font-texture)) "font-texture nil")
        ;; Update glyph texture in buffers
        (.clear glyph-image-data)
        (.clear fg-image-data)
        (.clear bg-image-data)
        (doseq [[row line] (map-indexed vector (reverse @character-map))
                [col c]    (map-indexed vector line)]
          ;;(log/info "row" row "col" col "c" c)
          (let [chr        (or (get c :fx-character) (get c :character))
                highlight  (= @cursor-xy [col row])
                [fg-r fg-g fg-b] (if highlight
                                   (or (get c :fx-bg-color)  (get c :bg-color))
                                   (or (get c :fx-fg-color)  (get c :fg-color)))
                [bg-r bg-g bg-b] (if highlight
                                   (or (get c :fx-fg-color)  (get c :fg-color))
                                   (or (get c :fx-bg-color)  (get c :bg-color)))
                ;s         (str (get c :character))
                style     (get c :style)
                i         (* 4 (+ (* texture-columns row) col))
                [x y]     (get character->col-row [chr (contains? style :underline)])]
            ;(log/info "Drawing at col row" col row "character from atlas col row" x y c "(index=" i ")")
            (when (zero? col)
              (.position glyph-image-data i)
              (.position fg-image-data i)
              (.position bg-image-data i))
            (assert (or (not (nil? x)) (not (nil? y))) (format "X/Y nil - glyph not found for character %s %s" (or (str chr) "nil") (or (format "%x" (int chr)) "nil")))
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
        (Display/makeCurrent)
        (GL11/glViewport 0 0 screen-width screen-height)
        (GL11/glClearColor 0.0 0.0 1.0 1.0)
        (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
        (GL20/glUseProgram program-id)
        (GL20/glUniformMatrix4 u-PMatrix false (ortho-matrix-buffer screen-width screen-height p-matrix-buffer))
        (GL20/glUniformMatrix4 u-MVMatrix false (position-matrix-buffer [(- (/ screen-width 2)) (- (/ screen-height 2)) -1.0 0.0]
                                                                        [screen-width screen-height 1.0]
                                                                        mv-matrix-buffer))
        ; Setup vertex buffer
        ;(GL15/glBindBuffer GL15/GL_ARRAY_BUFFER, vertices-vbo-id)
        (except-gl-errors (str "vbo bind - glBindBuffer" vertices-vbo-id))
        (GL20/glEnableVertexAttribArray 0);pos-vertex-attribute)
        (except-gl-errors "vbo bind - glEnableVertexAttribArray")
        ;;(GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false 0 0)
        (except-gl-errors "vbo bind - glVertexAttribPointer")
        ; Setup uv buffer
        ;(GL15/glBindBuffer GL15/GL_ARRAY_BUFFER, texture-coords-vbo-id)
        (GL20/glEnableVertexAttribArray 1);texture-coords-vertex-attribute)
        ;;(GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false 0 0)
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
        (GL20/glUniform2f font-size, character-width character-height)
        (GL20/glUniform2f term-dim columns rows)
        (GL20/glUniform2f font-tex-dim font-texture-width font-texture-height)
        (GL20/glUniform2f glyph-tex-dim glyph-texture-width glyph-texture-height)
        (except-gl-errors "uniform2f bind")
        (except-gl-errors "gl(en/dis)able")
        ; Send updated glyph texture to gl
        (GL13/glActiveTexture GL13/GL_TEXTURE1)
        (GL11/glBindTexture GL11/GL_TEXTURE_2D glyph-texture)
        (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL30/GL_RGBA8UI texture-columns texture-rows 0 GL30/GL_RGBA_INTEGER GL11/GL_UNSIGNED_BYTE glyph-image-data)
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
        (GL11/glDrawArrays GL11/GL_QUADS 0 vertices-count)
        (except-gl-errors "bg color texture data")
        (except-gl-errors "end of refresh")
        ;(Display/sync 60)
        (Display/update)
        (Display/releaseContext))))
  (clear! [_]
    (ref-set character-map character-map-cleared))
  (set-fx-fg! [_ x y fg]
    {:pre [(vector? fg)
           (= (count fg) 3)]}
      (alter character-map
             (fn [cm] (assoc-in cm [y x :fx-fg-color] fg))))
  (set-fx-bg! [_ x y bg]
    {:pre [(vector? bg)
           (= (count bg) 3)]}
      (alter character-map
             (fn [cm] (assoc-in cm [y x :fx-bg-color] bg))))
  (set-fx-char! [_ x y c]
    (alter character-map
           (fn [cm] (assoc-in cm [y x :fx-character] c))))
  (clear-fx! [_]
    (alter character-map
           (fn [cm]
             (mapv (fn [line]
                     (mapv (fn [c]
                             (assoc c :fx-fg-color nil
                                      :fx-bg-color nil
                                      :fx-character nil))
                           line))
                   cm)))))


(defn make-terminal
  ([]
    (make-terminal "" 80 24))
  ([title columns rows]
   (make-terminal title columns rows [255 255 255] [0 0 0]))
  ([title columns rows default-fg-color default-bg-color]
   (make-terminal title columns rows default-fg-color default-bg-color nil))
  ([title columns rows default-fg-color default-bg-color on-key-fn]
    (make-terminal title columns rows default-fg-color default-bg-color on-key-fn "Courier New" "Monospaced" 16 true))
  ([title columns rows default-fg-color  default-bg-color on-key-fn windows-font else-font font-size antialias]
    (let [is-windows       (>= (.. System (getProperty "os.name" "") (toLowerCase) (indexOf "win")) 0)
          normal-font      (atom nil)
          _                (log/info "Using font" normal-font)
          boxy-font        (make-font "fonts/Boxy/Boxy.ttf" Font/PLAIN 24)
          font-textures    (atom {})
          antialias        (atom antialias)
          _                (add-watch
                             normal-font
                             :font-watcher
                             (fn [_ _ _ new-font]
                               (let [font-metrics  ^FontMetrics (.getFontMetrics (Canvas.) new-font)
                                     char-width                 (.charWidth font-metrics \M)
                                     char-height                (.getHeight font-metrics)
                                     screen-width               (* columns char-width)
                                     screen-height              (* rows char-height)
                                     ;; create texture atlas as gl texture
                                     {:keys [font-texture-width
                                             font-texture-height
                                             font-texture-image]} (make-glyph-image char-width char-height new-font)]
                                 (log/info "Created font texture. screen-width" screen-width "screen-height" screen-height)
                                 (swap! font-textures assoc
                                                     (font-key new-font)
                                                     {:screen-width screen-width
                                                      :screen-height screen-height
                                                      :character-width char-width
                                                      :character-height char-height
                                                      :font-texture-width font-texture-width
                                                      :font-texture-height font-texture-height
                                                      :font-texture-image font-texture-image}))))
          _                  (reset! normal-font (if is-windows
                                                   (make-font windows-font Font/PLAIN font-size)
                                                   (make-font else-font Font/PLAIN font-size)))
          {:keys [screen-width
                  screen-height
                  character-width
                  character-height
                  font-texture-width
                  font-texture-height
                  font-texture-image]} (get @font-textures (font-key @normal-font))
          _                  (log/info "screen size" screen-width "x" screen-height)
          _                  (init-display title screen-width screen-height)

          font-texture       (texture-id font-texture-image)
          _                  (swap! font-textures update (font-key @normal-font) (fn [m] (assoc m :font-texture font-texture)))
          ;; create texture atlas
          character-map-cleared (vec (repeat rows (vec (repeat columns (make-terminal-character \space default-fg-color default-bg-color #{})))))
          character-map         (ref character-map-cleared)
          cursor-xy             (atom nil)

          key-chan         (async/chan)
          on-key-fn        (or on-key-fn
                               (fn default-on-key-fn [k]
                                 (async/put! key-chan k)))

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
          glyph-texture    (xy-texture-id next-pow-2-columns next-pow-2-rows glyph-image-data)
          fg-texture       (texture-id next-pow-2-columns next-pow-2-rows fg-image-data)
          bg-texture       (texture-id next-pow-2-columns next-pow-2-rows bg-image-data)
          ;; init shaders
          pgm-id ^long (init-shaders)
          pos-vertex-attribute (GL20/glGetAttribLocation pgm-id "aVertexPosition")
          texture-coords-vertex-attribute (GL20/glGetAttribLocation pgm-id "aTextureCoord")

          ;; We just need one vertex buffer, a texture-mapped quad will suffice for drawing the terminal.
          {:keys [vertices-vbo-id
                  vertices-count
                  texture-coords-vbo-id]} (init-buffers)
          u-MVMatrix                (GL20/glGetUniformLocation pgm-id "uMVMatrix")
          u-PMatrix                 (GL20/glGetUniformLocation pgm-id "uPMatrix")
          u-font                    (GL20/glGetUniformLocation pgm-id "uFont")
          u-glyphs                  (GL20/glGetUniformLocation pgm-id "uGlyphs")
          u-fg                      (GL20/glGetUniformLocation pgm-id "uFg")
          u-bg                      (GL20/glGetUniformLocation pgm-id "uBg")
          font-size                 (GL20/glGetUniformLocation pgm-id "fontSize")
          term-dim                  (GL20/glGetUniformLocation pgm-id "termDimensions")
          font-tex-dim              (GL20/glGetUniformLocation pgm-id "fontTextureDimensions")
          glyph-tex-dim             (GL20/glGetUniformLocation pgm-id "glyphTextureDimensions")
          terminal
          ;; Create and return terminal
          (OpenGlTerminal. columns
                           rows
                           next-pow-2-columns
                           next-pow-2-rows
                           font-textures
                           normal-font
                           antialias
                           character-map-cleared
                           character-map
                           cursor-xy
                           {:p-matrix-buffer (ortho-matrix-buffer screen-width screen-height)
                            :mv-matrix-buffer (position-matrix-buffer [(- (/ screen-width 2)) (- (/ screen-height 2)) -1.0 0.0]
                                                                      [screen-width screen-height 1.0])
                            :character-width character-width
                            :character-height character-height
                            :character->col-row (character->col-row (character-idxs (displayable-characters boxy-font)))
                            :buffers {:vertices-vbo-id vertices-vbo-id
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
                           key-chan)]
      ;; Access to terminal will be multi threaded. Release context so that other threads can access it)))
      (Display/releaseContext)
      ;; Start font file change listener thread
      #_(cwc/start-watch [{:path "./fonts"
                         :event-types [:modify]
                         :bootstrap (fn [path] (println "Starting to watch " path))
                         :callback (fn [_ filename]
                                     (println "Reloading font" filename)
                                     (reset! normal-font
                                             (make-font filename Font/PLAIN font-size)))
                         :options {:recursive true}}])
      ;; Poll keyboard in background thread and offer input to key-chan
      (future (go-loop []
                (locking terminal
                  (Display/processMessages)
                  (when (Display/isCloseRequested)
                    (System/exit 0))
                  (loop []
                    (when (Keyboard/next)
                      (when (Keyboard/getEventKeyState)
                        (let [character (Keyboard/getEventCharacter)
                              key       (Keyboard/getEventKey)]
                          (convert-key-code character key on-key-fn)))
                      (recur))))
                (Thread/sleep 1)
                (recur)))
      terminal)))

(defn- put-string
  ([^ATerminal screen x y string]
   (put-string screen (int (rmath/ceil x)) (int (rmath/ceil y)) string [255 255 255] [0 0 0] #{}))
  ([^ATerminal screen x y string fg bg]
   (put-string screen (int (rmath/ceil x)) (int (rmath/ceil y)) string fg bg #{}))
  ([^ATerminal screen x y string fg bg styles]
   (put-string screen (int (rmath/ceil x)) (int (rmath/ceil y)) string fg bg styles {}))
  ([^ATerminal screen x y string fg bg styles mask-opts]
   {:pre [(clojure.set/superset? #{:underline :bold} styles)]}
   (let [characters (map-indexed (fn [i c] {:c  (str c)
                                            :fg fg
                                            :bg bg
                                            :x  (+ x i)
                                            :y  y
                                            :opts mask-opts})
                                 string)]
     (rat/put-chars! screen characters))))

(defn single-thread-main
  "Show a terminal and echo input."
  [& _]
  #_(loop [i 30]
    (log/info "Starting in " i)
    (Thread/sleep 1000)
    (when (pos? i)
      (recur (dec i))))
  (let [terminal (make-terminal "glterminal.main" 80 24)
        last-key (atom nil)]
    (future (go-loop []
              (reset! last-key (async/<! (rat/get-key-chan terminal)))
              (log/info "got key" @last-key)
              (recur)))

    (loop []
      (when (not (Display/isCloseRequested))
        (let [key-in (or @last-key \?)]
        ;(let [key-in (rat/wait-for-key terminal)]
          (dosync
            (rat/clear! terminal)
            (put-string terminal 0 0 "Hello world 0 0")
            (put-string terminal 0 1 "abcdefghijklmno")
            (put-string terminal 60 0 "Hello world 60 0" [128 0 0] [0 0 128])
            (put-string terminal 0 19 "Hello world 60 19" [0 128 0] [128 0 0])
            (put-string terminal 60 19 "Hello world 0 19" [0 0 128] [0 128 0])
            (put-string terminal 5 10 (str key-in))
            (rat/refresh! terminal))
          (recur)))))
  (Display/destroy)
  (System/exit 0))

;; multi-thread main
(defn -main
  "Show a terminal and echo input."
  [& _]
  ;; render in background thread
  (let [terminal (make-terminal (format "Robinson - %s@%s" "demo" "glterminal")
                                80 24 [255 255 255] [5 5 8] nil
                                "Courier New" "fonts/Boxy/Boxy.ttf" 18 false)
        last-key (atom nil)
          input-chan (go-loop []
                       (reset! last-key (async/<!! (rat/get-key-chan terminal)))
                       (log/info "got key" (or (str @last-key) "nil"))
                       (case @last-key
                         \s (rat/apply-font! terminal "fonts/Boxy/Boxy.ttf" "fonts/Boxy/Boxy.ttf" 12 false)
                         \m (rat/apply-font! terminal "fonts/Boxy/Boxy.ttf" "fonts/Boxy/Boxy.ttf" 18 false)
                         \l (rat/apply-font! terminal "fonts/Boxy/Boxy.ttf" "fonts/Boxy/Boxy.ttf" 24 false)
                         nil)
                       (recur))]
    ;; get key presses in fg thread
    (loop []
      (when (not (Display/isCloseRequested))
        (let [key-in (or @last-key \?)]
          (dosync
            (rat/clear! terminal)
            (put-string terminal 0 0 "Hello world 0 0")
            (put-string terminal 0 1 "abcdefghijklmno")
            (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
              (put-string terminal 0 i (str c) [128 (+ i 100) 0] [0 0 50]))
            (put-string terminal 60 0 "Hello world 60 0" [128 0 0] [0 0 128])
            (put-string terminal 0 19 "Hello world 60 19" [0 128 0] [128 0 0])
            (put-string terminal 60 19 "Hello world 0 19" [0 0 128] [0 128 0])
            (put-string terminal 5 10 (str key-in))
            (rat/refresh! terminal)))
        (recur)))
    (locking terminal
      (log/info "shutting down")
      (async/close! input-chan)
      (Display/makeCurrent)
      (Display/destroy)))
  (System/exit 0))