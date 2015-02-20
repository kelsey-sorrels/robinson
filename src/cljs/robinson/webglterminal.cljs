;; Functions for rendering state to screen
(ns robinson.webglterminal
  (:use     robinson.common
            robinson.aterminal)
  (:require [taoensso.timbre :as timbre]
            [monet.canvas :as canvas]))

(def character-canvas-dom (.getElementById js/document "character-canvas"))
(def character-canvas (canvas/init character-canvas-dom "2d"))

(def terminal-canvas-dom (.getElementById js/document "terminal-canvas"))
(def terminal-canvas (canvas/init terminal-canvas-dom "2d"))

(timbre/refer-timbre)
(set! *warn-on-reflection* true)

(def characters
  (map identity 
       (concat "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789~!@#$%^&*()_-+=[]{}|:;,./<>?'\""
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
               "\u2665"
               "\u2500"
               "\u2500"
               "\u2500"
               "\u2500"
               "\u2500"
               "\u2500")))

;; A sequence of [\character x y] where [x y] is the position in the character atlas.
(def character-layout
  (let [character-matrix (partition (int (inc (.sqrt js/Math (count characters)))) characters)]
    (mapcat
      identity
      (map-indexed (fn [line y]
                     (map-indexed (fn [c x]
                                    [c (* x 12) (* y 16)])
                                  line))
                   character-matrix))))

;; A map from \character to [x1 y1 x2 y2] in the character atlas where
;;    [x1 y1]
;;    +--------+
;;    |        |
;;    |        |
;;    +--------+
;;              [x2 y2]
(def character->pos
  (reduce (fn [m [c x y]]
            (assoc m c [x y (+ x 12) (+ y 16)]))
          {}
          character-layout))


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
    (let [normal-font      (if is-windows
                              (Font. windows-font Font/PLAIN font-size)
                              (Font. else-font Font/PLAIN font-size))
          _                (info "Using font" (.getFontName normal-font))
          default-fg-color (Color. (long default-fg-color-r) (long default-fg-color-g) (long default-fg-color-b))
          default-bg-color (Color. (long default-bg-color-g) (long default-bg-color-g) (long default-bg-color-b))
          ;; create texture atlas
          _                (doseq [[c x y] character->pos]
                             (canvas/text character-canvas {:text (str c) :x x :y y}))
          character-map    (atom (vec (repeat rows (vec (repeat columns (make-terminal-character \space default-fg-color default-bg-color #{}))))))
          cursor-xy        (atom nil)
          ;; draws a character using webgl. 
          draw-character   (fn [ctx c [x y]]
                             (info "Drawing character" c)
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
                               (doto offscreen-graphics-2d
                                 (.setFont normal-font)
                                 (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                                 (.setRenderingHint RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
                                 (.setColor bg-color)
                                 (.fillRect 0 0 char-width char-height))
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
                                                y))))
                               (.dispose offscreen-graphics-2d)
                               glyph-image))
          key-queue        (LinkedBlockingQueue.)
          on-key-fn        (or on-key-fn
                               (fn default-on-key-fn [k]
                                 (.add key-queue k)))
          terminal-renderer (proxy [JComponent] []
                             (getPreferredSize []
                               (let [graphics      ^Graphics    (proxy-super getGraphics)
                                     font-metrics  ^FontMetrics (.getFontMetrics graphics normal-font)
                                     screen-width               (* columns (.charWidth font-metrics \space))
                                     screen-height              (* rows (.getHeight font-metrics))
                                     char-width                 (/ screen-width columns)
                                     char-height                (/ screen-height rows)]
                                 (Dimension. screen-width screen-height)))
                             (paintComponent [^Graphics graphics]
                               (log-time "blit"
                                 (let [graphics-2d ^Graphics2D           (.create graphics)
                                       font-metrics ^FontMetrics         (.getFontMetrics graphics normal-font)
                                       screen-width                      (* columns (.charWidth font-metrics \space))
                                       screen-height                     (* rows (.getHeight font-metrics))
                                       char-width                        (/ screen-width columns)
                                       char-height                       (/ screen-height rows)]
                                   (doto graphics
                                     (.fillRect 0 0 screen-width screen-height))
                                   ;(doseq [row (range rows)]
                                   ;  (println (apply str (map #(get % :character) (get @character-map row)))))
                                   (doseq [row (range rows)
                                           col (range columns)]
                                     (let [c         (get-in @character-map [row col])
                                           x         (long (* col char-width))
                                           y         (long (- (* (inc row) char-height) (.getDescent font-metrics)))
                                           highlight (= @cursor-xy [col row])
                                           char-img  (glyph-cache this font-metrics highlight char-width char-height c)]
                                       (.drawImage graphics char-img x (- y char-height) this)))
                                   (.dispose graphics-2d)))))
          keyListener      (reify KeyListener
                             (keyPressed [this e]
                               ;(println "keyPressed keyCode" (.getKeyCode e) "escape" KeyEvent/VK_ESCAPE "escape?" (= (.getKeyCode e) KeyEvent/VK_ESCAPE))
                               (when-let [k (cond
                                              (= (.getKeyCode e) KeyEvent/VK_ENTER)      :enter
                                              (= (.getKeyCode e) KeyEvent/VK_ESCAPE)     :escape
                                              (= (.getKeyCode e) KeyEvent/VK_SPACE)      :space
                                              (= (.getKeyCode e) KeyEvent/VK_BACK_SPACE) :backspace
                                              (= (.getKeyCode e) KeyEvent/VK_NUMPAD1)    :numpad1
                                              (= (.getKeyCode e) KeyEvent/VK_NUMPAD2)    :numpad2
                                              (= (.getKeyCode e) KeyEvent/VK_NUMPAD3)    :numpad3
                                              (= (.getKeyCode e) KeyEvent/VK_NUMPAD4)    :numpad4
                                              (= (.getKeyCode e) KeyEvent/VK_NUMPAD5)    :numpad5
                                              (= (.getKeyCode e) KeyEvent/VK_NUMPAD6)    :numpad6
                                              (= (.getKeyCode e) KeyEvent/VK_NUMPAD7)    :numpad7
                                              (= (.getKeyCode e) KeyEvent/VK_NUMPAD8)    :numpad8
                                              (= (.getKeyCode e) KeyEvent/VK_NUMPAD9)    :numpad9
                                              true (let [altDown (not= (bit-and (.getModifiersEx e) InputEvent/ALT_DOWN_MASK) 0)
                                                         ctrlDown (not= (bit-and (.getModifiersEx e) InputEvent/CTRL_DOWN_MASK) 0)]
                                                     ;(println "processing non-enter non-escape keypress")
                                                     (when (and altDown ctrlDown (<= \A (.getKeyCode e) \Z))
                                                       (.toLowerCase (char (.getKeyCode e))))))]

                                 (on-key-fn k)))
                             (keyReleased [this keyEvent]
                               nil)
                             (keyTyped [this e]
                               (let [character (.getKeyChar e)
                                     altDown   (not= (bit-and (.getModifiersEx e) InputEvent/ALT_DOWN_MASK) 0)
                                     ctrlDown  (not= (bit-and (.getModifiersEx e) InputEvent/CTRL_DOWN_MASK) 0)
                                     ignore    #{(char 10) (char 33) (char 27)}]
                                 (when-not (contains? ignore character)
                                   (if ctrlDown
                                       (on-key-fn (char (+ (int \a) -1 (int character))))
                                       (on-key-fn character))))))
          icon            (.getImage (java.awt.Toolkit/getDefaultToolkit) "images/icon.png")
          frame            (doto (JFrame. "Robinson")
                             (.. (getContentPane) (setLayout (BorderLayout.)))
                             (.. (getContentPane) (add terminal-renderer BorderLayout/CENTER))
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
                             (.pack))]
      (reify ATerminal
        (get-size [this]
          [columns rows])
        (put-string [this col row string]
          (.put-string this col row string [255 255 255] [0 0 0] #{}))
        (put-string [this col row string fg bg]
          (.put-string this col row string fg bg #{}))
        (put-string [this col row string fg bg style]
          (when (< -1 row rows)
            (let [fg-color (Color. (long (fg 0)) (long (fg 1)) (long (fg 2)))
                  bg-color (Color. (long (bg 0)) (long (bg 1)) (long (bg 2)))
                  s ^String string
                  string-length (.length s)
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
                                                 fg-color  (Color. (long (fg 0)) (long (fg 1)) (long (fg 2)))
                                                 bg-color  (Color. (long (bg 0)) (long (bg 1)) (long (bg 2)))
                                                 character (make-terminal-character (first (get c :c)) fg-color bg-color {})]
                                             (assoc! line (get c :x) character))
                                           line))
                                     (transient (get cm row))
                                     row-characters)))
                          cm))
                      cm
                      (group-by :y characters)))))
        (wait-for-key [this]
          (.take key-queue))
        (set-cursor [this xy]
          (reset! cursor-xy xy))
        (refresh [this]
          (SwingUtilities/invokeLater
            (fn refresh-fn [] (.repaint terminal-renderer))))
        (clear [this]
          (let [c (make-terminal-character \space default-fg-color default-bg-color #{})]
          (doseq [row (range rows)
                  col (range columns)]
            (reset! character-map (assoc-in @character-map [row col] c)))))))))


(defn -main
  "Show a terminal and echo input."
  [& args]
  (let [terminal ^robinson.swingterminal.ATerminal (make-terminal 80 20)]
    (.clear terminal)
    (.put-string terminal 5 5 "Hello world")
    (.refresh terminal)
    (loop []
      (let [key-in (wait-for-key terminal)]
        (.clear terminal)
        (.put-string terminal 5 5 "Hello world")
        (.put-string terminal 5 10 (str key-in))
        (.refresh terminal))
        (recur))))
