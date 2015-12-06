;; Functions for rendering state to screen
(ns robinson.swingterminal
  (:use     robinson.common
            robinson.aterminal)
  (:require [robinson.macros :as rm]
            [taoensso.timbre :as timbre]
            [clojure-watch.core :as cwc]
            [clojure.core.async :as async :refer [go go-loop]]
            clojure.core.memoize
            clojure.java.io)
  (:import  
            java.util.concurrent.LinkedBlockingQueue
            java.awt.Color
            java.awt.image.BufferedImage
            java.awt.BasicStroke
            java.awt.BorderLayout
            java.awt.Color
            java.awt.Container
            java.awt.Dimension
            java.awt.Font
            java.awt.FontMetrics
            java.awt.Graphics
            java.awt.Graphics2D
            java.awt.RenderingHints
            java.awt.event.ActionEvent
            java.awt.event.ActionListener
            java.awt.event.ComponentAdapter
            java.awt.event.ComponentEvent
            java.awt.event.InputEvent
            java.awt.event.KeyListener
            java.awt.event.KeyEvent
            javax.swing.JComponent
            javax.swing.JFrame
            javax.swing.SwingUtilities
            javax.swing.Timer
            javax.swing.ImageIcon))

(timbre/refer-timbre)
(set! *warn-on-reflection* true)

(defn mac-os?
  []
  (not (nil? (re-find #"[Mm]ac" (System/getProperty "os.name")))))


(defn make-font
  [name-or-path style size]
  (let [font-file (clojure.java.io/as-file name-or-path)]
    (if (.exists font-file)
      ;; Load font from file
      (.deriveFont (Font/createFont Font/TRUETYPE_FONT font-file) (int style) (float size))
      ;; Load font from font registry
      (Font. name-or-path style size))))

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
    (make-terminal "" 80 24))
  ([title columns rows]
    (make-terminal title columns rows [255 255 255] [0 0 0]))
  ([title columns rows default-fg-color default-bg-color]
    (make-terminal title columns rows default-fg-color default-bg-color nil))
  ([title columns rows default-fg-color default-bg-color on-key-fn]
    (make-terminal title columns rows default-fg-color default-bg-color on-key-fn "Courier New" "Monospaced" 14 true))
  ([title columns rows [default-fg-color-r default-fg-color-g default-fg-color-b]
                       [default-bg-color-r default-bg-color-g default-bg-color-b]
                       on-key-fn
                       windows-font
                       else-font
                       font-size
                       antialias]
    (let [is-windows       (>= (.. System (getProperty "os.name" "") (toLowerCase) (indexOf "win")) 0)
          normal-font      (atom (if is-windows
                                   (make-font windows-font Font/PLAIN font-size)
                                   (make-font else-font Font/PLAIN font-size)))
          bold-font        (atom (if is-windows
                                   (make-font windows-font Font/BOLD font-size)
                                   (make-font else-font  Font/BOLD font-size)))
          _                (info "Using font" (.getFontName @normal-font))
          antialias        (atom antialias)
          default-fg-color (Color. (long default-fg-color-r) (long default-fg-color-g) (long default-fg-color-b))
          default-bg-color (Color. (long default-bg-color-g) (long default-bg-color-g) (long default-bg-color-b))
          character-map-cleared (vec (repeat rows (vec (repeat columns (make-terminal-character \space default-fg-color default-bg-color #{})))))
          character-map    (atom character-map-cleared)
          cursor-xy        (atom nil)
          ;; draws a character to an image and returns the image. 
          glyph-cache      (clojure.core.memoize/memo (fn [component font-metrics highlight char-width char-height c]
                             (info "Creating glyph for" c)
                             (let [glyph-image                       (.createImage component char-width char-height)
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
                                 (.setFont @normal-font)
                                 (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING (if @antialias
                                                                                           RenderingHints/VALUE_TEXT_ANTIALIAS_GASP
                                                                                           RenderingHints/VALUE_TEXT_ANTIALIAS_OFF))
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
                               glyph-image)))
          key-chan         (async/chan)
          on-key-fn        (or on-key-fn
                               (fn default-on-key-fn [k]
                                 (async/put! key-chan k)))
          terminal-renderer (proxy [JComponent] []
                             (getPreferredSize []
                               (let [graphics      ^Graphics    (proxy-super getGraphics)
                                     font-metrics  ^FontMetrics (.getFontMetrics graphics @normal-font)
                                     screen-width               (* columns (.charWidth font-metrics \M))
                                     screen-height              (* rows (.getHeight font-metrics))
                                     char-width                 (/ screen-width columns)
                                     char-height                (/ screen-height rows)]
                                 (Dimension. screen-width screen-height)))
                             (paintComponent [^Graphics graphics]
                               ;(rm/log-time "blit"
                                 (let [graphics-2d ^Graphics2D           (.create graphics)
                                       font-metrics ^FontMetrics         (.getFontMetrics graphics @normal-font)
                                       screen-width                      (* columns (.charWidth font-metrics \M))
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
                                       (.drawImage graphics char-img x (+ (- y char-height) 5) this)))
                                   (.dispose graphics-2d))))
                                ;)
          keyListener      (reify KeyListener
                             (keyPressed [this e]
                               ;(println "keyPressed keyCode" (.getKeyCode e) "escape" KeyEvent/VK_ESCAPE "escape?" (= (.getKeyCode e) KeyEvent/VK_ESCAPE))
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

                                   (info "KeyPressed" k e)
                                   (on-key-fn k))))
                             (keyReleased [this keyEvent]
                               nil)
                             (keyTyped [this e]
                               (let [character (.getKeyChar e)
                                     numpad?   (= (.getKeyLocation e) KeyEvent/KEY_LOCATION_NUMPAD)
                                     altDown   (not= (bit-and (.getModifiersEx e) InputEvent/ALT_DOWN_MASK) 0)
                                     ctrlDown  (not= (bit-and (.getModifiersEx e) InputEvent/CTRL_DOWN_MASK) 0)
                                     ignore    #{(char 10) (char 33) (char 27)}]
                                 (when (and (re-matches #"[!-~]" (str character))
                                            (not (re-matches #"[0-9]" (str character))))
                                   (if ctrlDown
                                       (on-key-fn (char (+ (int \a) -1 (int character))))
                                       (on-key-fn character))))))
          icon            (.getImage (java.awt.Toolkit/getDefaultToolkit) "images/icon.png")
          frame            (doto (JFrame. title)
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
      ;; Start font file change listener thread
      (cwc/start-watch [{:path "./fonts"
                         :event-types [:modify]
                         :bootstrap (fn [path] (println "Starting to watch " path))
                         :callback (fn [event filename]
                                     (println "Reloading font" filename)
                                     (reset! normal-font
                                             (make-font filename Font/PLAIN font-size))
                                     (clojure.core.memoize/memo-clear! glyph-cache))
                         :options {:recursive true}}])
      ;; Create and return terminal
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
        (get-key-chan [this]
          key-chan)
        (apply-font [this windows-font else-font size smooth]
          (reset! normal-font
                  (if is-windows
                    (make-font windows-font Font/PLAIN size)
                    (make-font else-font Font/PLAIN size)))
          (reset! antialias smooth)
          (doto frame
            .doLayout
            .pack)
          nil)
        (set-cursor [this xy]
          (reset! cursor-xy xy))
        (refresh [this]
          (SwingUtilities/invokeLater
            (fn refresh-fn [] (.repaint terminal-renderer))))
        (clear [this]
          (reset! character-map character-map-cleared))))))


(defn -main
  "Show a terminal and echo input."
  [& args]
  (let [terminal ^robinson.aterminal.ATerminal (make-terminal 80 20)]
    (.clear terminal)
    (.put-string terminal 5 5 "Hello world")
    (.refresh terminal)
    (go-loop []
      (let [key-in (async/<! (get-key-chan terminal))]
        (.clear terminal)
        (.put-string terminal 5 5 "Hello world")
        (.put-string terminal 5 10 (str key-in))
        (.refresh terminal))
        (recur))))
