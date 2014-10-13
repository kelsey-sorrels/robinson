;; Functions for rendering state to screen
(ns robinson.swingterminal
  (:require [taoensso.timbre :as timbre])
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

(defprotocol ATerminal
  (get-size [this])
  (put-string [this x y string]
              [this x y string fg bg]
              [this x y string fg bg style])
  (wait-for-key [this])
  (set-cursor [this xy])
  (refresh [this])
  (clear [this]))

(defrecord TerminalCharacter [character fg-color bg-color style])

(defmacro for-loop [[sym init check change :as params] & steps]
 `(loop [~sym ~init value# nil]
    (if ~check
      (let [new-value# (do ~@steps)]
        (recur ~change new-value#))
      value#)))

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
    (let [is-windows       (>= (.. System (getProperty "os.name" "") (toLowerCase) (indexOf "win")) 0)
          normal-font      (if is-windows
                              (Font. windows-font Font/PLAIN font-size)
                              (Font. else-font Font/PLAIN font-size))
          bold-font        (if is-windows
                              (Font. windows-font Font/BOLD font-size)
                              (Font. else-font  Font/BOLD font-size))
          _                (info "Using font" (.getFontName normal-font))
          default-fg-color (Color. (long default-fg-color-r) (long default-fg-color-g) (long default-fg-color-b))
          default-bg-color (Color. (long default-bg-color-g) (long default-bg-color-g) (long default-bg-color-b))
          character-map    (atom (vec (repeat rows (vec (repeat columns (TerminalCharacter. \space default-fg-color default-bg-color #{}))))))
          cursor-xy        (atom nil)
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
                               (let [graphics-2d ^Graphics2D   (.create graphics)
                                     font-metrics ^FontMetrics (.getFontMetrics graphics normal-font)
                                     screen-width              (* columns (.charWidth font-metrics \space))
                                     screen-height             (* rows (.getHeight font-metrics))
                                     char-width                (/ screen-width columns)
                                     char-height               (/ screen-height rows)]
                                 (doto graphics-2d
                                   (.setFont normal-font)
                                   (.setColor default-bg-color)
                                   (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                                   (.setRenderingHint RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
                                   (.fillRect 0 0 screen-width screen-height))
                                 ;(doseq [row (range rows)]
                                 ;  (println (apply str (map #(get % :character) (get @character-map row)))))
                                 (doseq [row (range rows)
                                         col (range columns)]
                                   (let [c        (get-in @character-map [row col])
                                         x        (long (* col char-width))
                                         y        (long (- (* (inc row) char-height) (.getDescent font-metrics)))
                                         fg-color (if (= @cursor-xy [col row])
                                                    (get c :bg-color)
                                                    (get c :fg-color))
                                         bg-color (if  (= @cursor-xy [col row])
                                                    (get c :fg-color)
                                                    (get c :bg-color))
                                         s        (str (get c :character))
                                         style    (get c :style)]
                                     (when (not= bg-color default-bg-color)
                                       ;(println "filling rect" (* col char-width) (* row char-height) char-width char-height bg-color)
                                       (doto graphics-2d
                                         (.setColor bg-color)
                                         (.fillRect (* col char-width) (* row char-height) char-width char-height)))
                                     (when (not= s " ")
                                       ;(println "drawing" s "@" x y fg-color bg-color)
                                       (doto graphics-2d
                                         (.setColor fg-color)
                                         (.drawString s x y)))
                                     (when (contains? style :underline)
                                       (let [y (dec (* (inc row) char-height))]
                                         (doto graphics-2d
                                           (.setColor fg-color)
                                           (.drawLine (* col char-width)
                                                      y
                                                      (* (inc col) char-width)
                                                      y))))))
                                 (.dispose graphics-2d))))
          keyListener      (reify KeyListener
                             (keyPressed [this e]
                               ;(println "keyPressed keyCode" (.getKeyCode e) "escape" KeyEvent/VK_ESCAPE "escape?" (= (.getKeyCode e) KeyEvent/VK_ESCAPE))
                               (when-let [k (cond
                                              (= (.getKeyCode e) KeyEvent/VK_ENTER)      :enter
                                              (= (.getKeyCode e) KeyEvent/VK_ESCAPE)     :escape
                                              (= (.getKeyCode e) KeyEvent/VK_BACK_SPACE) :backspace
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
                             (.pack)
                             (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                             (.setLocationByPlatform true)
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
                  string-length (.length s)]
              (for-loop [i 0 (< i string-length) (inc i)] 
                (let [col (+ col i)]
                  (when (< -1 col columns)
                    (let [character (TerminalCharacter. (.charAt s i) fg-color bg-color style)]
                      (reset! character-map (assoc-in @character-map [row col] character)))))))))
        (wait-for-key [this]
          (.take key-queue))
        (set-cursor [this xy]
          (reset! cursor-xy xy))
        (refresh [this]
          (SwingUtilities/invokeLater
            (fn refresh-fn [] (.repaint terminal-renderer))))
        (clear [this]
          (let [c (TerminalCharacter. \space default-fg-color default-bg-color #{})]
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
