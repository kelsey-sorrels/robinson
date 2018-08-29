(ns robinson.mediaplayer
  (:require [clj-audio.core :as acore])
  (:import [java.io BufferedInputStream ByteArrayInputStream]))


(defn start-background-music []
  (let [url (java.net.URL. "http://opengameart.org/sites/default/files/Theme%20of%20Agrual_0.mp3")
        ais (javax.sound.sampled.AudioSystem/getAudioInputStream url)]
    (acore/->stream url)))

