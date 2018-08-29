;; Functions for sending feedback
(ns robinson.feedback
  (:require [clj-http.client :as client]
            [seesaw.core :as ss]
            [seesaw.border :as ss-border]
            [seesaw.forms :as ss-forms]
            [clojure.data.json :as json]))

(defn upload-report [data]
  (client/post "http://aaron-santos.com:3000/reports"
               {:body (json/write-str
                        (clojure.walk/postwalk (fn [v] (if (char? v)
                                                         (str v)
                                                         v))
                                               data))
                :content-type :json}))

(defn send-report [state]
  (let [date              (str (new java.util.Date))
        version           (get state :version)
        user-id           (get state :user-id)
        upload-input      (ss/checkbox :selected? true)
        description-input (ss/text :columns 80
                                   :rows 25
                                   :multi-line? true
                                   :wrap-lines? true
                                   :border (ss-border/line-border :color "#aab"))
        send-button       (ss/button :text "Send")
        cancel-button     (ss/button :text "Cancel")
        content (ss-forms/forms-panel
                  "right:pref,4dlu,pref"
                  :items [
                    "Date:"          date
                    "Version:"       version 
                    "User-id:"       user-id   
                    "Upload save?"   upload-input                                     
                    "What happened?" description-input
                    "" send-button      (ss-forms/next-line)
                    "" cancel-button])
        frame (ss/frame :title "Report an Issue",
                        :content content
                        :resizable? false
                        :on-close :hide)]
  (ss/listen cancel-button :mouse-clicked (fn [e] (ss/hide! frame)))
  (ss/listen send-button :mouse-clicked (fn [e] (upload-report
                                                  {:date date
                                                   :version version
                                                   :user-id user-id
                                                   :description (ss/value description-input)
                                                   :world (if (ss/value upload-input)
                                                            (get state :world)
                                                            nil)})
                                                (ss/hide! frame)))
  (->
    frame
    ss/pack!
    ss/show!)))

