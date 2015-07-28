;; Functions for sending feedback
(ns robinson.feedback
  (:require [clj-http.client :as client]
            [seesaw.core :as ss]
            [seesaw.forms :as ss-forms]
            [clojure.data.json :as json]))

(defn frame-content
  []
  (ss-forms/forms-panel
    "pref,4dlu,80dlu,8dlu,pref,4dlu,80dlu"
    :column-groups [[1 5]]
    :items [(ss-forms/separator "General")
            "Company" (ss-forms/span (ss/text) 5)
            "Contact" (ss-forms/span (ss/text) 5)
            (ss-forms/separator "Propeller")
            "PTI/kW"  (ss/text :columns 10) "Power/kW" (ss/text :columns 10)
            "R/mm"    (ss/text :columns 10) "D/mm"     (ss/text :columns 10)]
    :default-dialog-border? true))

(defn send-report [state]
  (let [
        upload-input      (ss/checkbox :selected? true)
        description-input (ss/text :columns 80 :rows 25 :multi-line? true :wrap-lines? true)
        send-button       (ss/button :text "Send")
        cancel-button     (ss/button :text "Cancel")
        content (ss-forms/forms-panel
                  "right:pref,4dlu,pref"
                  :items [
                    "Date:"          (str (new java.util.Date))
                    "Version:"       (get state :version) 
                    "User-id:"       (get state :user-id)   
                    "Upload save?"   upload-input                                     
                    "What happened?" description-input                                 
                    "" send-button      (ss-forms/next-line)
                    "" cancel-button])]
  (->
    (ss/frame :title "Report an Issue",
              :content content
              :resizable? false
              :on-close :hide)
    ss/pack!
    ss/show!)))

