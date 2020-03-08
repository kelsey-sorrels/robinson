(ns robinson.error
  (:require
            [clojure.stacktrace :as st]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [taoensso.timbre :as log]
            [taoensso.nippy :as nippy])
  (:import [java.io DataInputStream DataOutputStream]))

(def ^:private encountered (atom #{}))

(defn- encountered?
  [h]
  (-> encountered
    deref
    (contains? h)))

(defn write-exception
  [e state keyin h]
  (try
    (let [log-path (str "log/" h ".log")
          save-path (str "log/" h ".edn")]
      (with-open [o (io/writer (io/output-stream log-path))]
        (binding [*out* o]
          (log/error (str "Keyin:" keyin))
          (log/error (str "Current-State:" (get-in state [:world :current-state])))
          (log/error (str e))
          (st/print-stack-trace e)
          (st/print-cause-trace e)))
      (when state
        (with-open [o (io/output-stream save-path)]
          (nippy/freeze-to-out! (DataOutputStream. o) (get state :world)))))
    (catch Throwable t
      (log/error t))))

(defn log-exception
  ([e state]
    (log-exception e state nil))
  ([e state keyin]
    ; determine e hash
    (let [h (hash (Throwable->map e))]
      (when-not (encountered? h)
        (swap! encountered conj h)
        (log/error e)
        (write-exception e state keyin h)))))

(defn write-spec-message
  [message msg h]
  (try
    (let [log-path (str "log/" h "-spec.log")]
      (with-open [o (io/writer (io/output-stream log-path))]
        (binding [*out* o]
          (log/error msg message)
          (st/print-stack-trace (Throwable.)))))
    (catch Throwable t
      (log/error t))))

(defn validate
  ([spec x]
   (validate spec x nil))
  ([spec x message]
    ; determine spec+x hash
    (let [valid (s/valid? spec x)
          h (hash [spec x])]
      (when (and (not valid)
                 (not (encountered? h)))
        (swap! encountered conj h)
        (let [msg (expound/expound-str spec x)]
          (log/error message msg)
          (write-spec-message message msg h)))
      valid)))

