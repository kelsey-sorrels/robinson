{  ;;; Control log filtering by namespace patterns (e.g. ["my-app.*"]).
   ;;; Useful for turning off logging in noisy libraries, etc.
   :ns-whitelist []
   :ns-blacklist []

   ;; Fns (applied right-to-left) to transform/filter appender fn args.
   ;; Useful for obfuscating credentials, pattern filtering, etc.
   :middleware []

   ;;; Control :timestamp format
   :timestamp-pattern "yyyy-MMM-dd HH:mm:ss ZZ" ; SimpleDateFormat pattern
   :timestamp-locale  nil ; A Locale object, or nil

   ;; Output formatter used by built-in appenders. Custom appenders may (but are
   ;; not required to use) its output (:output). Extra per-appender opts can be
   ;; supplied as an optional second (map) arg.
   :fmt-output-fn
   (fn [{:keys [level throwable message timestamp hostname ns]}
       ;; Any extra appender-specific opts:
       & [{:keys [nofonts?] :as appender-fmt-output-opts}]]
     ;; <timestamp> <hostname> <LEVEL> [<ns>] - <message> <throwable>
     (format "%s %s %s [%s] - %s%s"
       timestamp hostname (-> level name str/upper-case) ns (or message "")
       (or (stacktrace throwable "\n" (when nofonts? {})) "")))

   :shared-appender-config {} ; Provided to all appenders via :ap-config key
   :spit-filename "logs/dungeon-crusade.log"
   :appenders
   {:standard-out
    {:doc "Prints to *out*/*err*. Enabled by default."
     :min-level :warn :enabled? false :async? true :rate-limit nil
     :fn (fn [{:keys [error? output]}] ; Use any appender args
           (binding [*out* (if error? *err* *out*)]
             (str-println output)))}

    :spit
    {:doc "Spits to `(:spit-filename :shared-appender-config)` file."
     :min-level :warn :enabled? false :async? false :rate-limit nil
     :fn (fn [{:keys [ap-config output]}] ; Use any appender args
           (when-let [filename (:spit-filename ap-config)]
             (try (spit filename output :append true)
                  (catch java.io.IOException _))))}}}
