(ns robinson.update-test
  (:require #_[robinson.update-specs :refer :all]
            robinson.update
            [taoensso.timbre :as log]
            [puget.printer :as puget]
            [clojure.string :as str]
            [clojure.test :as t :refer-macros [deftest is]] 
            [clojure.spec.test.alpha :as stest]))

(comment
(defmulti print-handler identity)
(defmethod print-handler Throwable [_]
  (fn [printer t]
    (fr/pst t)))
(defmethod print-handler clojure.lang.PersistentArrayMap [_]
  (fn [printer m]
    (if (vector? (get-in m [:data :atmo]))
      (fv/visit-map
        printer
        (update-in m [:data :atmo] seq))
      (fv/visit-map printer m))))
(defmethod print-handler Throwable [_]
  (fn [printer ex]
    (puget/format-doc
      printer
      ["#clojure.lang.ExceptionInfo"
       (.data ex)])))
(defmethod print-handler :default [typ]
  (fn [printer v]
    (get puget/common-handlers typ)))

(def opts {:seq-limit 10
           :print-handlers print-handler}))
#_(try
  (let [results (group-by #(contains? % :fatal)
                          (log/with-level :info (stest/check)))
        pass    (get results false)
        fail    (get results true)]
    (doseq [f fail]
      (puget/with-options opts
        (log/error "Spec failure" (puget/cprint-str f))))
    (println "Ran" (count (concat pass fail)) "tests")
    (println (count fail) "failures"))
  (catch Throwable t
    (log/error "Exception during check" t)))

;; https://gist.github.com/Risto-Stevcev/dc628109abd840c7553de1c5d7d55608
(defn summarize-results* [spec-check]
  (map (comp puget/cprint-str stest/abbrev-result) spec-check))

(defn check* [spec-check]
  (t/is (nil? (-> spec-check first :failure)) (summarize-results* spec-check)))

#_(t/deftest update-specs
  (check* (stest/check)))

;; https://stackoverflow.com/questions/40697841/howto-include-clojure-specd-functions-in-a-test-suite
(defmacro defspec-test
  ([name sym-or-syms] `(defspec-test ~name ~sym-or-syms nil))
  ([name sym-or-syms opts]
   (when t/*load-tests*
     `(def ~(vary-meta name assoc
                       :test `(fn []
                                (let [check-results# (clojure.spec.test.alpha/check ~sym-or-syms ~opts)
                                      checks-passed?# (every? nil? (map :failure check-results#))]
                                  (if checks-passed?#
                                    (t/do-report {:type    :pass
                                                  :message (str "Generative tests pass for "
                                                                (str/join ", " (map :sym check-results#)))})
                                    (doseq [failed-check# (filter :failure check-results#)
                                            :let [r# (clojure.spec.test.alpha/abbrev-result failed-check#)
                                                  failure# (:failure r#)]]
                                      (t/do-report
                                        {:type     :fail
                                         :message  (with-out-str (clojure.spec.alpha/explain-out failure#))
                                         :expected (->> r# :spec rest (apply hash-map) :ret)
                                         :actual   (if (instance?  Throwable failure#)
                                                     failure#
                                                     (:clojure.spec.test/val failure#))})))
                                  checks-passed?#)))
        (fn [] (t/test-var (var ~name)))))))

(defspec-test test-average [robinson.update/update-state])
