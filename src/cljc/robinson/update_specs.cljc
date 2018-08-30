(ns robinson.update-specs
  (:require [robinson.specs :as rs]
            [robinson.update :as rupdate]
            [clojure.spec :as s]))

(s/def ::keyin (set (map char
                         (concat (range (int \a) (int \z))
                                 (range (int \A) (int \Z))))))
(s/fdef rupdate/update-state
  :args (s/and (s/cat :state ::rs/state :key-in char?))
  :ret ::rs/state)

