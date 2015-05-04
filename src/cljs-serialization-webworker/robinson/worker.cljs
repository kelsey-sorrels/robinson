(ns robinson.worker
  (:require [tailrecursion.cljson :as cljson])
  (:require-macros [servant.macros :as [servant]]))


;(log/set-log-level! :debug)
;(log/set-log-level! :error)

;(cljs.reader/register-tag-parser! "robinson.monstergen.Monster" mg/map->Monster)


(servant/defservantfn serialize-world [world]
  (cljson/clj->cljson world))

