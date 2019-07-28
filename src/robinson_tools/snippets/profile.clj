(require '[clj-async-profiler.core :as prof]
         '[taoensso.timbre :as log])
(log/info "starting profile")
(prof/profile-for 10 {:threads true :width 6400})
(log/info "stopped profile")
*state*

