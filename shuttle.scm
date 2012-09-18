(use posix extras)
(include "interface.scm")

(handle-shuttle-events (lambda (event)
   (printf "~S~N"  event)))
