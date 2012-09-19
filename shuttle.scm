(use extras)
(include "interface.scm")
(include "fake-keys")

(handle-shuttle-events (lambda (event)
   (printf "~S~N"  event)))
