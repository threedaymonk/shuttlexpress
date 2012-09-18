(use posix extras)
(include "interface.scm")

(handle-shuttle-events (lambda (name . params)
  (printf "~A ~S~N" name params)))
