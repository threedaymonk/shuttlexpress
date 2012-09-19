(use extras)
(include "interface.scm")
(include "fake-keys")

(define mapping
  '((("jog" -1) .           ("Left"))
    (("jog"  1) .           ("Right"))
    (("ring-relative" -1) . ("bracketleft"))
    (("ring-relative"  1) . ("bracketright"))
    (("button" 3 1) .       ("p"))))

(handle-shuttle-events (lambda (event)
  (let ((command (assoc event mapping)))
    (if command
      (apply fake-keypress (cdr command))))))
