(use posix extras srfi-1 matchable)
(include "support.scm")

(define PACKET-LENGTH 5)
(define DEVICE "/dev/shuttlexpress")

; Read a packet from the controller and return a list of byte values
(define (read-packet fd)
  (match-let (((buffer bytes-read) (file-read fd PACKET-LENGTH)))
    (map char->integer (string->list buffer))))

; Return a list of the buttons: 1 for pressed, 0 for released
; As there's no overlap between the bit masks for the two bytes used to
; indicate button presses, we can just add them together
(define (button-states packet)
  (let ((bitfield (+ (fourth packet) (fifth packet)))
        (masks '(#x10 #x20 #x40 #x80 #x1)))
    (map (lambda (mask) (/ (bitwise-and mask bitfield) mask)) masks)))

; Current position of the ring (-7 to 7)
(define (ring-position packet) (twoc->signed 8 (first packet)))

; Current position of the jog wheel (0 to 255, rolling over)
(define (jog-position packet) (second packet))

; Wrap all the inputs into a single list
(define (shuttle-state packet)
  (list (ring-position packet) (jog-position packet) (button-states packet)))

; The jog position is an integer between 0 and 255, so we must compare it with
; the previous value to work out if it has moved. Taking the difference modulo
; 256 as a two's complement value gives us a relative movement that wraps
; around 255/0.
(define (wrapdiff prev curr)
  (twoc->signed 8 (modulo (- curr prev) 256)))

; Do something about an event. At the moment, that means display it.
(define (handle-event . params)
  (printf "~S~N" params))

; Compare the previous and current states, work out what's changed, and do
; something about it.
(define (compare-states previous current)
  (match-let* (((ring-p jog-p buttons-p) previous)
               ((ring-c jog-c buttons-c) current)
               (jog-diff (wrapdiff jog-p jog-c)))
    (when (not= ring-p ring-c)
      (handle-event "ring-absolute" ring-c)
      (handle-event "ring-relative" (- ring-c ring-p)))
    (if (nonzero? jog-diff) (handle-event "jog" jog-diff))
    (for-each
      (match-lambda ((p c i)
        (if (not= p c) (handle-event "button" i c))))
      (zip buttons-c buttons-p '(1 2 3 4 5)))))

; Attempt to find and open the ShuttleXpress.
; Returns #f on failure.
(define (shuttle-fd)
  (and (file-read-access? DEVICE)
       (file-open DEVICE open/rdonly)))

; Busy loop on select while waiting for input
(define (process-input fd)
  (let loop ((previous #f))
    (file-select fd #f)
    (handle-exceptions exn
      (if ((condition-predicate 'i/o) exn)
        (file-close fd)
        (abort exn))
      (let ((current (shuttle-state (read-packet fd))))
        (if previous (compare-states previous current))
        (loop current)))))

; Process input from the connected device.
(let loop ((fd (shuttle-fd)))
  (when fd
    (print "Device found")
    (process-input fd)
    (print "Device unplugged"))
  (sleep 1)
  (loop (shuttle-fd)))
