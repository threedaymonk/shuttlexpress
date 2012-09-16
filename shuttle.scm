(use posix srfi-1 matchable)
(include "support.scm")

(define PACKET-LENGTH 5)
(define SHUTTLE-ID #x0b330020)
(define DEVICE "/dev/shuttlexpress")

; Does this file descriptor refer to a ShuttleXpress?
(define (is-shuttle? fd)
  (= SHUTTLE-ID (hid-device-id fd)))

; Read a packet from the controller and return a list of byte values
(define (read-packet fd)
  (map char->integer
       (string->list (first (file-read fd PACKET-LENGTH)))))

; Return a list of the buttons: 1 for pressed, 0 for released
; As there's no overlap between the bit masks for the two bytes used to
; indicate button presses, we can just add them together
(define (button-states packet)
  (let ((bitfield (+ (fourth packet) (fifth packet)))
        (masks '(#x10 #x20 #x40 #x80 #x1)))
    (map (lambda (mask) (/ (bitwise-and mask bitfield) mask)) masks)))

; Current position of the ring (-7 to 7)
(define (ring-position packet) (decode-2s-c 8 (first packet)))

; Current position of the jog wheel (0 to 255, rolling over)
(define (jog-position packet) (second packet))

; Wrap all the inputs into a single list
(define (shuttle-state packet)
  (list (ring-position packet) (jog-position packet) (button-states packet)))

; Since the jog wheel always tells us that it has changed by one, we only need
; two bits of data to work out which direction it moved in.
(define (wrapdiff prev curr)
  (let* ((p (modulo prev 4))
         (c (modulo curr 4)))
    (decode-2s-c 2 (modulo (- c p) 4))))

; Compare the previous and current states, work out what's changed, and do
; something about it.
(define (compare-states previous current)
  (match-let* (((ring-p jog-p buttons-p) previous)
               ((ring-c jog-c buttons-c) current)
               (jog-diff (wrapdiff jog-p jog-c)))
    (if (not= ring-p ring-c) (print "ring " ring-c))
    (if (nonzero? jog-diff) (print "jog " jog-diff))
    (for-each
      (match-lambda ((p c i)
        (if (not= p c) (print "button " i " " c))))
      (zip buttons-c buttons-p '(1 2 3 4 5)))))

; Attempt to find and open the ShuttleXpress.
; Returns #f on failure.
(define (shuttle-fd)
  (if (file-read-access? DEVICE)
    (file-open DEVICE open/rdonly)
    #f))

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
