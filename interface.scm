(use posix extras srfi-1 srfi-4 matchable)
(include "support.scm")

(define PACKET-LENGTH 5)
(define DEVICE "/dev/shuttlexpress")

; Read a packet from the controller and return a list of byte values.
; Returns #f if we didn't get a complete packet: this should only happen on
; EOF, that is, the device was unplugged.
(define (read-packet port)
  (let* ((packet (read-u8vector PACKET-LENGTH port))
         (bytes-read (u8vector-length packet)))
    (and (= bytes-read PACKET-LENGTH) packet)))

; Return a list of the buttons: 1 for pressed, 0 for released.
; As there's no overlap between the bit masks for the two bytes used to
; indicate button presses, we can just add them together.
(define (button-states packet)
  (let ((bitfield (+ (u8vector-ref packet 3) (u8vector-ref packet 4)))
        (masks '(#x10 #x20 #x40 #x80 #x1)))
    (map (lambda (mask) (/ (bitwise-and mask bitfield) mask)) masks)))

; Current position of the ring (-7 to 7).
(define (ring-position packet) (twoc->signed 8 (u8vector-ref packet 0)))

; Current position of the jog wheel (0 to 255, rolling over).
(define (jog-position packet) (u8vector-ref packet 1))

; Wrap all the inputs into a single list.
(define (shuttle-state packet)
  (list (ring-position packet) (jog-position packet) (button-states packet)))

; The jog position is an integer between 0 and 255, so we must compare it with
; the previous value to work out if it has moved. Taking the difference modulo
; 256 as a two's complement value gives us a relative movement that wraps
; around 255/0.
(define (wrapdiff prev curr)
  (twoc->signed 8 (modulo (- curr prev) 256)))

; Compare the previous and current states, work out what's changed, and do
; something about it.
(define (compare-states previous current handler)
  (match-let* (((ring-p jog-p buttons-p) previous)
               ((ring-c jog-c buttons-c) current)
               (jog-diff (wrapdiff jog-p jog-c)))
    (when (not= ring-p ring-c)
      (handler `("ring-absolute" ,ring-c))
      (handler `("ring-relative" ,(- ring-c ring-p))))
    (if (nonzero? jog-diff) (handler `("jog" ,jog-diff)))
    (for-each
      (match-lambda ((p c i)
        (if (not= p c) (handler `("button" ,i ,c)))))
      (zip buttons-c buttons-p '(1 2 3 4 5)))))

; Attempt to find and open the ShuttleXpress.
; Returns #f on failure.
(define (shuttle-port)
  (and (file-read-access? DEVICE)
       (open-input-file DEVICE)))

; Wait (blocking) for a packet, then process it.
(define (process-input port handler)
  (let loop ((previous #f))
    (let ((packet (read-packet port)))
      (if packet
        (let ((current (shuttle-state packet)))
          (if previous (compare-states previous current handler))
          (loop current))
        (close-input-port port)))))

; Process input from the connected device.
(define (handle-shuttle-events handler)
  (let loop ((port (shuttle-port)))
    (when port
      (print "Device found")
      (process-input port handler)
      (print "Device unplugged"))
    (sleep 1)
    (loop (shuttle-port))))
