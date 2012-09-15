(use posix srfi-1)
(include "hid.scm")

(define PACKET-LENGTH 5)
(define SHUTTLE-ID #x0b330020)

; Interpret a two's complement value stored in an unsigned integer as a
; signed value
(define (decode-2s-c bits value)
  (let ((threshold (expt 2 (- bits 1)))
        (adjustment (expt 2 bits)))
    (if (< value threshold) value (- value adjustment))))

; Does this file descriptor refer to a ShuttleXpress?
(define (is-shuttle? fd)
  (= SHUTTLE-ID (hid-device-id fd)))

; Read a packet from the controller and return a list of byte values
(define (read-packet fd)
  (map char->integer
       (string->list (car (file-read fd PACKET-LENGTH)))))

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
  (list (ring-position packet)
        (jog-position packet)
        (button-states packet)))

; Since the jog wheel always tells us that it has changed by one, we only need
; two bits of data to work out which direction it moved in.
(define (wrapdiff prev curr)
  (let* ((p (modulo prev 4))
         (c (modulo curr 4))
         (r '(0 1 0 -1)))
    (decode-2s-c 2 (modulo (- c p) 4))))

; Compare the previous and current states, work out what's changed, and do
; something about it.
(define (compare-states prev curr)
  (let* ((ring-moved? (not (= (first curr) (first prev))))
         (jog-diff (wrapdiff (second prev) (second curr)))
         (jog-moved? (not (zero? jog-diff))))
    (if ring-moved? (print "ring " (first curr)))
    (if jog-moved? (print "jog " jog-diff))
    (for-each
      (lambda (pci) ; Oh I wish I had a destructuring bind
        (let ((p (first pci))
              (c (second pci))
              (i (third pci)))
          (if (not (= p c)) (print "button " i " " c))))
      (zip (third prev) (third curr) '(1 2 3 4 5)))))

; Open every accessible device matching pattern and return the file descriptor
; for those that are ShuttleXpress-es
(define (shuttle-fds pattern)
  (filter is-shuttle?
          (map (lambda (path) (file-open path open/rdonly))
               (filter file-read-access? (glob pattern)))))

; Busy loop on select while waiting for input
(define (process-input fd)
  (let loop ((previous #f))
    (file-select fd #f)
    (let ((current (shuttle-state (read-packet fd))))
      (if previous (compare-states previous current))
      (loop current))))

; Find the first accessible ShuttleXpress (if any) and process input
(let ((fds (shuttle-fds "/dev/hidraw*")))
  (if (null? fds)
    (begin
      (print "No ShuttleXpress devices found")
      (exit))
    (process-input (car fds))))
