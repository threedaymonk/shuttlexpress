(module support (not= nonzero? twoc->signed)
(import chicken scheme)
(use data-structures)

(define not= (compose not =))
(define nonzero? (compose not zero?))

; Interpret a two's complement value stored in an unsigned integer as a
; signed value
(define (twoc->signed bits value)
  (let ((threshold (expt 2 (- bits 1)))
        (adjustment (expt 2 bits)))
    (if (< value threshold) value (- value adjustment))))

)
