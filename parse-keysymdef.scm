(use irregex srfi-13 extras)

(define port (open-input-file "/usr/include/X11/keysymdef.h"))
(define definition-re
  '(: "#define" (+ whitespace)
      "XK_" ($ (+ (or alphanumeric "_"))) (+ whitespace)
      "0x" ($ (+ hex-digit))))

(print "(define keysymdef (alist->hash-table '(")

(let loop ()
  (let* ((line (read-line port))
         (line (if (eof-object? line) #f line))
         (m (and line (irregex-search definition-re line))))
    (when m
      (printf "(~S . #x~A)\n"
              (irregex-match-substring m 1)
              (irregex-match-substring m 2)))
    (if line (loop))))

(print ") string=? string-hash 2500))")
