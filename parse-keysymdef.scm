(use irregex srfi-13 extras)

(define port (open-input-file "/usr/include/X11/keysymdef.h"))

(print "(define keysymdef (alist->hash-table '(")

(let loop ()
  (let* ((line (read-line port))
         (more (not (eof-object? line)))
         (m (and more
                 (irregex-search
                    '(: "#define" (+ whitespace) "XK_"
                        ($ (+ (or alphanumeric "_")))
                        (+ whitespace) "0x"
                        ($ (+ hex-digit)))
                    line))))
    (when m
      (printf "(~S . #x~A)\n"
              (irregex-match-substring m 1)
              (irregex-match-substring m 2)))
    (if more (loop))))

(print ")))")
