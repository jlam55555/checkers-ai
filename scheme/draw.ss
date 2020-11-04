(define unicode-man-p1 (string #\x25ef))
(define unicode-man-p2 (string #\x2b24))

(define unicode-king-p1 (string #\x2654))
(define unicode-king-p2 (string #\x265a))

(define (black-on-white str)
  (format "~c[30;107m~a~c[0m" #\esc str #\esc)
)

(define (white-on-black str)
  (format "~c[40;97m~a~c[0m" #\esc str #\esc)
)
