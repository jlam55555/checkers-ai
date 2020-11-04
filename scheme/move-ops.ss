; a move will just be a bytevector
(define-record-type move
  (fields state start pos prev)
)

; note: doesn't advance state turn
(define (move-new state start-row start-col)
  (let ([pos (piece-encode start-row start-col 0)])
    (make-move (state-copy state #f) pos pos '())
  )
)

; copies move state and advances state turn to next step
; this is mainly forcing the state to be copied;
; captured and target should be (i, j) conses;
; capture can be arbitrary if on a non-capture move
(define (move-copy move target turn)
  (make-move
    (state-copy (move-state move) turn)
    (move-start move)
    (piece-encode (car target) (cdr target) 0)
    move
  )
)
