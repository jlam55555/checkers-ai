(define-record-type state
  (fields boards pieces turn)
)

(define (state-copy state turn)
  (let (
    [new-boards (make-bytevector 8 0)]
    [old-boards (state-boards state)]
    [new-pieces (make-bytevector 24 0)]
    [old-pieces (state-pieces state)]
  )
    ; copy over the board
    (bytevector-u64-set! new-boards 0
      (bytevector-u64-ref old-boards 0 'little) 'little)

    ; copy over the pieces
    (bytevector-u64-set! new-pieces 0
      (bytevector-u64-ref old-pieces 0 'little) 'little)
    (bytevector-u64-set! new-pieces 8
      (bytevector-u64-ref old-pieces 8 'little) 'little)
    (bytevector-u64-set! new-pieces 16
      (bytevector-u64-ref old-pieces 16 'little) 'little)

    ; return new object
    (make-state new-boards new-pieces (or turn (state-turn state)))
  )
)

(define (board-get boards player)
  (bytevector-u32-ref boards (fxlogor (fxsll player 2)) 'little)
)

(define (board-set! boards player board)
  (bytevector-u32-set! boards (fxlogor (fxsll player 2)) board 'little)
)

(define (board-shift row col)
  (fxlogor (fxsll row 2) (fxsrl col 1))
)

(define (board-test-pos? board row col)
  (fxlogbit? (board-shift row col) board)
)

(define (board-set-pos board row col)
  (fxlogbit1 (board-shift row col) board)
)

(define (board-clear-pos board row col)
  (fxlogbit0 (board-shift row col) board)
)

(define (piece-get pieces player i)
  (bytevector-u8-ref pieces (fx+ (if (fxzero? player) 0 12) i))
)

(define (piece-set! pieces player i piece)
  (bytevector-u8-set! pieces (fx+ (if (fxzero? player) 0 12) i) piece)
)

; returns position and piece, if found
(define (piece-find pieces player row col)
  ; use let* here because of dependence of piece on i;
  ; not sure if do guarantees this
  (let loop ([i 0])
    (let ([piece (piece-get pieces player i)])
      (if
        (or (fxzero? piece)
          (and (fx=? (piece-row piece) row) (fx=? (piece-col piece) col)))
        (cons i piece)
        (loop (fx1+ i))
      )
    )
  )
)

; only used when constructing the board
(define (piece-add! pieces player row col type)
  (piece-set! pieces player
    (car (piece-find pieces player 0 0))
    (piece-encode row col type))
)

(define (piece-encode row col type)
  (fxlogor (fxsll type 7) (fxsll row 3) col)
)

(define (piece-type piece)
  (fxsrl piece 7)
)

(define (piece-row piece)
  (fxlogand (fxsrl piece 3) #b0111)
)

(define (piece-col piece)
  (fxlogand piece #b0111)
)
