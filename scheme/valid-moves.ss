; attempts to capture; returns the move if successful, empty list if not
(define (attempt-capture row col type player dir-row dir-col move piece-pos)
  (let (
    [boards (state-boards (move-state move))]
    [opponent (fx- 1 player)]
    [capture (cons (fx+ row dir-row) (fx+ col dir-col))]
    [target (cons (fx+ row (fxsll dir-row 1)) (fx+ col (fxsll dir-col 1)))]
  )
    (if (and
      ; don't go out of bounds
      (if (fx=? dir-row 1) (fx< row 6) (fx> row 1))
      (if (fx=? dir-col 1) (fx< col 6) (fx> col 1))
      ; go in correct direction
      (or (fx=? type 1) (fx=? player (if (fx=? dir-row 1) 0 1)))
      ; target spot to is empty (neither player can be there)
      (not
        (board-test-pos?
          (fxlogor (board-get boards 0) (board-get boards 1))
          (car target)
          (cdr target)))
      ; capture spot has opponent piece
      (board-test-pos?
        (board-get boards opponent) (car capture) (cdr capture))
    )
      ; move is valid, generate and return new move (as list)
      (let* (
        [new-move (move-copy move target opponent)]
        [boards (state-boards (move-state new-move))]
        [pieces (state-pieces (move-state new-move))]
      )
        ; update board: remove old player position
        (board-set! boards player
          (board-clear-pos (board-get boards player) row col))
        ; update board: put player piece in new position
        (board-set! boards player
          (board-set-pos (board-get boards player)
            (car target) (cdr target)))
        ; update board: remove capture piece
        (board-set! boards opponent
          (board-clear-pos (board-get boards opponent)
            (car capture) (cdr capture)))
        ; update pieces: move player piece and promote to king if necessary
        (piece-set! pieces player piece-pos
          (piece-encode
            (car target) (cdr target)
            (if (or (fx= (car target) 7) (fx= (car target) 0)) 1 type)))
        ; update pieces: remove opponent piece and shift other pieces left
        ; tight for loop here but it's okay because of small size
        ; eww this code is spaghetti
        (let (
          [pos (car (piece-find pieces opponent (car capture) (cdr capture)))]
        )
          (do ([i pos (fx1+ i)]) [
            (or (fx=? i 11) (fxzero? (piece-get pieces opponent i))) 0
          ]
            (piece-set! pieces opponent i (piece-get pieces opponent (fx1+ i)))
          )
          (piece-set! pieces opponent 11 0)
        )
        ; return new move as a list
        (list new-move)
      )
      ; if not valid, return empty list
      '()
    )
  )
)

; nocapture move
(define (attempt-nocapture row col type player dir-row dir-col move piece-pos)
  (let (
    [boards (state-boards (move-state move))]
    [target (cons (fx+ row dir-row) (fx+ col dir-col))]
    [opponent (fx- 1 player)]
  )
    (if (and
      ; don't go out of bounds
      (if (fx=? dir-row 1) (fx< row 7) (fx> row 0))
      (if (fx=? dir-col 1) (fx< col 7) (fx> col 0))
      ; go in correct direction
      (or (fx=? type 1) (fx=? player (if (fx=? dir-row 1) 0 1)))
      ; target spot is empty
      (not (board-test-pos?
        (fxlogor (board-get boards 0) (board-get boards 1))
        (car target) (cdr target)))
    )
      ; move is valid, generate and return new move (as list)
      (let* (
        [new-move (move-copy move target opponent)]
        [boards (state-boards (move-state new-move))]
        [pieces (state-pieces (move-state new-move))]
      )
        ; update board: remove old player position
        (board-set! boards player
          (board-clear-pos (board-get boards player) row col))
        ; update board: put player piece in new position
        (board-set! boards player
          (board-set-pos (board-get boards player)
            (car target) (cdr target)))
        ; update pieces, promote to king if necessary
        (piece-set! pieces player piece-pos
          (piece-encode
            (car target) (cdr target)
            (if (or (fx= (car target) 7) (fx= (car target) 0)) 1 type)))
        ; return new move as a list
        (list new-move)
      )
      ; if not valid, return empty list
      '()
    )
  )
)

; returns list of valid moves
(define (valid-moves state)
  (let* (
    [player (state-turn state)]
    [pieces (state-pieces state)]
    [boards (state-boards state)]
  )
    (let ([valid-move-list
      ; iterate through valid captures
      (let piece-loop ([i 0] [valid-move-list '()])
        (let ([piece (piece-get pieces player i)])
          ; loop break condition
          (if (fxzero? piece)
            valid-move-list
            (let* (
              [start-row (piece-row piece)]
              [start-col (piece-col piece)]
              [type (piece-type piece)]
            )
              ; hide starting position on board temporarily; maybe put this in
              ; the let, but would be a little more complicated since you have
              ; to make a copy of boards; this is more empirical but
              ; well-controlled (I hope) and should be a little faster
              (board-set! boards player
                (board-clear-pos (board-get boards player) start-row start-col)
              )

              (let ([valid-move-list
                ; find capture positions!
                (let capture-loop (
                  [move-stack (list (move-new state start-row start-col))]
                  [valid-move-list valid-move-list]
                )
                  (if (null? move-stack)
                    ; retval of this loop is valid-move-list
                    valid-move-list
                    (let* (
                      [move (car move-stack)]
                      [row (piece-row (move-pos move))]
                      [col (piece-col (move-pos move))]
                      [found-moves (append
                        (attempt-capture row col type player -1 -1 move i)
                        (attempt-capture row col type player -1 +1 move i)
                        (attempt-capture row col type player +1 -1 move i)
                        (attempt-capture row col type player +1 +1 move i)
                      )]
                    )
                      (capture-loop
                        (append found-moves (cdr move-stack))
                        ; valid move if not initial state and no further moves
                        (if (and (null? found-moves)
                          (not (null? (move-prev move)))
                        )
                          (cons move valid-move-list)
                          valid-move-list)
                      )
                    )
                  )
                )
              ])
                ; restore starting position on board
                (board-set! boards player
                  (board-set-pos (board-get boards player) start-row start-col)
                )
                ; loop
                (if (fx< i 11)
                  (piece-loop (fx1+ i) valid-move-list)
                  valid-move-list)
              )
            )
          )
        )
      )
    ])
    ; END CAPTURE MOVES

      ; only if no capture moves were found, then try to attempt non-capture
      ; moves; otherwise pass through valid move list
      (if (null? valid-move-list)
        (let piece-loop ([i 0] [valid-move-list '()])
          ; first loop break condition
          (if (fx=? i 12)
            valid-move-list
            (let ([piece (piece-get pieces player i)])
              ; other loop break condition
              (if (fxzero? piece)
                valid-move-list
                (let* (
                  [row (piece-row piece)]
                  [col (piece-col piece)]
                  [type (piece-type piece)]
                  [move (move-new state row col)]
                )
                  ; attempt to move in all four directions
                  (piece-loop
                    (fx1+ i)
                    (append
                      (attempt-nocapture row col type player -1 -1 move i)
                      (attempt-nocapture row col type player -1 +1 move i)
                      (attempt-nocapture row col type player +1 -1 move i)
                      (attempt-nocapture row col type player +1 +1 move i)
                      valid-move-list))
                )
              )
            )
          )
        )
        valid-move-list
      )
    )
  )
)
