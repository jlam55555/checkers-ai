(define (print-board state)
  (printf "\n\n\nPlayer ~a's turn:\n" (fx1+ (state-turn state)))
  (let (
    [boards (state-boards state)]
    [pieces (state-pieces state)]
  )
    (printf "   0  1  2  3  4  5  6  7\n")
    (do ([i 0 (fx1+ i)]) [(fx= i 8) 0]
      (printf "~a " i)
      (do ([j 0 (fx1+ j)]) [(fx= j 8) 0]
        (printf
          (if (fx= (fxmod i 2) (fxmod j 2))
            (black-on-white "   ")
            (white-on-black (format " ~a "
              (cond
                [(board-test-pos? (board-get boards 0) i j)
                  (if (fxzero? (piece-type (cdr (piece-find pieces 0 i j))))
                    unicode-man-p1
                    unicode-king-p1
                  )
                ]
                [(board-test-pos? (board-get boards 1) i j)
                  (if (fxzero? (piece-type (cdr (piece-find pieces 1 i j))))
                    unicode-man-p2
                    unicode-king-p2
                  )
                ]
                [#t " "]
              )
            ))
          )
        )
      )
      (printf "\n")
    )
  )
)

(define (empty-state-port)
  (open-input-string
    "1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 2 2 2 2 2 2 2 2 2 2 2 2 1"
  )
)

(define (load-state port timeout)
  (let (
    [boards (make-bytevector 8 0)]
    [pieces (make-bytevector 24 0)]
  )
    ; on exit from loop, close port and return state
    (do ([i 0 (fx1+ i)]) [(fx= i 8) 0]
      (do ([j 0 (fx1+ j)]) [(fx= j 4) 0]
        (let ([piece (read port)])
          (unless (fxzero? piece)
            (let (
              [col (fx+ (fxsll j 1) (fx- 1 (fxmod i 2)))]
              [player (fx- 1 (fxmod piece 2))]
              [type (fx/ piece 3)]
            )
              ; set updated board
              (board-set! boards player
                (board-set-pos (board-get boards player) i col)
              )
              (piece-add! pieces player i col type)
            )
          )
        )
      )
    )
    (let* ([turn (fx1- (read port))] [timeout (or timeout (read port))])
      (close-input-port port)
      (cons (make-state boards pieces turn) timeout)
    )
  )
)

