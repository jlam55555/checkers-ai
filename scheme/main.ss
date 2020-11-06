(load "board-ops.ss")
(load "move-ops.ss")
(load "board-macros.ss")
(load "valid-moves.ss")
(load "minimax.ss")
(load "draw.ss")

; recursively print out move history
(define (print-move i move)
  (printf "~2,' d: ~a\n"
    i
    (let ([path-string
      (let loop ([move move] [res ""])
        (if (null? move)
          res
          (loop
            (move-prev move)
            (string-append
              (format "(~a,~a)->"
                (piece-row (move-pos move)) (piece-col (move-pos move)))
              res))
        ))
    ])
      (substring path-string 0 (fx- (string-length path-string) 2))
    )
  )
)

; helper for get-game config
(define (in-list? in lst)
  (ormap (lambda (elem) (string=? in elem)) lst)
)

(define (get-game-config)
  ; eat incoming newline if it's there
  (when (and (char-ready? (current-input-port)) (char=? (peek-char) #\newline))
    (get-line (current-input-port)))
  (let* (
    [black-is-comp? [begin
      (printf "Black is computer? [y]/n: ")
      (not (in-list? (get-line (current-input-port)) '("n" "N")))
    ]]
    [red-is-comp? [begin
      (printf "Red is computer? [y]/n: ")
      (not (in-list? (get-line (current-input-port)) '("n" "N")))
    ]]
    [state-timeout [begin
      (printf "Read from file? y/[n]: ")
      (if (in-list? (get-line (current-input-port)) '("y" "Y"))
        [begin
          (printf "State file: ")
          (load-state
            (open-input-file (get-line (current-input-port))) #f)
        ]
        [begin
          (printf "Timeout (s) [3]: ")
          (let ([timeout (get-line (current-input-port))])
            (load-state
              (empty-state-port)
              (if (string=? timeout "") 3 (string->number timeout)))
          )
        ]
      )
    ]]
  )
    (values
      (list black-is-comp? red-is-comp?)
      (car state-timeout)
      (cdr state-timeout))
  )
)

(define (play-game)
  ; game setup
  (let-values (
    [(player-is-comp start-state timeout) (get-game-config)]
  )

    ; main game loop (1 turn = 1 iteration)
    (let game-loop ([game-history (list start-state)] [turns 0])
      (let* (
        [state (car game-history)]
        [valid-move-list (valid-moves state)]
      )
        (print-board state)
        (if (null? valid-move-list)
          (printf "Game over\n")
          [begin
            (printf
              "Turn ~a\n-3: Quit\n-2: Undo last move\n-1: Iterative deepening best move\n"
              turns)
            (let valid-move-loop (
              [move-iter valid-move-list]
              [i 0]
            )
              ; print out valid moves
              (unless (null? move-iter)
                (print-move i (car move-iter))
                (valid-move-loop (cdr move-iter) (fx1+ i)))
            )

            (if (list-ref player-is-comp (state-turn state))
              ; if computer move
              (let ([best-move (iterative-deepening-search state timeout)])
                (printf "Best move: ")
                (print-move -1 best-move)

                (game-loop
                  (cons (move-state best-move) game-history) (fx1+ turns))
              )

              ; if player move
              (let input-loop ()
                (printf "Enter your move: ")
                (let ([in (string->number (get-line (current-input-port)))])
                  (cond
                    ; invalid move
                    [(or
                        (not (fixnum? in))
                        (and (fx=? in -2) (fx<? turns 2))
                        (fx<? in -3)
                        (fx>=? in (length valid-move-list)))
                      (input-loop)
                    ]
                    ; exit condition
                    [(fx=? in -3) (void)]
                    ; go back a move
                    [(fx=? in -2)
                      (game-loop (cddr game-history) (fx- turns 2))
                    ]
                    ; let minimax choose
                    [(fx=? in -1)
                      (let (
                        [best-move (iterative-deepening-search state timeout)]
                      )
                        (printf "Iterative-deepening chosen move: ")
                        (print-move -1 best-move)
                        (game-loop
                          (cons
                            (move-state best-move)
                            game-history)
                          (fx1+ turns))
                      )
                    ]
                    ; regular move
                    [#t
                      (game-loop
                        (cons
                          (move-state (list-ref valid-move-list in))
                          game-history)
                        (fx1+ turns))
                    ]
                  )
                )
              )
            )
          ]
        )
      )
    )
  )
)
