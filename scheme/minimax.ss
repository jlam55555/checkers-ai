; helper for heuristic function; calculates relevant metrics
(define (get-metrics state player)
  (let loop (
    [i 0]
    [king-men-count 0]
    [men-dist-to-king 0]
    [back-row 0]
    [dist-to-corner1 0]
    [dist-to-corner2 0]
    [king-corner1? #f]
    [king-corner2? #f]
    [board-center 0]
    [piece-count 0]
  )
    (if (fx=? i 12)
      (values king-men-count men-dist-to-king back-row
        dist-to-corner1 dist-to-corner2 king-corner1?
        king-corner2? board-center piece-count)
      (let* (
        [piece (piece-get (state-pieces state) player i)]
        [row (piece-row piece)]
        [col (piece-col piece)]
        [type (piece-type piece)]
      )
        (if (fxzero? piece)
          (values king-men-count men-dist-to-king back-row
            dist-to-corner1 dist-to-corner2 king-corner1?
            king-corner2? board-center piece-count)
          (loop
            (fx1+ i)
            (fx+ king-men-count (if (fxzero? type) 3 5))
            (fx+ men-dist-to-king
              (if (fxzero? type) (if (fxzero? player) (fx- 7 row) row) 0))
            (fx+ back-row
              (if (and (fxzero? type) (fx=? row (if (fxzero? player) 0 7)))
                1 0))
            (fx+ dist-to-corner1 (fx+ row col))
            (fx+ dist-to-corner2 (fx- 14 row col))
            (or king-corner1? (fx=? (fx+ row col) 1))
            (or king-corner2? (fx=? (fx- 14 row col) 1))
            (fx+ board-center
              (if (and (fx>=? col 2) (fx<=? col 5) (fx>=? row 2) (fx<=? row 5))
                1 0))
            (fx1+ piece-count)
          )
        )
      )
    )
  )
)

; rng seed
(define x 52)

; minimax heuristic function
(define (score state player)
  (let-values (
    ; player metrics
    [(
      king-men-count-p1 men-dist-to-king-p1 back-row-p1
      dist-to-corner1-p1 dist-to-corner2-p1 king-corner1?-p1
      king-corner2?-p1 board-center-p1 piece-count-p1
    ) (get-metrics state player)]
    ; opponent metrics
    [(
      king-men-count-p2 men-dist-to-king-p2 back-row-p2
      dist-to-corner1-p2 dist-to-corner2-p2 king-corner1?-p2
      king-corner2?-p2 board-center-p2 piece-count-p2
    ) (get-metrics state (fx- 1 player))]
  )
    (let* (
      [king-men-count (fx- king-men-count-p1 king-men-count-p2)]
      [men-dist-to-king (fx- men-dist-to-king-p1 men-dist-to-king-p2)]
      [back-row (fx- back-row-p1 back-row-p2)]
      [dist-to-corner1 (fx- dist-to-corner1-p1 dist-to-corner1-p2)]
      [dist-to-corner2 (fx- dist-to-corner2-p1 dist-to-corner2-p2)]
      [board-center (fx- board-center-p1 board-center-p2)]
      [total-piece-count (fx+ piece-count-p1 piece-count-p2)]
      ; attempt trades if winning
      ; attempts to maximize the ratio of player piece counts
      [trade-affinity (cond 
        [(fxpositive? king-men-count)
          (fx/ (fx* piece-count-p1 10) piece-count-p2)]
        [(fxnegative? king-men-count)
          (fx/ (fx* piece-count-p2 -10) piece-count-p1)]
        [#t 0]
      )]
      ; pry loser out of double corners; 6 is arbitrarily chosen to represent
      ; endgame position
      [sum-distances
        (if (and (fx<? total-piece-count 6) (not (fxzero? king-men-count)))
          (let (
            [board (board-get (state-boards state)
              (if (fxpositive? king-men-count) (fx- 1 player) player))]
          )
            (cond
              [(fxlogtest board #b00000000000000000001000100110011)
                (fx- dist-to-corner1)]
              [(fxlogtest board #b11001100100010000000000000000000)
                (fx- dist-to-corner2)]
              [#t 0]
            )
          )
          0
        )
      ]
    )
      ; update rng; simple BSD style LCG
      ; (with different modulus b/c of fixnum overflow since fixnum is 60 bits)
      (set! x (fxmod (fx+ (fx* 1103515245 x) 12345) (fxsll 1 24)))
      ; final score
      (fx+
        (fx* king-men-count 100000)
        (fx* (fx- (fxsll back-row 1) men-dist-to-king) 1000)
        (fx* trade-affinity 500)
        (fx* sum-distances 100)
        (fx* board-center 10)
        (fx/ (fxmod x 100000) 10000)
      )
    )
  )
)

(define (minimax-search state search-depth check-timeout)
  (max-value state (most-negative-fixnum) (most-positive-fixnum) 0
    search-depth check-timeout)
)

(define (max-value state alpha beta depth search-depth check-timeout)
  (let ([move-iter (valid-moves state)])
    (check-timeout)
    (cond 
      ; no moves stop condition
      [(null? move-iter) (cons (fx- depth 100000000) '())]
      ; depth limit stop condition
      [(fx=? depth search-depth) (cons (score state (state-turn state)) '())]
      [#t
        ; loop through all items
        (let loop (
          [move-iter move-iter]
          [best-move '()]
          [v (most-negative-fixnum)]
          [alpha alpha]
        )
          (if (null? move-iter)
            (cons v best-move)
            (let* (
              [state (move-state (car move-iter))]
              [minv (min-value state alpha beta (fx1+ depth) search-depth
                check-timeout)]
              [best-move (if (fx<? v minv) (car move-iter) best-move)]
              [v (max v minv)]
            )
              ; alpha-beta pruning
              (if (fx>=? v beta)
                (cons (fx1+ v) '())
                (loop
                  (cdr move-iter)
                  best-move
                  v
                  (fxmax v alpha)
                )
              )
            )
          )
        )
      ]
    )
  )
)

(define (min-value state alpha beta depth search-depth check-timeout)
  (let ([move-iter (valid-moves state)])
    (check-timeout)
    (cond 
      ; no moves stop condition
      [(null? move-iter) (fx- 100000000 depth)]
      ; search depth limit reached
      [(fx=? depth search-depth)
        (score state (fx- 1 (state-turn state)))
      ]
      [#t
        ; loop through all items
        (let loop (
          [move-iter move-iter]
          [v (most-positive-fixnum)]
          [beta beta]
        )
          (if (null? move-iter)
            v
            (let* (
              [state (move-state (car move-iter))]
              [maxv (car (max-value state alpha beta (fx1+ depth) search-depth
                check-timeout))]
              [v (min v maxv)]
            )
              ; alpha-beta pruning
              (if (fx<=? v alpha)
                (fx1- v)
                (loop
                  (cdr move-iter)
                  v
                  (fxmin v beta)
                )
              )
            )
          )
        )
      ]
    )
  )
)

(define (iterative-deepening-search state timeout)
  ; quick check to see if there is only one valid move
  (let ([valid-move-list (valid-moves state)])
    (if (fx=? (length valid-move-list) 1)
      [begin
        (printf "Choosing only available move:\n")
        (car valid-move-list)
      ]
      ; start at (arbitrarily-chosen) min-depth of 6
      (let loop (
        [depth 6]
        [start-time (current-time)]
        [best-move '()]
      )
        (call/1cc
          (lambda (cont)
            (let (
              [minimax-result (minimax-search state depth
                  ; lambda for time-based short circuit
                  (lambda ()
                    (let ([elapsed (time-difference (current-time) start-time)])
                      (when
                        (and
                          (fx=? (time-second elapsed) (fx1- timeout))
                          (fx>=? (time-nanosecond elapsed) 990000000)
                        )
                        (printf "Iterative deepening time: ~as ~ams; depth: ~a\n"
                          (time-second elapsed)
                          (fx/ (time-nanosecond elapsed) 1000000)
                          (fx1- depth)
                        )
                        (cont best-move))
                    )
                  )
                )
              ]
            )
              ; if guaranteed win or lose, short circuit
              (if (fx>=? (abs (car minimax-result)) 99000000)
                (let ([elapsed (time-difference (current-time) start-time)])
                  (printf "Iterative deepening time: ~as ~ams; depth: ~a\n"
                    (time-second elapsed)
                    (fx/ (time-nanosecond elapsed) 1000000)
                    depth
                  )
                  (cdr minimax-result)
                )
                (loop (fx1+ depth) start-time (cdr minimax-result))
              )
            )
          )
        )
      )
    )
  )
)
