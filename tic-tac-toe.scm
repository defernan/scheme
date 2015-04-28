#lang scheme

; Tic-tac-toe
;
; credit: https://torquingwetstrainers.wordpress.com/2010/02/24/tic-tac-toe-scheme/

; Define the players
(define o 'O)
(define x 'X)


; Switch player
(define (next-player player)
  (case player
    ((X) o)
    ((O) x)))


; New game board
(define new-board
  '((1 2 3)
    (4 5 6)
    (7 8 9)))


; Check if the board space is already taken
(define (space-available? space board)
  ; This is like let-in in Haskell -- checks each row to see if the space is a
  ; member of the row. If so, the space is available, else, it is taken.
  (let ((member-space (lambda (row) (member space row))))
    (ormap member-space board)))


; Make a move -- returns the transformed board
; Space should be guaranteed to be available by calling space-available? first
(define (move player space board)
  ; This function traverses each space in a row looking for 'space'. If found,
  ; it replaces it with the player's mark
  (letrec ((move-in-row
           (lambda (row)
             (cond
               ; reached the end of the row, so return empty list and end search
               [(null? row) '()]
               ; found the space, so prepend player's mark instead of 'space' and
               ; end search
               [(eqv? space (first row)) (cons player (rest row))]
               ; not found, so prepend whatever was already there and
               ; continue search
               [else (cons (first row) (move-in-row (rest row)))]))))
    ; Try the move in each row
    (map move-in-row board)))


; Transforms board to string representation
(define (board->string board)
  (let* ([space->string
          ; transform each space into "(#)" or " x " depending on whether the space
          ; is available
          (lambda (space)
            (cond
              [(number? space) "   "]
              [(symbol? space) (string-append " " (symbol->string space) " ")]
              [else ""]))]
         [row->string
          ; transform each row by adding '|' between spaces
          (lambda (row) (string-append* (add-between (map space->string row) "|")))])
    ; map each row to its string representation and add a border in between
    (string-append* (add-between (map row->string board) "\n---+---+---\n"))))


; Checks whether either player has won. Returns the winning player or false
; if there is none
(define (winner? board)
  ; checks whether all elements in a list are equal. If so, returns the value,
  ; else, returns false
  (let ([all-eq?
         (lambda (lst) 
           (foldl (lambda (x y) (if (eq? x y) x #f)) (first lst) (rest lst)))])
    ; returns the first non-false value from the list, which would indicate the
    ; winning player, or false if none are found
    (findf (lambda (x) x)
           ; generates a list of values corresponding to each row, column, and diagonal:
           ;    if each element in the row, col, diag is equal -> player
           ;    else -> false
           (list
            ; rows
            (all-eq? (first board))
            (all-eq? (second board))
            (all-eq? (third board))
            ; columns
            (all-eq? (map first board))
            (all-eq? (map second board))
            (all-eq? (map third board))
            ; diagonals
            (all-eq? (list (first (first board))
                           (second (second board)) 
                           (third (third board))))
            (all-eq? (list (third (first board)) 
                           (second (second board)) 
                           (first (third board))))))))


; Checks for a completely filled board
(define (full-board? board)
  (not (findf number? (flatten board))))


; Displays the board to the screen
(define (display-board board)
  ; begin is like 'do' in Haskell
  (begin
    (display "\n")
    (display (board->string board))
    (display "\n\n")))


; Driver for the game
(define (play board player)
  (begin
    ; display the board
    (display-board board)
    ; check for game over
    (let ((winning-player (winner? board)))
      (cond
        ; we have  a winner
        [winning-player (display (string-append (symbol->string winning-player) " wins!\n"))]
        ; it's a draw
        [(full-board? board) (display "It's a draw!\n")]
        ; continue game
        [else (begin
                (display (string-append "Select a square (1 - 9), " (symbol->string player) ": "))
                ; get next move selection
                (let ((answer (string->number (regexp-replace* #px"\\s*" (read-line) ""))))
                  (if (and answer (space-available? answer board))
                      ; valid
                      (play (move player answer board) (next-player player))
                      ; invalid
                      ((begin
                         (display "\nInvalid choice\n")
                         (play board player))))))]))))


; Start the game
(play new-board x)