#lang scheme
;;HIGHER ORDER EXAMPLES
;;map example
(define double
  (lambda(x)
    (* 2 x)))

(define doubleList
  (lambda(xs)
    (map double xs)))

"Double list (1 2 3 4 5)"
(doubleList '(1 2 3 4 5))

;;filter example
(define positiveList
  (lambda(xs)
    (filter positive? xs)))

"Filter positives on list (-2 3 -4 5 -1 -3 8)"
(positiveList '(-2 3 -4 5 -1 -3 8))

;;lexical closure example
(define (create-account balance)
  (lambda (n)
    (set! balance (+ balance n))
    balance))
"create my account with 20, call (define my_account (create-account 20))"
(define my_account (create-account 20))
"call (my_account 20)"
(my_account 20)
"call (my_account -40)"
(my_account -40)

