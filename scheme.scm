#lang scheme
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