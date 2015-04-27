#lang scheme
;;map example
(define double
  (lambda(x)
    (* 2 x)))

(map double '(2 3 4 5))
(define doubleList
  (lambda(xs)
    (map double xs)))
