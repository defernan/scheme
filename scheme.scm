#lang scheme
(define hello-world
  (lambda()
    (begin
      ;comment
      (write `Hello-World)
      (newline)
      (hello-world))))

