#lang scheme
(define merge-lists
	(lambda (list1 list2)
		(if (null? list1)
			list2
			(if (< (car list1) (car list2))
				(cons (car list1) (merge-lists (cdr list1) list2))
				(cons (car list2) (merge-lists (cdr list2) list1))))))

(define (insert x lst)
  (if (null? lst)
      (list x)
      (let ((y (car lst))
            (ys (cdr lst)))
        (if (<= x y)
            (cons x lst)
            (cons y (insert x ys))))))
 
(define (insertion-sort lst)
  (if (null? lst)
      '()
      (insert (car lst)
              (insertion-sort (cdr lst)))))
 


(define read-word
  (lambda (p)
    (list->string
      (let f ()
        (let ((c (peek-char p)))
          (cond
            ((eof-object? c) '())
            ((char-alphabetic? c)
             (read-char p)
             (cons c (f)))
            (else '())))))))

"call (merge-lists '(1 5 7 9) '(2 3 4 6))"
(merge-lists '(1 5 7 9) '(2 3 4 6))
