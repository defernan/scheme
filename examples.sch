(define merge-lists
	(lambda (list1 list2)
		(if (null? list1)
			list2
			(if (< (car list1) (car list2))
				(cons (car list1) (merge-lists (cdr list1) list2))
				(cons (car list2) (merge-lists (cdr list2) list1))))))

(define isort (lambda (l)
	(letrec
		((insert (lambda (x l)
			(if (null? l)
				(list x)
				(if (<= x (car l))
					(cons x l)
					(cons (car l) (insert x (cdr l))))))))
	(if (null? l)
		nil
		(insert (car l) (isort (cdr l)))))))


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
