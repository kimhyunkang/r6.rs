(begin
  (define (list . a) a)

  (define (list? x) 
    (or (null? x)
        (and (pair? x) (list? (cdr x)))
    )
  )

  (define (apply f args) 
    (eval (cons f args))
  )

  (define (length lst)
    (if (null? lst)
        0
        (+ 1 (length (cdr lst)))
    )
  )

  (define (odd? n)
    (= (modulo n 2) 1)
  )

  (define (even? n)
    (= (remainder n 2) 0)
  )

  (define (filter pred lst)
    (if (null? lst)
        '()
        (let ((h (car lst))
              (t (filter pred (cdr lst))))
          (if (pred h)
              (cons h t)
              t)
        )
    )
  )
)
