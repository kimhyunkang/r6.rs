(begin
  (define (list . a) a)

  (define (cons x y) `(,x . ,y))

  (define (list? x) 
    (or (null? x)
        (and (pair? x) (list? (cdr x)))
    )
  )
)
