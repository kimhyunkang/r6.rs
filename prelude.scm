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
)