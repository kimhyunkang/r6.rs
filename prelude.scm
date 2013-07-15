(begin
  (define (list . a) a)
  (define (cons x y) `(,x . ,y))
)
