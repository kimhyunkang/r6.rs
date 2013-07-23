(begin
  (define (list . a) a)

  (define (list? x) 
    (or (null? x)
        (and (pair? x) (list? (cdr x)))
    )
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

  (define null '())

  (define (caar x) (car (car x)))
  (define (cadr x) (car (cdr x)))
  (define (cdar x) (cdr (car x)))
  (define (cddr x) (cdr (cdr x)))

  (define (append . lss)
    (if (null? lss)
        null
        (let ((h (car lss))
              (t (cdr lss)))
          (if (null? h)
              (apply append t)
              (cons (car h) (apply append (cons (cdr h) t)))
          )
        )
    )
  )

  (define (reverse lst)
    (letrec ((f (lambda (l r)
                (if (null? l)
                    r
                    (f (cdr l) (cons (car l) r))
                )
            )))
      (f lst null)
    )
  )

  (define (hsplit lst)
    (if (null? lst)
        (cons '() '())
        (let ((this (car lst))
              (next (hsplit (cdr lst))))
          (cons
            (cons (car this) (car next))
            (cons (cdr this) (cdr next)))
        )
    )
  )
)
