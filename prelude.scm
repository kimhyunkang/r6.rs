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

  (define (list-tail x k)
    (if (zero? k)
        x
        (list-tail (cdr x) (- k 1)))
  )

  (define (list-ref x k)
    (if (zero? k)
        (car x)
        (list-ref (cdr x) (- k 1)))
  )

  (define (memq obj x)
    (cond ((null? x) #f)
          ((eq? obj (car x)) x)
          (else (memq obj (cdr x))))
  )

  (define (memv obj x)
    (cond ((null? x) #f)
          ((eqv? obj (car x)) x)
          (else (memv obj (cdr x))))
  )

  (define (member obj x)
    (cond ((null? x) #f)
          ((equal? obj (car x)) x)
          (else (member obj (cdr x))))
  )

  (define (assq obj alist)
    (if (null? alist)
        #f
        (let ((p (car alist)))
          (if (eq? obj (car p))
              p
              (assq obj (cdr alist)))))
  )

  (define (assv obj alist)
    (if (null? alist)
        #f
        (let ((p (car alist)))
          (if (eqv? obj (car p))
              p
              (assv obj (cdr alist)))))
  )

  (define (assoc obj alist)
    (if (null? alist)
        #f
        (let ((p (car alist)))
          (if (equal? obj (car p))
              p
              (assoc obj (cdr alist)))))
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
          (cond ((null? t) h)
                ((null? h) (apply append t))
                (else (cons (car h) (apply append (cons (cdr h) t))))
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
)
