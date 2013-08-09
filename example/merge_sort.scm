(define (merge-sort ls)
  (cond [(null? ls) ls]
        [(null? (cdr ls)) ls]
        [else (let* [(p (let split ((xs ls))
                          (cond [(null? xs)
                                  (cons null null)]
                                [(null? (cdr xs))
                                  (cons (list (car xs)) null)]
                                [else 
                                  (let [(p (split (cddr xs)))]
                                    (cons
                                      (cons (car xs) (car p))
                                      (cons (cadr xs) (cdr p))))
                                  ])))
                      (xs (car p))
                      (ys (cdr p))]
                (let merge [(xs (merge-sort xs))
                            (ys (merge-sort ys))]
                  (cond [(null? xs) ys]
                        [(null? ys) xs]
                        [(< (car xs) (car ys)) (cons (car xs) (merge (cdr xs) ys))]
                        [else (cons (car ys) (merge xs (cdr ys)))]
                        )))
        ]))
