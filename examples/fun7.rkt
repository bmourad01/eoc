(define (abs-int [x : Integer]) : Integer
  (if (< x 0) (- x) x))

(define (even? [x : Integer]) : Boolean
  (let ([x (abs-int x)])
    (if (eq? x 0) #t
        (odd? (- x 1)))))

(define (odd? [x : Integer]) : Boolean
  (let ([x (abs-int x)])
    (if (eq? x 0) #f
        (even? (- x 1)))))

(even? (read))
