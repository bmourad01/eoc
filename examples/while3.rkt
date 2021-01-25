(define (never-terminate) : Integer
  (let ([x 5])
    (begin
      (while #t
        (set! x (+ x 1)))
      x)))

(define (always-terminate) : Integer
  (let ([x 5])
    (begin
      (while #f
        (set! x (+ x 1)))
      x)))

(if (eq? (read) 0)
    (never-terminate)
    (always-terminate))
      
