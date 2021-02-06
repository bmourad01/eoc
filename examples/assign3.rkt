(let* ([x 10] [y 0])
  (begin
    (+ (+ (begin (set! y (read)) x)
          (begin (set! x (read)) y))
       x)))
