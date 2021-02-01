(let ([x (read)])
  (begin
    (unless (< x 0)
      (set! x (- x))
      (set! x (+ x 1)))
    x))
