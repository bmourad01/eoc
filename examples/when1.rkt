(let ([x (read)])
  (begin
    (when (< x 0)
      (set! x (- x))
      (set! x (+ x 1)))
    x))
