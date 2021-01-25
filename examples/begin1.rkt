(let ([x (read)])
  (begin
    (if (>= x 0) (set! x (- x)) (void))
    (let ([y x]) (begin (set! y (+ y 1)) y))))
