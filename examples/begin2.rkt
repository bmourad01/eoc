(let ([x 666])
  (let ([y (begin (set! x (+ x 1))
                  (let ([z (vector x (- x 1))])
                    (begin
                      (vector-set! z 0 42)
                      z)))])
    y))
