(let ([x 0] [y 0])
  (begin
    (while (< x 666)
      (let ([v (vector x (vector x (vector x)))])
        (begin
          (set! y (vector-ref (vector-ref v 1) 0))
          (set! x (+ x 1)))))
    (+ y 1)))
