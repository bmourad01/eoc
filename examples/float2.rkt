(let ([v (vector 1.23 4.56 7.89)])
  (let ([x (vector-ref v 1)])
    (begin
      (when (> (read) 0)
        (set! x (- x)))
      (vector-set! v 1 x)
      v)))
