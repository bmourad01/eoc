(let ([x (vector 1 2 (vector 5) (vector))])
  (let ([y (vector-set! x 1 1234)])
    x))
