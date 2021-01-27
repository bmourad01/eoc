(let ([x 5])
  (let ([f (lambda: ([x : Integer]) : Integer (let ([x (+ x 1)]) x))])
    (let ([x (+ x (f 1))])
      x)))
