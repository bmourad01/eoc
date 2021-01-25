(let ([x 5])
  (let ([y (while (> x 0)
             (set! x (- x 1)))])
    x))
