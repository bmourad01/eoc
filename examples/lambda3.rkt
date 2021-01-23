(let ([f (if (eq? (read) 1)
             (lambda: ([x : Integer]) : Integer (+ x 1))
             (let ([y 2])
               (lambda: ([x : Integer]) : Integer (+ x y))))])
  (f 3))
         
