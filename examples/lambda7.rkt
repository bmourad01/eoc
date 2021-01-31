(let ([k (lambda: ([x : Integer]) : (Integer -> Integer)
           (lambda: ([y : Integer]) : Integer x))])
  ((k 42) 777))
