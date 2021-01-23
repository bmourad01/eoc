(define (fac [n : Integer]) : Integer
  (if (<= n 1)
      1
      (* n (fac (- n 1)))))

(fac (read))
