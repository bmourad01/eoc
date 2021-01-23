(define (fib [n : Integer]) : Integer
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib (read))
