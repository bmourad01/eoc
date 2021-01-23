(define (get-n [n : Integer]
               [x : Integer]) : Integer
  (if (>= x n) n (get-n n (+ x 1))))

(get-n (read) (read))
