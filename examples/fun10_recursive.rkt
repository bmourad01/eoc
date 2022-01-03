(define (prime-aux? [n : Integer] [i : Integer] [b : Boolean]) : Boolean
  (if (and b (<= (* i i) n))
      (prime-aux? n (+ i 6) (not (or (eq? (rem n i) 0)
                                     (eq? (rem n (+ i 2)) 0))))
      b))

(define (prime? [n : Integer]) : Boolean
  (if (<= n 3) (> n 1)
      (if (or (eq? (rem n 2) 0)
              (eq? (rem n 3) 0)) #f
          (prime-aux? n 5 #t))))
                        
(let ([i 0] [n (read)])
  (while (> n 0)
    (begin
      (when (prime? i)
        (set! n (- n 1))
        (print i))
      (set! i (+ i 1)))))
