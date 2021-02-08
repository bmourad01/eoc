(define (prime? [n : Integer]) : Boolean
  (if (<= n 3) (> n 1)
      (if (or (eq? (rem n 2) 0)
              (eq? (rem n 3) 0)) #f
          (let ([i 5] [b #t])
            (begin
              (while (and b (<= (* i i) n))
                (begin
                  (set! b (or (eq? (rem n i) 0)
                              (eq? (rem n (+ i 2)) 0)))
                  (set! i (+ i 6))))
              b)))))
                        
(let ([i 0] [n (read)])
  (while (> n 0)
    (begin
      (when (prime? i)
        (set! n (- n 1))
        (print i))
      (set! i (+ i 1)))))
