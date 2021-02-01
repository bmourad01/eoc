(let ([n (read)] [i 1] [v (vector 0 1)])
  (while (<= i n)
    (begin
      (print v)
      (let ([sum (+ (vector-ref v 0) (vector-ref v 1))])
        (begin
          (vector-set! v 0 (vector-ref v 1))
          (vector-set! v 1 sum)
          (set! i (+ i 1)))))))
