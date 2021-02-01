(let ([n (read)]
      [i 1]
      [v (vector 0 1)]
      [f (lambda: () : Void
           (let ([sum (+ (vector-ref v 0)
                         (vector-ref v 1))])
             (begin
               (vector-set! v 0 (vector-ref v 1))
               (vector-set! v 1 sum))))])
  (while (<= i n)
    (begin
      (print v)
      (f)
      (set! i (+ i 1)))))
