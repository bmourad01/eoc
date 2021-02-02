(let ([n (read)]
      [v (vector 1 0 1)]
      [f (lambda: () : Void
           (let ([sum (+ (vector-ref v 1)
                         (vector-ref v 2))])
             (begin
               (vector-set! v 1 (vector-ref v 2))
               (vector-set! v 2 sum)
               (vector-set! v 0 (+ (vector-ref v 0) 1)))))])
  (while (<= (vector-ref v 0) n)
    (begin
      (print v)
      (f))))
