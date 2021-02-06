(define (f [x : Integer]) : (Vector (-> Integer) (-> Void))
  (vector
   (lambda: () : Integer x)
   (lambda: () : Void (set! x (+ 1 x)))))

(let ([counter (f 0)])
  (let* ([get (vector-ref counter 0)]
         [inc (vector-ref counter 1)])
    (begin
      (inc)
      (get))))
        
