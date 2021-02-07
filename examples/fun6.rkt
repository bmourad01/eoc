(define (fun1 [f : (-> Integer Boolean)]
              [n : Integer]) : Integer
  (if (f n) 42 37))

(define (fun2 [x: Integer]) : Boolean
  (eq? (rem x 2) 0))

(fun1 fun2 (read))
