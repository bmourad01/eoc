(define (quadratic-formula
         [a : Float]
         [b : Float]
         [c : Float]) : (Vector Float Float)
  (let* ([s (sqrt (- (* b b) (* 4.0 a c)))]
         [a2 (* 2.0 a)]
         [nb (- b)])
    (vector (/ (+ nb s) a2) (/ (- nb s) a2))))

(quadratic-formula 1.0 2.0 (- 3.0))
