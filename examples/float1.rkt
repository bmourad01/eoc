(define (sub1 [z : Float]) : Float
  (- z 1.0))

(let* ([x 1.5] [y 2.3] [z (+ x y)])
  (begin
    (set! z (sub1 z))
    (print 1.234)
    z))

