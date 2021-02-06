(let* ([x 0] [y 0] [z 20])
  (let ([f (lambda: ([a : Integer]) : Integer (+ a (+ x z)))])
    (begin
      (set! x 10)
      (set! y 12)
      (f y))))
