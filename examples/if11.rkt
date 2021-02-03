(let ([x 0] [y 0])
  (let ([z (if (let ([w (read)])
                 (begin
                   (set! x (+ x w))
                   (set! y (- y w))
                   (> x y)))
               x y)])
    z))
