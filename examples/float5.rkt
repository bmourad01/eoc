(let ([x (int->float (read))]
      [y (int->float (read))]) 
  (* (if (> x y) (+ x y) (* x y)) 0.12))
