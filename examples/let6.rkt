(let ([x (read)])
  (let ([y (- x 1)])
    (let ([z (* y y)])
      (* 5 (- z x)))))
