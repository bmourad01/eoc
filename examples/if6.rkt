(let ([x (read)])
  (let ([y (read)])
    (let ([z (read)])
      (if (and (eq? x 0) (> y 0) (< z 0))
          (+ y z 1 x)
          (- y z 1 x)))))
