(let ([x (read)])
  (let ([y (read)])
    (if (eq? y 0)
        (if (eq? x 0)
            0
            (rem y x))
        (/ x y))))
