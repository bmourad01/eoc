(let ([x 0] [y 0] [r (read)])
  (begin
    (when (> r 0)
      (set! x r))
    (unless (>= r 0)
      (set! y r))
    (print x)
    (print y)))
