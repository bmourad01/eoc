(let ([sum 0] [i 5])
  (begin
    (while (> i 0)
      (begin
        (set! sum (+ sum i))
        (set! i (- i 1))))
    sum))
  
