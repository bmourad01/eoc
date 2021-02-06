(let* ([a (let ([x (read)]) (+ x 1))]
       [b (let ([x (read)]) (- x 1))])
  (* a b))
