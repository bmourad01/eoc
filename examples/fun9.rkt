(define (gcd [a : Integer] [b: Integer]) : Integer
  (if (eq? b 0)
      a
      (gcd b (rem a b))))

(let ([a (read)] [b (read)])
  (gcd a b))
