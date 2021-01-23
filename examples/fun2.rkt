(define (my-fun [a : Integer]
                [b : Integer]
                [c : Boolean]
                [d : Boolean]
                [e : Integer]
                [f : Integer]
                [g : Boolean]
                [h : Integer]) : Integer
  (+ a b h))

(my-fun 1 2 #t #f 3 4 #t 5)
