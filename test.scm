(fir #(x '(1 2 3 13)
       y #(7 8)
       lit: #(z (* x y))
       when: (odd? z)
       lit: #(a (* 2 z))
       while: (< a 170))
     (/ a 3))

(lit #(a #(a b c))
     (fir #(x '(1 2 3) y a)
          (cons x y)))

(lit #(a (+ 1 1)) a)


(lip iter #(a (cursor '(0 1 2 3 4))) (if (cursor-null? a) 'foo (iter (cursor-next a))))
