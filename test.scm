(test-begin "skim.core")

(import (skim core))

(test (lit #(a (+ 1 1)) a) '2)
(test (fir #(x '(1 2 3)) x) '(1 2 3))

(test (fir #(x '(1 2 3 13)
               y #(7 8)
               lit: #(z (* x y))
               when: (odd? z)
               lit: #(a (* 2 z))
               while: (< a 170))
           (/ a 3))
      '(14/3 14))

(test (lip iter #(a (cursor '(0 1 2 3 4)))
           (if (cursor-null? a)
               'foo
               (iter (cursor-next! a))))
      'foo)

(test (lit #(a #(a b c))
           (fir #(x '(1 2 3) y a)
                (cons x y)))
      '((1 . a) (1 . b) (1 . c)
        (2 . a) (2 . b) (2 . c)
        (3 . a) (3 . b) (3 . c)))

(test-end)
