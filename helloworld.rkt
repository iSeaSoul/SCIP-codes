#lang racket

(define (max_number_sum x y z)
  (cond ((< x y) (if (< x z) (+ y z) (+ x y)))
        (else (if (< y z) (+ x z) (+ x y)))))

(max_number_sum 2 3 4)
(max_number_sum 5 6 4)