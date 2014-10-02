#lang racket

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good_enough? (improve guess x) guess) 
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good_enough? new_guess guess)
  (< (/ (abs (- new_guess guess)) guess) 0.0000001))

(sqrt 2)
(sqrt 1000)
(sqrt 1032)
  