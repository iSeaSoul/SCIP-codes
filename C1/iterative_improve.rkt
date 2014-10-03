#lang racket

(define (iterative_improve good_enough? improve)
  (lambda (x)
    (if (good_enough? x)
        x
        ((iterative_improve good_enough? improve) (improve x)))))

(define eps 0.000001)

(define (real_equal? x y)
  (< (abs (- x y)) eps))

(define (average x y)
  (/ (+ x y) 2.0))

(define (sqrt z)
  ((iterative_improve (lambda (x) (real_equal? (* x x) z))
                     (lambda (x) (average x (/ z x)))) 1.0))

(define (fixed_point f cur_guess)
  ((iterative_improve (lambda (x) (real_equal? (f x) x)) f) cur_guess))

(define (sqrt_fp x)
  (fixed_point (lambda (y) (average y (/ x y))) 1.0))

(define (square x)
  (* x x))

(sqrt 2.0)
(fixed_point cos 1.0)
(sqrt_fp 2.0)