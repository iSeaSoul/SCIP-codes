#lang racket

(define (fast_expt ret a n)
  (cond ((= n 0) ret)
        ((even? n) (fast_expt ret (square a) (halve n)))
        (else (fast_expt (* ret a) (square a) (halve (- n 1))))))

(define (even? x)
  (= 0 (remainder x 2)))

(define (halve n)
  (/ n  2))

(define (square x)
  (* x x))

(define (fast_expt_entry a n)
  (fast_expt 1 a n))

(fast_expt_entry 2 2)
(fast_expt_entry 2 31)