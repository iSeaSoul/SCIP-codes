#lang racket

(define (fast_fib n)
  (fib_iter 1 0 0 1 n))

(define (even? x)
  (= 0 (remainder x 2)))

(define (halve n)
  (/ n  2))

(define (square x)
  (* x x))

(define (fib_iter a b p q n)
  (cond ((= n 0) b)
        ((even? n) 
         (fib_iter a b 
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (halve n)))
        (else 
         (fib_iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q))
                   p q (- n 1)))))

(define (fib_display bound n)
  (cond ((>= n bound) 
         (fib_display bound (- n 1))
         (display (fast_fib n))
         (display " "))))

(fib_display 1 10)
(fib_display 50 55)