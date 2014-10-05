#lang racket

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(define (test-func x)
  (+ x x))

((zero test-func) test-func)
(test-func 1)
((one test-func) 1)
(((add-1 zero) test-func) 1)
((two test-func) 1)
(((add one one) test-func) 1)
(((add two two) test-func) 1)

