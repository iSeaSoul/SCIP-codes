#lang racket

; recursion
(define (accumulate combiner null_value term a next b)
  (if (> a b) 
       null_value
       (combiner (term a)
                 (accumulate combiner null_value term (next a) next b))))

; iteration
(define (accumulate_iter combiner null_value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null_value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(sum + 1 (lambda (x) (+ x 1)) 10)
(product + 1 (lambda (x) (+ x 1)) 10)

(define (product_iter term a next b)
  (accumulate_iter * 1 term a next b))

(product_iter + 1 (lambda (x) (+ x 1)) 10)

; with filter
(define (accumulate_filter combiner filter null_value term a next b)
  (if (> a b) 
       null_value
       (combiner (if (filter (term a)) (term a) null_value)
                 (accumulate_filter combiner filter null_value term (next a) next b))))

(define (even? x)
  (= 0 (remainder x 2)))

(accumulate_filter + even? 0 + 1 (lambda (x) (+ x 1)) 4)
(accumulate_filter + (lambda (x) (<= x 1)) 0 (lambda (x) (* x x)) -3 (lambda (x) (+ x 1)) 3)

(define (product_co_prime_number n)
  (accumulate_filter * 
                     (lambda (x) (= (gcd x n) 1))
                     1 + 1 (lambda (x) (+ x 1)) (- n 1)))

(product_co_prime_number 4)
(product_co_prime_number 5)