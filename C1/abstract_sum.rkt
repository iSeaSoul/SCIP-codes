#lang racket

; recursion
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))

; iteration
(define (sum_iter term a next b)
  (define (iter a result)
    (if (> a b) 
         result
         (iter (next a) (+ (term a) result))))
    (iter a 0))

(define (sum_nora a b)
  (sum + a (lambda (x) (+ x 1)) b))

(define (sum_nora_iter a b)
  (sum_iter + a (lambda (x) (+ x 1)) b))

(sum_nora 1 4)
(sum_nora_iter 1 4)

(define (pi_sum a b)
  (* 8 (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a 
       (lambda (x) (+ x 4))
       b)))

(pi_sum 1 1000)

(define (integral f a b dx)
  (* dx (sum f 
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)))

(integral (lambda (x) (* x x x)) 0 1 0.001)

(define (even? x)
  (= 0 (remainder x 2)))

(define (simpson_integral f a b n)
  (let ((h (/ (- b a) (* 1.0 n))))
    (define (sum k)
      (cond ((= k n) (f b))
            ((= k 0) (+ (f a) (sum (+ k 1))))
            ((even? k) (+ (* 2 (f (+ a (* k h)))) (sum (+ k 1))))
            (else (+ (* 4 (f (+ a (* k h)))) (sum (+ k 1))))))
    (* h (/ 1.0 3) (sum 0))))

(simpson_integral (lambda (x) (* x x x)) 0 1 1000)