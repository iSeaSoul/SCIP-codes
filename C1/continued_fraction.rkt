#lang racket

; recursion
(define (cont_frac n d k)
  (define (cont_frac_val i)
    (if (> i k) 
        0          
        (/ (n i) (+ (d i) (cont_frac_val (+ i 1))))))
  (cont_frac_val 1))

; iteration
(define (cont_frac_iter n d k)
  (define (cont_frac_val i result)
    (if (= i 0) 
        result
        (cont_frac_val (- i 1) (/ (n i) (+ (d i) result)))))
  (cont_frac_val k 0))

(define (golden k)
  (cont_frac (lambda (x) 1.0)
             (lambda (x) 1.0)
             k))

(define (golden_iter k)
  (cont_frac_iter (lambda (x) 1.0)
             (lambda (x) 1.0)
             k))

; test golder (& iter)
(golden 10)
(golden_iter 10)
(golden 100)
(golden_iter 100)

(define (calc_e k)
  (+ 2
     (cont_frac (lambda (x) 1.0)
                (lambda (x) 
                  (let ((rem (remainder x 3)))
                    (if (= rem 2)
                        (* 2.0 (+ (/ (- x rem) 3) 1))
                        1.0)))
                k)))

; test calc_e
(calc_e 10)
(calc_e 100)