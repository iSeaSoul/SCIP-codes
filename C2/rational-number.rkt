#lang racket

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (if (< d 0)
      (make-rat (- 0 n) (- 0 d))
      (let ((G (gcd n d)))
        (cons (/ n G) (/ d G)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (printf "~a / ~a ~n" (numer x) (denom x)))

(define one-half (make-rat -2 -4))
(define one-third (make-rat 1 3))

(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (add-rat one-third one-third))

; cons : dispatch

(define (cons-d x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car-d z) (z 0))
(define (cdr-d z) (z 1))

(car-d (cons-d 1 2))
;((cons-d 1 2) 3)

; cons : double lambda

(define (cons-dl x y)
  (lambda (m) (m x y)))

(define (car-dl z)
  (z (lambda (p q) p)))

(define (cdr-dl z)
  (z (lambda (p q) q)))

(car-dl (cons-dl 1 2))
((cons-dl 1 2) (lambda (x y) (+ x y)))