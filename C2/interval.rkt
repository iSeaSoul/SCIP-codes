#lang racket

(define (make-interval a b) (cons a b))
(define lower-bound car)
(define upper-bound cdr)

(define (print-interval x)
  (printf "[~a, ~a] ~n" (lower-bound x) (upper-bound x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define one-two (make-interval 1 2))
(define one-three (make-interval 1 3))

(print-interval one-two)
(print-interval (add-interval one-two one-three))
(print-interval (sub-interval one-two one-three))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (rev-interval x)
  (make-interval (/ 1.0 (upper-bound x))
                 (/ 1.0 (lower-bound x))))

(define (div-interval x y)
  (mul-interval x (rev-interval y)))

(print-interval (mul-interval one-two one-three))
(print-interval (div-interval one-two one-three))

(define (make-centre-width c w)
  (make-interval (- c w) (+ c w)))

(define (centre x)
  (/ (+ (lower-bound x) (upper-bound x)) 2.0))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

(define (make-centre-percent c p)
  (make-centre-width c (* c p)))

(define (percent x)
  (/ (width x) (centre x)))

(define (print-percented-interval x)
  (printf "C: ~a P: ~a ~n" (centre x) (percent x)))

(define cp-interval (make-centre-percent 10 0.1))
(print-interval cp-interval)
(percent cp-interval)
 
(define (parallen-interval x y)
  (div-interval (mul-interval x y) (add-interval x y)))

(define (parallen-interval-2 x y)
  (rev-interval (add-interval (rev-interval x) (rev-interval y))))

(print-percented-interval (parallen-interval cp-interval one-two))
(print-percented-interval (parallen-interval-2 cp-interval one-two))