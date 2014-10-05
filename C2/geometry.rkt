#lang racket

(define (make-point x y)
  (cons x y))

(define (point-x p) (car p))
(define (point-y p) (cdr p))

(define (print-point p)
  (printf "(~a, ~a)~n" (point-x p) (point-y p)))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (print-segment s)
  (print-point (start-segment s))
  (print-point (end-segment s)))

(define (midpoint-segment s)
  (define (mid-func point-dimo)
           (average (point-dimo (start-segment s)) (point-dimo (end-segment s))))
  (make-point (mid-func point-x)
              (mid-func point-y)))

(define (average x y)
  (/ (+ x y) 2.0))

(define PX (make-point 0 0))
(define PY (make-point 1 1))
(define SEG (make-segment PX PY))

(print-segment SEG)
(print-point (midpoint-segment SEG))

; rectangle : one expression
(define (make-rectangle p-bottom W H)
  (cons p-bottom (cons W H)))

(define (get-width R)
  (car (cdr R)))

(define (get-height R)
  (cdr (cdr R)))

(define (get-area R)
  (* (get-width R) (get-height R)))

(define (get-perim R)
  (* (+ (get-width R) (get-height R)) 2.0))

(define unit-square (make-rectangle PX 1 1))
(get-area unit-square)
(get-perim unit-square)

; rectangle : another expression
(define (make-rectangle-seg s1 s2) 
  (cons s1 s2))

(define (get-width-seg R) 
  (abs (- (point-x (start-segment (car R))) (point-x (end-segment (car R))))))

(define (get-height-seg R) 
  (abs (- (point-y (start-segment (cdr R))) (point-y (end-segment (cdr R))))))

(define (get-area-seg R)
  (* (get-width-seg R) (get-height-seg R)))

(define (get-perim-seg R)
  (* (+ (get-width-seg R) (get-height-seg R)) 2.0))

(define PXX (make-point 1 0))
(define PXY (make-point 0 1))
(define unit-square-seg (make-rectangle-seg (make-segment PX PXX) (make-segment PX PXY)))

(get-area-seg unit-square-seg)
(get-perim-seg unit-square-seg)