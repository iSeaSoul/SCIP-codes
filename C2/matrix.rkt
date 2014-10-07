#lang racket

(require "util.rkt")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vec m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-mat m n)
  (let ((cols (transpose n)))
    (map (lambda (w) (matrix-*-vec cols w)) m)))

(define vec1 (list 1 2 3 4))
(define vec2 (list 4 3 2 1))
(define mat1 (list vec1 vec2 vec1))
(define mat2 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8)))

(print-list (map * vec1 vec2))
(dot-product vec1 vec2)
(print-list (matrix-*-vec mat1 vec1))
(print-list (transpose mat2))
(print-list (matrix-*-mat mat1 mat2))