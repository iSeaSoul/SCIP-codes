#lang racket

(require "util.rkt")
(require math/number-theory)

; equivalent to range(low (+ hight 1))

(define (enum-interval low high)
  (if (> low high)
      null
      (cons low (enum-interval (+ low 1) high))))

(define (make-pair n)
  (flatmap (lambda (i)
         (map (lambda (j) (list i j))
              (enum-interval 1 (- i 1))))
       (range 1 (+ n 1))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(print-list (make-pair 3))

(define (prime-sum-pairs n)
  (filter (lambda (pair)
            (prime? (+ (car pair) (cadr pair))))
          (make-pair n)))

(print-list (prime-sum-pairs 10))

(define (make-triple n)
  (flatmap (lambda (i)
             (flatmap (lambda (j) 
                    (map (lambda (k) (list i j k))
                         (range 1 j)))
                  (range 1 i)))
           (range 1 (+ 1 n))))

(print-list (make-triple 5))

(define (make-triple-given-sum sum)
  (filter (lambda (triple)
            (= sum (+ (car triple) (cadr triple) (caddr triple))))
          (make-triple (- sum 2))))

(print-list (make-triple-given-sum 10))