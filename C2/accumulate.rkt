#lang racket

(define (print-list items)
  (cond ((not (null? (cdr items)))
         (printf "~a -> " (car items))
         (print-list (cdr items)))
    ((null? (cdr items))
     (printf "~a ~n" (car items)))))

(define (accumulate combiner init-val sequence)
  (if (null? sequence)
      init-val
      (combiner (car sequence)
                (accumulate combiner init-val (cdr sequence)))))

(define (map-ac func seq)
  (accumulate (lambda (x y) (cons (func x) y)) null seq))

(define (append-ac seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-ac seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(define test-list (list 1 2 3 4))
(define test-list-2 (list 8 7))
(define (square x)(* x x))

(print-list (map-ac square test-list))
(print-list (append-ac test-list-2 test-list))
(length test-list)

(define (horner-eval x coeff)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0 coeff))

(horner-eval 2 test-list)

(define test-tree-2 (list test-list (list 1 (list 2 3))))

(define (count-leaves-ac tree)
  (accumulate (lambda (this-item other-count)
                (if (pair? this-item)
                    (+ (count-leaves-ac this-item) other-count)
                    (+ 1 other-count)))
              0
              tree))

(count-leaves-ac test-tree-2)

; accumulate n

(define (accumulate-n combiner init-val seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate combiner init-val (map car seqs))
            (accumulate-n combiner init-val (map cdr seqs)))))

(define test-mat (list (list 1 2 3) (list 4 5 6)))

(print-list (accumulate-n + 0 test-mat))