#lang racket

(define test-tree (list 1 2 3 4))
(define test-tree-2 (list 1 (list 2 (list 3 4))))

(define (print-list items)
  (cond ((not (null? (cdr items)))
         (printf "~a -> " (car items))
         (print-list (cdr items)))
    ((null? (cdr items))
     (printf "~a ~n" (car items)))))

(print-list test-tree)
(print-list test-tree-2)

(define test-tree-3 (list 1 2))

(print-list (append test-tree test-tree-3))
(print-list (cons test-tree test-tree-3))
(print-list (list test-tree test-tree-3))

(define (deep-reverse items)
  (define (iter items ret)
    (if (null? items)
        ret
        (iter (cdr items) (cons (deep-reverse (car items)) ret))))
  (if (pair? items) (iter items null) items))

(define test-tree-4 (list test-tree (list 1 (list 2 3))))
(print-list (deep-reverse test-tree-4))

(define (fringe items)
  (cond ((null? items) null)
        ((pair? items) (append (fringe (car items)) (fringe (cdr items))))
        (else (list items))))

(print-list (fringe test-tree-2))
(print-list (fringe test-tree-4))

(define (tree-map proc items)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       items))

(define (scale x) (lambda (y) (* x y)))
(define (square) (lambda (x) (* x x)))

(print-list (tree-map (scale 10) test-tree-4))
(print-list (tree-map (square) test-tree-4))

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset-rest)
                            (cons (car s) subset-rest)) rest)))))

(print-list (subsets (list 1 2 3)))