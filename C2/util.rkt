#lang racket

(provide print-list)
(provide accumulate)
(provide accumulate-n)
(provide flatmap)

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

(define (accumulate-n combiner init-val seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate combiner init-val (map car seqs))
            (accumulate-n combiner init-val (map cdr seqs)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))