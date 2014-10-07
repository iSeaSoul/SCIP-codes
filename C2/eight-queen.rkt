#lang racket

(require "util.rkt")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (pos) (safe? k pos))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row) (append rest-of-queens (list new-row)))
                 (range 1 (+ board-size 1))))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board (list))

(define (safe? new-row pos)
  (define (conflict? R1 C1 R2 C2)
    (or (= R1 R2) 
        (= C1 C2)
        (= (+ R1 C1) (+ R2 C2))
        (= (- R1 C1) (- R2 C2))))
  (define (safe-rec? r-last c-last row-num)
    (if (= row-num new-row)
        true
        (and
         (not (conflict? r-last c-last row-num (list-ref pos (- row-num 1))))
         (safe-rec? r-last c-last (+ row-num 1)))))
  (safe-rec? new-row (list-ref pos (- new-row 1)) 1))

(print-list (queens 8))