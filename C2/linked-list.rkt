#lang racket

(provide print-list)

(define (print-list items)
  (cond ((not (null? (cdr items)))
         (printf "~a -> " (car items))
         (print-list (cdr items)))
    ((null? (cdr items))
     (printf "~a ~n" (car items)))))

(define (length-list items)
  (if (null? items) 0 (+ 1 (length (cdr items)))))

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

; (define (reverse-list items)
;   (if (null? (cdr items))
;       items
;       (cons (reverse-list (cdr items)) (car items))))

; list cons must be (cons element c-list)
; (cons c-list element) will generate ((P1 P2) P3)

(define (reverse-list items)
  (define (iter items ret)
    (if (null? items)
        ret
        (iter (cdr items) (cons (car items) ret))))
  (iter items null))

(define test-list (list 1 2 3 4 5))

(print-list test-list)
(length-list test-list)
(last-pair test-list)
(print-list (reverse-list test-list))

(define (same-parity . items)
  (define (recursion items first-parity)
    (if (null? items)
        null
        (let ((cur-element (car items)))
          (cond ((= first-parity (remainder cur-element 2))
                 (cons cur-element (recursion (cdr items) first-parity)))
                (else (recursion (cdr items) first-parity))))))
  (recursion items (remainder (car items) 2)))

(print-list (same-parity 1 2 3 4 5))
(print-list (same-parity 2 3 4 5))

(define (square x)
  (* x x))

(define (square-list items)
  (map square items))

(print-list (square-list test-list))

(define (for-each proc items)
  (if (null? items)
      null
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))

(print (for-each (lambda (x) (print x)) test-list))
(newline)
