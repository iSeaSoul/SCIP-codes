#lang racket

(define (double_procedure f)
  (lambda (x) (f (f x))))

(define (compose_procedure f g)
  (lambda (x) (f (g x))))

(define (repeat_procedure f k)
  (if (= k 1)
      f
      (compose_procedure f (repeat_procedure f (- k 1)))))

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

; test procedure abstraction
(((double_procedure (double_procedure double_procedure)) inc) 2)
((compose_procedure square inc) 6)
((repeat_procedure square 3) 2)

(define dx 0.000001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3.0)))

; Wrong repeat_procedure
; (define (smooth_n_times f n)
;   (repeat_procedure (smooth f) n))
; This is smooth ( f ( smooth ( f (val) ) ) )
; instead of smooth ( smooth ( f ) ) (val)

(define (smooth_n_times n)
  (repeat_procedure smooth n))

(define (non_continuous_func x)
  (if (= x 1) 2 x))

; test smooth
(non_continuous_func 1)
((smooth non_continuous_func) 1)
((smooth (smooth non_continuous_func)) 1)
(((smooth_n_times 1) non_continuous_func) 1)
(((smooth_n_times 2) non_continuous_func) 1)
(((smooth_n_times 10) non_continuous_func) 1)

(define eps 0.000001)

(define (real_equal? x y)
  (< (abs (- x y)) eps))

(define (fixed_point f cur_guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (real_equal? next guess)
          next
          (try next))))
  (try cur_guess))

(define (average x y)
  (/ (+ x y) 2.0))

(define (average_damp f)
  (lambda (x) (average x (f x))))

(define (average_damp_n_times n)
  (repeat_procedure average_damp n))

(define (sqrt x)
  (fixed_point ((average_damp_n_times 1) (lambda (y) (/ x (expt y 1)))) 1.0))

(define (nth_root x n)
  (fixed_point ((average_damp_n_times (- n 1)) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

; test nth_root
(nth_root 2 2)
(nth_root 2 3)
(nth_root 2 4)
((double_procedure square) (nth_root 2 4))