#lang racket

(define dx 0.000001)
(define eps 0.000001)

(define (real_equal? x y)
  (< (abs (- x y)) eps))

(define (fixed_point f cur_guess)
  (define (try guess)
    ; (printf "new guess ~a ~n" guess)
    (let ((next (f guess)))
      (if (real_equal? next guess)
          next
          (try next))))
  (try cur_guess))

(define (average x y)
  (/ (+ x y) 2.0))

(define (average_damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  ;(fixed_point (lambda (y) (average y (/ x y))) 1.0))
  (fixed_point (average_damp (lambda (y) (/ x y))) 1.0))

(define (cube x)
  (fixed_point (average_damp (lambda (y) (/ x (expt y 2)))) 1.0))

; tet fixed_point
(fixed_point cos 1.0)
(fixed_point (lambda (x) (+ 1 (/ 1.0 x))) 1)
(sqrt 2.0)
(cube 2.0)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

; newton_transform : f(x) = x - g(x) / d(g(x))
(define (newton_transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton_method g guess)
  (fixed_point (newton_transform g) guess))

(define (sqrt_newton z)
  (newton_method (lambda (x) (- (expt x 2) z)) 1.0))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

; test newton_method
((deriv (lambda (x) (* x x))) 10)
(sqrt_newton 2.0)
(newton_method (cubic 1 1 1) 0)