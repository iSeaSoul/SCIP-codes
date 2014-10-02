#lang racket

; brute_force

(define (smallest_divisor n)
  (find_divisor n 2))

(define (find_divisor n test_divisor)
  (cond ((> (square test_divisor) n) n)
        ((divides? test_divisor n) test_divisor)
        (else (find_divisor n (next_test_divisor test_divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next_test_divisor test_divisor)
  (if (= test_divisor 2)
      (+ test_divisor 1)
      (+ test_divisor 2)))

(define (square x)
  (* x x))

(define (prime_bf? n)
  (= n (smallest_divisor n)))

(prime_bf? 2)
(prime_bf? 100)
(prime_bf? 103)

; fermat_test

(define (fast_expmod a exp mod)
  (cond ((= exp 0) 1)
        ((= 0 (remainder exp 2)) 
         (remainder (square (fast_expmod a (/ exp 2) mod)) mod))
        (else
         (remainder (* a (fast_expmod a (- exp 1) mod)) mod))))

(define (fermat_test x)
  (define (fermat_test_imp a)
    (= (fast_expmod a x x) a))
  (fermat_test_imp (+ 1 (random (- x 1)))))

(define (fast_prime_test x times)
  (cond ((= times 0) true)
        ((fermat_test x) (fast_prime_test x (- times 1)))
        (else false)))

(define (fast_prime? x)
  (fast_prime_test x 10))

(fast_prime? 2)
(fast_prime? 1000000000)
(fast_prime? 1000000007)

; display

(define (timed_prime_test test_method n)
  (start_prime_test n test_method (current-inexact-milliseconds)))

(define (start_prime_test n test_method start_time)
  (cond ((test_method n) (report_used_time n (- (current-inexact-milliseconds) start_time)) true)
        (else false)))

(define (report_used_time n elapsed_time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed_time))

(define (search_prime prime_test_method need_num test_num)
  (cond ((> need_num 0) 
         (if (timed_prime_test prime_test_method test_num) 
             (search_prime prime_test_method (- need_num 1) (+ test_num 1))
             (search_prime prime_test_method need_num (+ test_num 1))))))

(search_prime prime_bf? 3 100000000)
(search_prime fast_prime? 3 100000000)

; miller_rabin

(define (miller_fast_expmod a exp mod)
  ;(printf "miller_fast_expmod ~a ~a ~a ~n" a exp mod)
  (cond ((= exp 0) 1)
        ((= 0 (remainder exp 2)) 
         (let ((halfexp_mod (miller_fast_expmod a (/ exp 2) mod)))
           (let ((ret (remainder (square halfexp_mod) mod)))
             (if (and (= ret 1) (not (= halfexp_mod 1)) (not (= halfexp_mod (- mod 1)))) 0 ret))))
        (else
         (remainder (* a (miller_fast_expmod a (- exp 1) mod)) mod))))

(define (miller_test x)
  (define (miller_test_imp a)
    (= (miller_fast_expmod a (- x 1) x) 1))
  (miller_test_imp (+ 1 (random (- x 1)))))

(define (miller_prime_test x times)
  ;(printf "miller_prime_test ~a ~a ~n" x times)
  (cond ((= times 0) true)
        ((miller_test x) (miller_prime_test x (- times 1)))
        (else false)))

(define (miller_prime? x)
  (miller_prime_test x 10))

; Carmichael Number

(prime_bf? 1105)
(fast_prime? 1105)
(miller_prime? 1105)