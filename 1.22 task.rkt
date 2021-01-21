#lang racket

(define (odd? n) (= (remainder n 2) 1))

(define (square n) (* n n))

(define (runtime) (current-milliseconds))

(define (prime? n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp denominator)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) denominator))
          denominator))
        (else
         (remainder 
          (* base (expmod base (- exp 1) denominator))
          denominator))))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  true)

(define (search-for-primes start) (search-for-primes-new start 3))
  
(define (search-for-primes-new start counter)
    (if (= counter 0)
        false
      (if (timed-prime-test start)
         (search-for-primes-new (+ start 1) (- counter 1))
         (search-for-primes-new (+ start 1) counter))
      ))
  