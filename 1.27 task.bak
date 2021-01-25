#lang racket

(define (square n) (* n n))
(define (runtime) (current-inexact-milliseconds))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n (- n 1))
      (report-prime n (- (runtime) start-time))
      false))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (search-for-primes start-value how-many-values?)
  (if (= how-many-values? 0)
      (display "Task have done")
      (if (odd? start-value)
          (if (timed-prime-test start-value)
              (search-for-primes (+ 2 start-value) (- how-many-values? 1))
              (search-for-primes (+ 2 start-value) how-many-values?))
          (search-for-primes (+ 1 start-value) how-many-values?))))

