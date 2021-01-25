#lang racket

(define (square n) (* n n))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp denominator)
  (cond ((= exp 0) 1)
        ((even? exp)
         (nontrivial-square-root?
          (remainder
           (square (expmod base (/ exp 2) denominator)) denominator) denominator))
        (else
         (remainder
          (* base (expmod base (- exp 1) denominator))
          denominator))))

(define (nontrivial-square-root? x n)
  (if (and
       (not (or
             (= x 1)
             (= x (- n 1))))
       (= (remainder (square x) n) 1))
      0
      x))



