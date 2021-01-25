#lang racket

(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
          m)))) 

(define (fast-prime? n current-time)
  (define (try-it a) (= (expmod a n n) a))
  (define (fermat-test n) (try-it current-time))

  (if (= (- n 1) current-time)
      (display "prime!")
      (if (fermat-test n)
          (fast-prime? n (+ current-time 1))
          (display "not prime!"))))