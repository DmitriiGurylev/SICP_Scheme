#lang racket

(define (square n) (* n n))
(define (runtime) (current-inexact-milliseconds))

(define (smallest-divisor n) (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         (display test-divisor))
        (else (find-divisor 
               n 
               (next test-divisor)))))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (smallest-divisor n)
      (report-prime n (- (runtime) start-time))
      false))

(define (report-prime n elapsed-time)  
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (next divisor)
  (cond ((= divisor 2) 3)
        (else (+ divisor 2))))
