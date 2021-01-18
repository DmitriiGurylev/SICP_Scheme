#lang racket

(define (exponent b n)
  (if (= n 0)
      1
      (* b (exponent b (- n 1)))))