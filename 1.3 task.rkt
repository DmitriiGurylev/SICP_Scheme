#lang racket

(define (square x) (* x x))

(define (sum-of-the-squares a b c)
  (cond ((and (or (< a b) (= a b))  (or (< a c)  (= a c))) (+ (square b) (square c)))
        ((and (or (< b a) (= b a))  (or (< b c)  (= b c))) (+ (square a) (square c)))
        ((and (or (< c a) (= c a))  (or (< c b)  (= c b))) (+ (square a) (square b)))))


