#lang racket

(define (cube x) (* x x x))

(define (simpsons-rule-integral f a b n)
  (integral f a b n 0 (find-h a b n)))

(define (integral func a b n counter h)
  
  (define (find-next x)
  (+ x h))
  
  (* (sum cube (+ a h) find-next b) h))

(define (find-next a counter h n)
  (cond
    ((= counter 0) a)
    ((= counter n) (+ a (* h counter)))
    (even? counter) (* 2 (+ a (* h counter)))
    (* 4 (+ a (* h counter)))))

(define (find-h a b n)
  (/ (- b a) n))

(define (find-y a n h counter)
  (cond
    ((= counter 0) a)
    ((= counter n) a)
    (even? counter) (+ a (* 2 h))
    (+ a (* 4 h))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
