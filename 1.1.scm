;; 1.3

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (largest-ss x y z)
  (cond ((and (>= x z) (>= y z)) (sum-of-squares x y))
        ((and (>= x y) (>= z y)) (sum-of-squares x z))
        (else (sum-of-squares y z))
        ))

;; 1.4
;;  ((if (> b 0) + -) a b) -> (- a b) when b < 0

;; 1.5
;; Normal order will return 0; applicative order will get stuck in an infinite loop

;; 1.6
;; Infinite loop, because of applicative-order.

;; 1.7

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt1 x)
  (sqrt-iter 1.0 x))

;; (sqrt1 0.00001) -> 0.03, (sqrt 0.00001) -> 0.1