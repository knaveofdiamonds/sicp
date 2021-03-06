(define (cube x) (* x x x))

;; 1.29

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* k h))))
  (* (/ h 3) (+ (yk 0) 
                (* 4 (sum yk 1 (lambda (i) (+ i 2)) (- n 1)))
                (* 2 (sum yk 2 (lambda (i) (+ i 2)) (- n 1)))
                (yk n))))
                       
; Gives 1/4 for both n = 100 and n = 1000

;; 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; 1.31

(define (identity x) x)
(define (inc x) (+ x 1))

(define (product1 term a next b)
  (if (> a b)
      1
      (* (term a)
         (product1 term (next a) next b))))


(define (product2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n) (product1 identity 1 inc n))

;; 1.32

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum2 term a next b)
  (accumulate + 0 term a next b))

(define (product3 term a next b)
  (accumulate * 1 term a next b))

;; 1.33

(define (filter-accumulate predicate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (predicate a) (combiner (term a) result) result))))
  (iter a null-value))

(define (sum-of-primes-between a b)
  (filter-accumulate prime? + 0 identity a inc b))

; From 1.2
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
; End 1.2

;; 1.34

(define (f g)
  (g 2))

; We'll get an error calling (f f), because eventually we'll be trying to evaluate (2 2) which doesn't make sense.

;; 1.35

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))