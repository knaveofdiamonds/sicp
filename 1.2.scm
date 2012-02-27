;; 1.9

(lambda (a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
;
; Recursive Process

(lambda (a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
;
; Iterative Process

;; 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;(A 1 10)
; 1024

;(A 2 4)
; 65536

;(A 3 3)
; 65536

; Gives 2y
(define (fA n) (A 0 n))

; Gives 2^y
(define (gA n) (A 1 n))

; Gives 2^^y
(define (hA n) (A 2 n))

;; 1.11

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) 
	 (* 2 (f (- n 2))) 
	 (* 3 (f (- n 3))))))

;; (f 0) # 0
;; (f 1) # 1
;; (f 2) # 2

;; (f 3)
;; (+ (* 1 2) (* 2 1) (* 3 0)) # 4

;; (f 4)
;; (+ (* 1 4) (* 2 2) (* 3 1))

(define (f-iter n)
  (define (f-iter-impl n a b c counter)
    (if (eq? n counter)
        a
        (f-iter-impl n (+ a (* 2 b) (* 3 c)) a b (+ 1 counter))))
  (if (< n 3)
      n
      (f-iter-impl n 2 1 0 2)))
      
;; 1.12

(define (pascal row col)
  (if (or (eq? col 1) (eq? col row)) 1
      (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))


;; 1.14

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; (cc 11 5)
;; (+ (cc 11 4) (cc (- 11 50) 5))
;; (+ (cc 11 4) 0)
;; (cc 11 4)
;; (+ (cc 11 3) (cc (- 11 25) 4))
;; (+ (cc 11 3) 0)
;; (cc 11 3)
;; (+ (cc 11 2) (cc 1 3))
;; (+ (cc 11 2) (+ (cc 1 2) (cc -9 3)))
;; (+ (cc 11 2) (+ (cc 1 2) 0))
;; (+ (cc 11 2) (cc 1 2))
;; (+ (cc 11 2) (+ (cc 1 1) (cc -4 2)))
;; (+ (cc 11 2) (+ (cc 1 1) 0))
;; (+ (cc 11 2) (cc 1 1))
;; (+ (cc 11 2) (+ (cc 1 0) (cc 0 1)))
;; (+ (cc 11 2) (+ 0 1))
;; (+ (cc 11 2) 1)
;; (+ (+ (cc 11 1) (cc 6 2)) 1)
;; (+ (+ (cc 11 1) (+ (cc 6 1) (cc 1 1))) 1)
;; (+ (+ (cc 11 1) (+ (cc 6 1) (+ (cc 1 0) (cc 0 1)))) 1)
;; (+ (+ (cc 11 1) (+ (cc 6 1) (+ 0 1))) 1)
;; (+ (+ (cc 11 1) (+ (cc 6 1) 1)) 1)


;; 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;; (sine 12.5)
;; (p (sine 4.17))
;; (p (p (sine 1.39)))
;; (p (p (p (sine 0.46))))
;; (p (p (p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05))))))

;; p applied 5 times
;; O(log a) growth, O(log a) space

;; 1.16

(define (square x) (* x x))

(define (fast-expt b n)
  (define (fast-expt-impl b n a)
    (if (eq? n 1)
        a
        (if (even? n)
            (fast-expt-impl b (/ n 2) (* a (square b)))
            (* b (fast-expt-impl b (/ (- n 1) 2) (* a (square b)))))))
  (fast-expt-impl b n 1))

;; 1.17

(define (halve n)
  (/ n 2))

(define (double n)
  (* n 2))

(define (fast-mult a b)
  (cond ((or (= a 0) (= b 0)) 0)
        ((= b 1) a)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

;; 1.18

(define (fast-mult-iter a b)
    (cond ((or (eq? b 0) (eq? a 0)) 0)
          ((eq? a 1) b)
          ((even? a) (fast-mult-iter (halve a) (double b)))
          (else (+ b (fast-mult-iter (- a 1) b)))))