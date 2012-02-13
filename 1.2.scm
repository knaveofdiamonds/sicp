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

(A 1 10)
; 1024

(A 2 4)
; 65536

(A 3 3)
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
	 (f (- n 2)) 
	 (f (- n 3)))))
