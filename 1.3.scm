(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

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
                (* 4 (sum yk 1 (\lambda (i) (+ i 2)) (- n 1)))
                (* 2 (sum yk 2 (\lambda (i) (+ i 2)) (- n 1)))
                (yk n))))
                       
; Gives 1/4 for both n = 100 and n = 1000