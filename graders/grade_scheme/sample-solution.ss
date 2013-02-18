(define plus1
  (lambda (n)
    (+ n 1)))

(define minus1
  (lambda (n)
    (- n 1)))

(define tens
  (lambda (n)
    (remainder (quotient n 10) 10)))

(define negative-magnitude
  (lambda (n)
    (* -1 (abs n))))

(define cube
  (lambda (n)
    (* n n n)))

(define subtract-cubes
  (lambda (x y)
    (- (cube x) (cube y))))

(define yd->in
  (lambda (x)
    (* 36 x)))

(define in->cm
  (lambda (x)
    (* 2.54 x)))

(define yd->cm
  (lambda (x)
    (in->cm (yd->in x))))

(define celsius->fahrenheit
  (lambda (c)
    (+ (* 9/5 c) 32)))

(define fahrenheit->celsius
  (lambda (f)
    (* (- f 32) 5/9)))

(define fluctuate
  (lambda (n)
    (+ (random 5) (- n 2))))

(define char-at
  (lambda (s i)
    (substring s i (+ i 1))))
(define random-char
  (lambda (s)
    (let ([n (random (string-length s))])
      (substring s n (+ n 1)))))

(define make-simile
  (lambda (x y)
    (string-append x " as " y)))

(define left-part
  (lambda (w)
    (substring w 0 (quotient (string-length w) 2))))

(define right-part
  (lambda (w)
    (substring w (+ (quotient (string-length w) 2)
                   (remainder (string-length w) 2)) (string-length w))))

(define middle
  (lambda (w)
    (substring w (quotient (string-length w) 2)
      (+ (quotient (string-length w) 2) (remainder (string-length w) 2)))))

(define random-in-range
  (lambda (x y)
    (+ x (random (+ (- y x) 1)))))

(define random-jump
  (lambda (n dist)
    (random-in-range (- n dist) (+ n dist))))
