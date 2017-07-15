
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;;1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;;2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

;;3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;;4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (letrec ([pair (s)])
        (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))))

;;5
(define (funny-number-stream)
  (define (f n)
    (letrec ([t (if (= (remainder n 5) 0) (- n) n)])
      (cons t (lambda () (f (+ n 1))))))
  (f 1))

;;6
(define (dan-then-dog)
  (define (f lst)
    (cons (car lst) (lambda () (f (reverse lst)))))
  (f (list "dan.jpg" "dog.jpg")))

;;(define dan-then-dog
;;  (letrec ([dan-st (lambda () (cons "dan.jpg" dog-st))]
;;           [dog-st (lambda () (cons "dog.jpg" dan-st))])
;;    dan-st))

;;(define (dan-then-dog)
;;  (cons "dan.jpg"
;;        (lambda () (cons "dog.jpg" dan-then-dog))))

;;7
(define (stream-add-zero s)
  (letrec [(raw-pair (s))]
    (lambda () (cons (cons 0 (car raw-pair))
                     (stream-add-zero (cdr raw-pair))))))

;;8
(define (cycle-lists xs ys)
  (define (helper n)
    (lambda () (cons
                (cons (list-nth-mod xs n) (list-nth-mod ys n))
                (helper (+ n 1)))))
  (helper 0))

;;9
(define (vector-assoc v vec)
  (define len (vector-length vec))
  (define (helper n)
    (if (>= n len) #f
        (letrec [(cur (vector-ref vec n))]
          (if (and (pair? cur) (equal? v (car cur)))
              cur
              (helper (+ n 1))))))
  (helper 0))

;;10
(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define next 0)
  (define (incre-next) (if (= next (- n 1)) (set! next 0) (set! next (+ next 1))))
  (lambda (v)
    (letrec ([cache-p (vector-assoc v cache)])
      (if cache-p cache-p
          (letrec ([raw-p (assoc v xs)])
            (if raw-p
                (begin (vector-set! cache next raw-p)
                    (incre-next)
                    raw-p)
                raw-p))))))

;;11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less exp do body)
     (letrec ([e exp]
              [loop (lambda (thk)
                      (letrec ([thk-v (thk)])
                        (if (< thk-v e)
                            (loop thk)
                            #t)))])
       (loop (lambda () body)))]))

;;(define-syntax while-less
;;  (syntax-rules (do)
;;    ((while-less x do y)
;;      (let ([z x])
;;        (letrec ([loop (lambda ()
;;			                  (let ([w y])
;;		 	                    (if (or (not (number? w)) (>= w z))
;;			                        #t
;;			                        (loop))))])
;;          (loop))))))