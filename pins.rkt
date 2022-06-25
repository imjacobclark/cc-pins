#lang racket

(define ht #hash(
            (0 . (list 1 2 3))
            (1 . (list 4 5 6))
            (2 . (list 7 8 9))
            (3 . (list 0))))

(define (up x) (match x [x #:when (> x 0) (- x 1)] [x '()]))
(define (down x) (match x [x #:when (< x 2) (+ x 1)] [x '()]))
(define (perms x) (cons (up x) (down x)))

(define (m x)
  (match x
    [x #:when (eq? 0 x) 3]
    [x #:when (and (>= x 0) (<= x 1)) 0]
    [x #:when (and (>= x 1) (<= x 2)) 1]
    [x #:when (and (>= x 2) (<= x 3)) 2]
    [x '()]))

(define (pivot-around-column row pivot-points)
  (map (lambda (number)
         (list-ref (hash-ref ht number) (+ row 1)))
       pivot-points))

(define (pivot-around-row row pivot-points)
  (map (lambda (number)
         (list-ref (hash-ref ht row) (+ number 1)))
       pivot-points))

(define (number-to-pad-position num)
  (define num-as-num (string->number num))
  (define column (m (/ num-as-num 3)))
  (define row (- (- num-as-num (* (m (/ num-as-num 3)) 3)) 1))
  (define current-number (list-ref (hash-ref ht column) (+ row 1)))
  (define columns (flatten (cons column (perms column))))
  (define rows (flatten (cons row (flatten (perms row)))))
  
  (list (flatten (pivot-around-column row columns)) (pivot-around-row column rows)))

(define (get-row num)
  (map (lambda (num) (number-to-pad-position num)) num))

(define (run approxpin)
  (get-row (map string (string->list (number->string approxpin)))))

(run 46)