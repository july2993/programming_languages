#lang racket

(provide (all-defined-out)) ;;


(define (sequence lo hi diff)
 (cond [(<= lo hi) (cons lo (sequence (+ lo diff) hi diff))]
       [#t '()]
 ))

(define (string-append-map xs suffix)
 (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
 (cond [(< n 0) (error "list-nth-mod: negative number")]
  [(null? xs) (error "list-nth-mod: empty list")]
  [#t (list-ref xs (remainder n (length xs)))]
 ))

(define (stream-for-n-steps s n)
 (letrec ([f (lambda (s n xs)
          (cond [(= n 0) xs]
           [#t (f (cdr (s)) (- n 1) (cons (car (s)) xs))]))])
          (reverse (f s n '()))))


(define (funny-number-stream)
 (letrec ([f (lambda (x)
              (cond [(= (remainder x 5) 0) (cons (* x -1) (lambda () (f (+ x 1))))]
               [#t (cons x (lambda () (f (+ x 1))))]))])
  (f 1)))


(define (dan-then-dog)
 (letrec ([f (lambda (x)
              (cond [(= x 1) (cons "dan.jpg" (lambda () (f (0))))]
               [(= x 0) (cons "dog.jpg" (lambda () (f (1))))])
             )])
  (f 1)))

(define (stream-add-zero s)
 (letrec ([f (lambda (s)
  (let ([pr (s)])
   (cons (cons 0 (car pr)) (lambda () (cdr pr)))
 ))])
  (lambda () (f s))
 )
)


(define (cycle-lists xs ys)
 (letrec ([f (lambda (n xs ys)
              (cons (cons (list-ref xs (remainder n (length xs))) (list-ref ys (remainder n (length ys))))
               (lambda () (f (+ n 1) xs ys))))])
  (lambda () (f 0 xs ys))))


(define (vector-assoc v vec)
 (letrec ([f (lambda (v vec n)
              (cond [(>= n (vector-length vec)) #f]
                    [(not (pair? (vector-ref vec n))) (f v vec (+ n 1))]
               [(= (car (vector-ref vec n)) v) (vector-ref vec n)]
               [#t (f v vec (+ n 1))]))])
  (f v vec 0)))

(define (cached-assoc xs n)
 (letrec ([memo (make-vector n #f)]
         [idx 0]
         [f (lambda (v)
             (let ([ans (vector-assoc v memo)])
              (if ans
               (cdr ans)
               (let ([new-ans (assoc v xs)])
                (begin
                 (vector-set! memo idx new-ans)
                 (set! idx (remainder (+ idx 1) n))
                 new-ans)))))])
  f
  ))
