
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (λ (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error"list-nth-mod: empty list")]
        [#t (list-ref xs (remainder n (length xs)))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([negate (λ (n) (* n -1))]
           [div-by-5 (λ (n) (= (remainder n 5) 0))]
           [funny-number (λ (n) (if (div-by-5 n) (negate n) n))]
           [f (λ (n) (cons (funny-number n) (λ () (f (+ n 1)))))])
    (λ () (f 1))))

(define dan-then-dog
  (letrec ([f (λ () (cons "dan.jpg" g))]
           [g (λ () (cons "dog.jpg" f))])
    f))

(define (stream-add-zero s)
  (letrec ([f (λ () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s)))))])
    f))

(define (cycle-lists xs ys)
  (letrec ([f (λ (n) (cons (cons (list-nth-mod xs n)
                                      (list-nth-mod ys n))
                                (λ () (f (+ n 1)))))])
    (λ () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (λ (n)
                (cond [(= n (vector-length vec)) #f]
                      [(not (pair? (vector-ref vec n))) (f (+ n 1))]
                      [#t (let ([elm (vector-ref vec n)])
                            (if (equal? (car elm) v) elm (f (+ n 1))))]))])
    (f 0)))

(define (cached-assoc xs n)
  (let* ([cache-vector (make-vector n)]
         [cache-counter 0]
         [inc-counter (λ (c) (remainder (+ c 1) n))]
         [add-to-cache-and-return (λ (v)
                                    (let ([assoc-result (assoc v xs)])
                                      (if assoc-result
                                          ; as hw description says, add the pair to 
                                          ; the cache only if result is not #f.
                                          (begin (vector-set! cache-vector cache-counter assoc-result)
                                                 (set! cache-counter (inc-counter cache-counter))
                                                 assoc-result)
                                          #f)))])
    (λ (v)
      (let ([cache-result (vector-assoc v cache-vector)])
        (if cache-result
            cache-result
            (add-to-cache-and-return v))))))

; challenge problem
(define-syntax while-less
  (syntax-rules (while-less do)
    [(while-less e1 do e2)
     (let ([evaluated-e1 e1])
       (letrec ([loop (λ (it)
                        (if (< it evaluated-e1)
                            (loop e2)
                            #t))])
         (loop e2)))]))
       
             
           
             
      
    
    