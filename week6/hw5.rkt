;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist rlist)
  (if (null? rlist)
      (aunit)
      (apair (car rlist) (racketlist->mupllist (cdr rlist)))))

(define (mupllist->racketlist mlist)
  (if (aunit? mlist)
      null
      (cons (apair-e1 mlist) (mupllist->racketlist (apair-e2 mlist)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond 
    ;; A variable evaluates to the value associated with it in the environment.
    [(var? e)
         (envlookup env (var-string e))]
    ;; An addition evaluates its subexpressions and assuming they both produce
    ;; integers, produces the integer that is their sum.
    [(add? e) 
     (let ([v1 (eval-under-env (add-e1 e) env)]
           [v2 (eval-under-env (add-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
           (int (+ (int-num v1) 
                   (int-num v2)))
           (error "MUPL addition applied to non-number")))]
    ;; All values (including closures) evaluate to themselves.
    [(int? e)
     e]
    [(closure? e)
     e]
    ;; Functions are lexically scoped: A function evaluates to a closure holding the
    ;; function and the current environment
    [(fun? e)
     (closure env e)]
    ;; An ifgreater evaluates its first two subexpressions to values v1 and v2.  If
    ;; both values are integers, it evaluates its third subexpression if v1 is
    ;; strictly greater integer than v2 else it evaluates its fourth subexpression.
    [(ifgreater? e)
     (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
           [v2 (eval-under-env (ifgreater-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
           (if (> (int-num v1) (int-num v2))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env))
           (error "MUPL ifgreater applied to non-number")))]
    ;; An mlet expression evaluates its first expression to a value v. Then it
    ;; evaluates the second expression to a value, in an environment extended to map
    ;; the name in the mlet expression to v.
    [(mlet? e)
     (let* ([v (eval-under-env (mlet-e e) env)]
            [ext-env (cons (cons (mlet-var e) v) env)])
       (eval-under-env (mlet-body e) ext-env))]
    ;; A call evaluates its first and second subexpressions to values. If the first
    ;; is not a closure, it is an error. Else, it evaluates the closure's function's
    ;; body in the closure's environment extended to map the function's name to the
    ;; closure (unless the name field is #f) and the function's argument to the
    ;; result of the second subexpression.
    [(call? e)
     (let ([v1 (eval-under-env (call-funexp e) env)]
           [v2 (eval-under-env (call-actual e) env)])
       (if (closure? v1)
           (let* ([c-fun (closure-fun v1)]
                  [c-env (closure-env v1)]
                  [ext-env-temp (cons (cons (fun-formal c-fun) v2) c-env)]
                  [ext-env (if (fun-nameopt c-fun)
                               (cons (cons (fun-nameopt c-fun) v1) ext-env-temp)
                               ext-env-temp)])
             (eval-under-env (fun-body c-fun) ext-env))
           (error "MUPL call first-subexpression not closure")))]
    ;; A pair expression evaluates its two subexpressions and produces a (new)
    ;; pair holding the results.
    [(apair? e)
     (let ([v1 (eval-under-env (apair-e1 e) env)]
           [v2 (eval-under-env (apair-e2 e) env)])
       (apair v1 v2))]
     ;(racketlist->mupllist (list v1 v2)))]
    ;; A fst expression evaluates its subexpression. If the result for the
    ;; subexpression is a pair, then the result for the fst expression is the e1
    ;; field in the pair.
    [(fst? e)
     (let ([v (eval-under-env (fst-e e) env)])
       (if (apair? v)
           (apair-e1 v)
           (error "MUPL fst applied to non-pair")))]
    ;; A snd expression evaluates its subexpression. If the result for the
    ;; subexpression is a pair, then the result for the snd expression is the e2
    ;; field in the pair.
    [(snd? e)
     (let ([v (eval-under-env (snd-e e) env)])
       (if (apair? v)
           (apair-e2 v)
           (error "MUPL snd applied to non-pair")))]
    ;; An isaunit expression evaluates its subexpression. If the result is an aunit
    ;; expression, then the result for the isaunit expression is the mupl value (int
    ;; 1), else the result is the mupl value (int 0).
    [(isaunit? e)
     (let ([v (eval-under-env (isaunit-e e) env)])
       (if (aunit? v) (int 1) (int 0)))]
    ;; aunit
    [(aunit? e)
     (aunit)]
    ;; Otherwise it is a bad MUPL expression
    [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4) 
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "mupl-fun" (fun "g" "mupl-lst" (ifaunit (var "mupl-lst")
                                                  (aunit)
                                                  (apair (call (var "mupl-fun") (fst (var "mupl-lst")))
                                                         (call (var "g") (snd (var "mupl-lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i" (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
