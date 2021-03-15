#lang racket

; eval with proper namespace
(define my-eval
  (let ((ns (make-base-namespace)))
    (lambda (expr) (eval expr ns))))

; check if value is a lambda expr
(define (lambda? x)
  (member x '(lambda λ)))

;check if value is a quote
(define (quote? x)
  (equal? x 'quote))

; check if value is an if expr
(define (if? x)
  (equal? x (car '(if ))))

; compares two Scheme expressions x and y, and produces a difference summary of them
(define (expr-compare x y)
    ;(display x) (display ", ") (display y) (newline)
    (cond
        ; if expressions are equal, return one
        [(equal? x y) x]
        ; handle boolean case
        [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
        ; handle numbers case
        [(and (number? x) (number? y)) (list 'if '% x y)]
        ; handle case where either one is not a list
        [(or (not (list? x)) 
             (not (list? y)))
         (list 'if '% x y)]
         ; handle case where expressions are list of unequal length
        [(not (equal? (length x) (length y))) (list 'if '% x y)]
        ; cases where expressions are lists of equal length
        [else (expr-compare-lists x y)]
    )
)

; given lists of equal length, compare similarities
; handles the special forms of expressions (lambda, if, ...)
(define (expr-compare-lists x y)
    (cond
        ; case where either list is actually just a quoted literal
        [(or (quote? (car x)) (quote? (car y))) (list 'if '% x y)]
        ; case where lists are a special form if expression
        [(and (if? (car x)) (if? (car y))) 
        (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
        ; case where one is an if and another is not
        [(and (or (if? (car x)) (if? (car y))) (or (equal? (length x) 4) (equal? (length y) 4))) (list 'if '% x y)]
        ; case where lists are a special form lambda expression
        [(and (lambda? (car x)) (lambda? (car y))) 
          (if (and (equal? (length x) 3) (list? (car (cdr x))))
            (expr-compare-lambda x y)
            (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y))))
        ]
        ; case where one expression is a proper lambda function and other is not
        [(or (and (lambda? (car x)) (equal? (length x) 3)) (and (lambda? (car y)) (equal? (length y) 3))) (list 'if '% x y)]
        ; all other cases
        [else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
    )
)

; handle lambda case special form
(define (expr-compare-lambda x y)
    (cond
      ; if the two lambda functions have different arguments, don't combine
      [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% x y)]
      ; case where at least one of the lambdas is a λ
      [(or (equal? (car x) 'λ) (equal? (car y) 'λ)) (cons 'λ (expr-compare-lambda-helper (cdr x) (cdr y)))]
      ; case where neither are λ
      [else (cons 'lambda (expr-compare-lambda-helper (cdr x) (cdr y)))]
    )
)

; function that takes in the arguments and bodies of two lambda expressions x and y
; x and y of the form: ((formals) (expr))
(define (expr-compare-lambda-helper x y)
  ; define the hashmaps for lambda expressions
  (define lambda-hashmap-x (make-weak-hash))
  (define lambda-hashmap-y (make-weak-hash))
  ; append list of formals to expr having properly hashed and replaced values
  (cons (expr-compare-lambda-formal (car x) (car y) lambda-hashmap-x lambda-hashmap-y) (expr-compare (replace-all (cdr x) lambda-hashmap-x) (replace-all (cdr y) lambda-hashmap-y)))
)

; function to compare and hash formal values
(define (expr-compare-lambda-formal x y Hmapx Hmapy)
    (cond
        ; args are the same
        [(equal? x y) x]
        ; Either arg is null
        [(or (equal? x '()) (equal? y '())) null]
        ; recurse through both lists of formals:
        [(and (list? x) (list? y)) (cons (expr-compare-lambda-formal (car x) (car y) Hmapx Hmapy) (expr-compare-lambda-formal (cdr x) (cdr y) Hmapx Hmapy))]
        ; Set hashmap values
        [else (hash-set! Hmapx x (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))
              (hash-set! Hmapy y (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))
              (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))]
    )
)

; function to replace all values in expr x s.t. x is a lambda expression body and Hmap is the hash map of values
(define (replace-all x Hmap)
    (cond
        [(null? x) null]
        ; If a nested lambda expression is encountered
        [(and (list? x) (equal? (length x) 3) (lambda? (car x)) (list? (car (cdr x)))) 
            ; create a new hashmap that is a copy of original but with nested lambda args rehashed
            (define lambda-hashmap2 (hash-copy Hmap)) 
            (cons (replace-all (car x) Hmap) (replace-all (cdr x) (rehash-formals (cdr x) lambda-hashmap2)))
          ]
        ; case where a value in the lambda body is an if statement
        [(and (list? x) (equal? (length x) 4) (if? (car x))) (cons 'if (replace-all (cdr x) Hmap))]
        ; case where a value in the lambda body is a quoted expression
        [(and (list? x) (quote? (car x))) x]
        ; recurse through list and replace:
        [(list? x) (cons (replace-all (car x) Hmap) (replace-all (cdr x) Hmap))]
        [else (hash-ref Hmap x x)]
    )
)

; rehash the formals inside a nested lambda to themselves
(define (rehash-formals x Hmap)
  (cond
    [(null? x) Hmap]
    [(list? (car x)) (rehash-formals (car x) Hmap)]
    [else (hash-set! Hmap (car x) (car x))
          (rehash-formals (cdr x) Hmap)]
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tests implementation of expr-compare by using eval and comparing 
; return values of expr-compare based off the value of %
(define (test-expr-compare x y)
   (let ((test1 (equal? (my-eval (list 'let '([% #t]) (expr-compare x y))) (my-eval x))))
        (let ((test2 (equal? (my-eval (list 'let '([% #f]) (expr-compare x y))) (my-eval y))))
            (and test1 test2)
        )
   )
)

(define test-expr-x
  '(lambda (x y) (if x '(a b) ((lambda (t) t) y)))
)

(define test-expr-y
  '(λ (a b) (if b '(a b) ((λ (z) z) a)))
)

; Test Cases:

(display "1 ") (equal? (expr-compare 12 12) '12)
(equal? (expr-compare 12 20) '(if % 12 20))
(equal? (expr-compare #t #t) #t)
(equal? (expr-compare #f #f) #f)
(display "5 ") (equal? (expr-compare #t #f) '%)
(equal? (expr-compare #f #t) '(not %))
(equal? (expr-compare 'a '(cons a b)) '(if % a (cons a b)))
(equal? (expr-compare '(cons a b) '(cons a b)) '(cons a b))
(equal? (expr-compare '(cons a lambda) '(cons a λ)) '(cons a (if % lambda λ)))
(display "10 ") (equal? (expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c))) '(cons (cons a (if % b c)) (cons (if % b a) c)))
(equal? (expr-compare '(cons a b) '(list a b)) '((if % cons list) a b))
(equal? (expr-compare '(list) '(list a)) '(if % (list) (list a)))
(equal? (expr-compare ''(a b) ''(a c)) '(if % '(a b) '(a c)))
(equal? (expr-compare '(quote (a b)) '(quote (a c))) '(if % '(a b) '(a c)))
(display "15 ") (equal? (expr-compare '(quoth (a b)) '(quoth (a c))) '(quoth (a (if % b c))))
(equal? (expr-compare '(if x y z) '(if x z z)) '(if x (if % y z) z))
(equal? (expr-compare '(if x y z) '(g x y z)) '(if % (if x y z) (g x y z)))
(equal? (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)) '((lambda (a) ((if % f g) a)) (if % 1 2)))
(equal? (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2)) '((λ (a) ((if % f g) a)) (if % 1 2)))
(display "20 ") (equal? (expr-compare '((lambda (a) a) c) '((lambda (b) b) d)) '((lambda (a!b) a!b) (if % c d)))
(equal? (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d)) '(if % '((λ (a) a) c) '((lambda (b) b) d)))
(equal? (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2))) '(+
     (not %)
     ((λ (a b!c) (f a b!c)) 1 2)))
(equal? (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2)) '((λ (a b) (f (if % a b) (if % b a))) 1 2))
(equal? (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2)) '((λ (a b!c) (f (if % a b!c) (if % b!c a))) 1 2))

(display "25 ") (equal? (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3)) '((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ)))) 3))
(equal? (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b)))) '((λ (a)
      ((if % eq? eqv?)
       a
       ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))
        a (λ (a!b) (if % a!b a)))))
     (lambda (b!a a!b) (b!a a!b))))

(equal? (expr-compare '(cons a lambda) '(cons a λ))
      '(cons a (if % lambda λ)))
(equal? (expr-compare '(lambda (a) a) '(lambda (b) b))
       '(lambda (a!b) a!b))
(equal? (expr-compare '(lambda (a) b) '(cons (c) b))
      '(if % (lambda (a) b) (cons (c) b)))
(display "30 ") (equal? (expr-compare '((λ (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3))
      '((λ (if!fi) (+ if!fi 1)) 3))
(equal? (expr-compare '(lambda (lambda) lambda) '(λ (λ) λ))
       '(λ (lambda!λ) lambda!λ))
(equal? (expr-compare ''lambda '(quote λ))
      '(if % 'lambda 'λ))
(equal? (expr-compare '(lambda (a b) a) '(λ (b) b))
      '(if % (lambda (a b) a) (λ (b) b)))
(equal? (expr-compare '(λ (a b) (lambda (b) b)) '(lambda (b) (λ (b) b)))
       '(if % (λ (a b) (lambda (b) b)) (lambda (b) (λ (b) b))))
(display "35 ") (equal? (expr-compare '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y)))
       '(λ (let) (let (((if % x y) 1)) (if % x y))))
(equal? (expr-compare '(λ (x) ((λ (x) x) x))
              '(λ (y) ((λ (x) y) x)))
     '(λ (x!y) ((λ (x) (if % x x!y)) (if % x!y x))))
(equal? (expr-compare '(((λ (g)
                   ((λ (x) (g (λ () (x x))))     ; This is the way we define a recursive function
                    (λ (x) (g (λ () (x x))))))   ; when we don't have 'letrec'
                 (λ (r)                               ; Here (r) will be the function itself
                   (λ (n) (if (= n 0)
                              1
                              (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
                10)
              '(((λ (x)
                   ((λ (n) (x (λ () (n n))))
                    (λ (r) (x (λ () (r r))))))
                 (λ (g)
                   (λ (x) (if (= x 0)
                              1
                              (* x ((g) (- x 1)))))))
                9))
      '(((λ (g!x)
                  ((λ (x!n) (g!x (λ () (x!n x!n))))
                   (λ (x!r) (g!x (λ () (x!r x!r))))))
                (λ (r!g)
                  (λ (n!x) (if (= n!x 0)
                               1
                               (* n!x ((r!g) (- n!x 1)))))))
               (if % 10 9)))