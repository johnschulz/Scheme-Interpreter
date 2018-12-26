(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (lst) (append (list first) lst)) rests)
)
(define (zip pairs)
  (if (pair? pairs)
  (list (map car pairs) (map car (map cdr pairs)))
  ())
  )

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (helper s n)
      (if (pair? s)
        (cons (list n (car s)) (helper (cdr s) (+ n 1)))
        ()
      )
  )
  (helper s 0)
  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
      ((or (null? denoms) (< total 0)) ())
      ((= total 0) (cons (cons (car denoms) nil) nil))
      ((> (car denoms) total) (list-change total (cdr denoms)))
      ((= (car denoms) total) (append (list-change (- total (car denoms)) denoms) (list-change total (cdr denoms))))
      (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
      )
  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
          (if (null? (cdr body))
             (cons form (cons params (let-to-lambda body)))
             (cons form (cons params (cons (car body) (cons (let-to-lambda (car (cdr body))) nil))))
           )
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (define parameters (car (zip values)))
           (define nums (cdr (zip values)))
           (cons (cons 'lambda (cons parameters (cons (let-to-lambda (car body)) nil))) (cons (let-to-lambda (car (car nums))) (cdr (car nums))))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
