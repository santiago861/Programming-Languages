#lang plai

#|
Team members:
- Reyes Medina Santiago Iván - 320308598
- López Espinoza Ashley Yael - 319129391
- Genaro de Jesus Miranda Martinez - 411000758
|#

(require (file "./grammars.rkt"))

;; parse : s-expression -> FWAE
(define (parse sexp)
  (cond
    [(symbol? sexp) (id sexp)]
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (match sexp
       [(list '+ e1 e2 ...) (op + (map parse (cons e1 e2)))]
       [(list '- e1 e2 ...) (op - (map parse (cons e1 e2)))]
       [(list '* e1 e2 ...) (op * (map parse (cons e1 e2)))]
       [(list '/ e1 e2 ...) (op / (map parse (cons e1 e2)))]
       [(list 'modulo e1 e2) (op modulo (list (parse e1) (parse e2)))]
       [(list 'expt e1 e2) (op expt (list (parse e1) (parse e2)))]
       [(list 'add1 e) (op (lambda (x) (+ x 1)) (list (parse e)))]
       [(list 'sub1 e) (op (lambda (x) (- x 1)) (list (parse e)))]
       [(list 'with bindings body)
        (if (and (list? bindings) (andmap (lambda (b) (and (list? b) (= (length b) 2))) bindings))
            (with (map (lambda (b) (binding (first b) (parse (second b)))) bindings)
                  (parse body))
            (error "Syntax Error: Expresión mal formada en parse"))]
       [(list 'with* bindings body)
        (if (and (list? bindings) (andmap (lambda (b) (and (list? b) (= (length b) 2))) bindings))
            (with* (map (lambda (b) (binding (first b) (parse (second b)))) bindings)
                   (parse body))
            (error "Syntax Error: Expresión mal formada en parse"))])]
    [else (error "Syntax Error: Expresión mal formada en parse")]))
