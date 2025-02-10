#lang racket  ; Especifica el lenguaje Racket

#|
Team members:
- Reyes Medina Santiago Iván - 320308598
- López Espinoza Ashley Yael - 319129391
|#






; Función que verifica si la cadena recibida tiene paréntesis balanceados
(define (parentesis-balanceados? cadena)
  (aux cadena 0 0)
)

(define (aux cadena balance index)
  (cond
    [(= index (string-length cadena)) (= balance 0)]
    [(< balance 0) #f] ; esto maneja el caso donde directamente se rompe el balance ')('
    [(char=? (string-ref cadena index) #\()
     (aux cadena (+ balance 1) (+ index 1))]
    [(char=? (string-ref cadena index) #\))
     (aux cadena (- balance 1) (+ index 1))]
    [else (aux cadena balance (+ index 1))]
  )
)

; Pruebas
(displayln "Ejercicio 1")
(displayln (parentesis-balanceados? "((()"))
(displayln (parentesis-balanceados? "(())"))
(displayln (parentesis-balanceados? "()"))
(displayln (parentesis-balanceados? ")("))
(displayln "Ejercicio 2")
(displayln "Ejercicio 3")
(displayln "Ejercicio 4")
(displayln "Ejercicio 5")
(displayln "Ejercicio 6")
(displayln "Ejercicio 7")
(displayln "Ejercicio 8")
(displayln "Ejercicio 9")
(displayln "Ejercicio 10")