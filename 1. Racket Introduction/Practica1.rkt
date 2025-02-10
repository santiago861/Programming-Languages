#lang racket  ; Especifica el lenguaje Racket

#|
Team members:
- Reyes Medina Santiago Iván - 320308598
- López Espinoza Ashley Yael - 319129391
|#






; 1. Función que verifica si la cadena recibida tiene paréntesis balanceados
(define (parentesis-balanceados? cadena)
  (aux1 cadena 0 0)
)

(define (aux1 cadena balance index)
  (cond
    [(= index (string-length cadena)) (= balance 0)]
    [(< balance 0) #f] ; esto maneja el caso donde directamente se rompe el balance ')('
    [(char=? (string-ref cadena index) #\()
     (aux1 cadena (+ balance 1) (+ index 1))]
    [(char=? (string-ref cadena index) #\))
     (aux1 cadena (- balance 1) (+ index 1))]
    [else (aux1 cadena balance (+ index 1))]
  )
)


; 2. Función que verifica si la cadena recibida tiene más vocales que consonantes
(define (vocal-predominante? cadena)
  (aux2 cadena 0 0 0)
)

(define (aux2 cadena countVocales countConsonantes index)
  (cond
    [(= index (string-length cadena)) (> countVocales countConsonantes)]
    [(or (char=? (string-ref cadena index) #\a) (char=? (string-ref cadena index) #\e) (char=? (string-ref cadena index) #\i) (char=? (string-ref cadena index) #\o) (char=? (string-ref cadena index) #\u))
     (aux2 cadena (+ countVocales 1) countConsonantes (+ index 1))]
    [else (aux2 cadena countVocales (+ countConsonantes 1) (+ index 1))]
  )
)


; 3. Función que recibe dos cadenas s1 y s2 y devuelva verdadero si las cadenas son isomorfas y falso en otro caso.



; 4. Función que recibe un número entero n y devuelve verdadero si el número es automórfico y falso en otro caso.



; 5. Función recursiva que recibe una lista de números n y devuelve una lista de cadenas indicando si el número es si Positivo, Negativo o Cero



; 



; Pruebas
(displayln "Ejercicio 1")
(displayln (parentesis-balanceados? "((()"))
(displayln (parentesis-balanceados? "(())"))
(displayln (parentesis-balanceados? "()"))
(displayln (parentesis-balanceados? ")("))
(displayln "Ejercicio 2")
(displayln (vocal-predominante? "adgaaaasc"))
(displayln (vocal-predominante? "adfttkyasc"))
(displayln (vocal-predominante? "jskdlfnavcs"))
(displayln (vocal-predominante? "aeioufj"))
(displayln "Ejercicio 3")
(displayln "Ejercicio 4")
(displayln "Ejercicio 5")
(displayln "Ejercicio 6")
(displayln "Ejercicio 7")
(displayln "Ejercicio 8")
(displayln "Ejercicio 9")
(displayln "Ejercicio 10")