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
(define (cadenas-isomorfas? s1 s2)
  (define (longitud cadena)
    (if (null? cadena) 0
        (+ 1 (longitud (cdr cadena)))))

  (define (crear-mapeo s1 s2 mapa)
    (cond
      [(or (null? s1) (null? s2)) mapa] ; Caso base: ambas listas vacías
      [(hash-has-key? mapa (car s1)) ; Si el mapeo ya existe
       (if (char=? (hash-ref mapa (car s1)) (car s2))
           (crear-mapeo (cdr s1) (cdr s2) mapa) ; Continua si el mapeo es válido
           #f)] ; Si el mapeo es inconsistente, devuelve #f
      [else
       (crear-mapeo (cdr s1) (cdr s2) ; Crea nuevo mapeo
                    (hash-set mapa (car s1) (car s2)))]))

  (define (listas-isomorfas? l1 l2)
  (if (not (= (longitud l1) (longitud l2)))  
      #f
      (not (eq? (crear-mapeo l1 l2 (make-immutable-hash)) #f))))


  ; Convertir cadenas a listas de caracteres y verificar isomorfismo
  (listas-isomorfas? (string->list s1) (string->list s2))
)


; 4. Función que recibe un número entero n y devuelve verdadero si el número es automórfico y falso en otro caso.
(define (automorfico? num)
  (cond
   [(= (getUltimoDigito num) num) #t]
   [else #f]
  )
)

(define (getUltimoDigito n)
  (let ((cuadrado (* n n)))
    (modulo cuadrado 10))
)


; 5. Función recursiva que recibe una lista de números n y devuelve una lista de cadenas indicando si el número es si Positivo, Negativo o Cero
(define (clasificar-numeros lista)
  (cond
    [(null? lista) '()] ; Caso base: lista vacía, devuelve lista vacía
    [else
     (cons (cond [(> (car lista) 0) "Positivo"]
                 [(< (car lista) 0) "Negativo"]
                 [else "Cero"])
           (clasificar-numeros (cdr lista)))])) ; Llamada recursiva con el resto de la lista


; 6. Una función recursiva que simula lanzamientos de dados hasta alcanzar un número objetivo, recibe el número objetivo n y devuelve la lista de números generados hasta alcanzar el número objetivo. 
(define (lanzar-dado objetivo)
  (define (aux lanzar-lista)
    (let ((lanzamiento (+ 1 (random 6)))) ; Genera un número aleatorio entre 1 y 6
      (if (= lanzamiento objetivo)
          (append lanzar-lista (list lanzamiento)) ; Caso base: se alcanza el objetivo
          (aux (append lanzar-lista (list lanzamiento)))))) ; Llamada recursiva

  (aux '()))  ; Llamada inicial con lista vacía


; 7. Una función recursiva que rota una lista hacia la izquierda un número de veces dado, recibe una lista lst y un número para rotar n, devuelve la lista ya rotada.



; 8. Una función recursiva recibe una lista de números lst y devuelve la suma de los elementos de una lista alternando entre suma y restar cada elemento.



; 9. Una función recursiva recibe un número n y devuelve una lista de todas las combinaciones válidas de paréntesis para n pares.



; 10. Una función recursiva recibe un número entero n y devuelve el número de pasos necesarios para reducir el número dado a 1 siguiendo estas reglas:
; Si es par, divídelo en 2.
; Si es impar, multiplícalo por 3 y súmale 1.
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
(displayln (cadenas-isomorfas? "egg" "add"))
(displayln (cadenas-isomorfas? "foo" "bar"))
(displayln (cadenas-isomorfas? "paper" "title"))
(displayln (cadenas-isomorfas? "hi" "hello"))


(displayln "Ejercicio 4")
(displayln (automorfico? 5))
(displayln (automorfico? 6))
(displayln (automorfico? 23))
(displayln (automorfico? 9))

(displayln "Ejercicio 5")
(displayln (clasificar-numeros '(5 -3 0 12 -1 8 0)))
(displayln (clasificar-numeros '(23 -45 0 0 -1 8 0)))
(displayln (clasificar-numeros '(5 -3 0 2 1 1 0)))
(displayln (clasificar-numeros '(0 0 0 0 0 0 0)))

(displayln "Ejercicio 6")
(displayln (lanzar-dado 5))
(displayln (lanzar-dado 3))
(displayln (lanzar-dado 1))
(displayln (lanzar-dado 6))

(displayln "Ejercicio 7")

(displayln "Ejercicio 8")

(displayln "Ejercicio 9")

(displayln "Ejercicio 10")