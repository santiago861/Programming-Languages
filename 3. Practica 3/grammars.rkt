#lang plai
#|
Team members:
- Reyes Medina Santiago Iván - 320308598
- López Espinoza Ashley Yael - 319129391
- Genaro de Jesus Miranda Martinez - 411000758
|#

;; Definición del tipo Binding
(define-type Binding
  [binding (id symbol?) (value FWAE?)])

;; Definición del tipo FWAE
(define-type FWAE
  [id    (i symbol?)]
  [num   (n number?)]
  [op    (f procedure?) (args (listof FWAE?))]
  [with  (bindings (listof binding?)) (body FWAE?)]
  [with* (bindings (listof binding?)) (body FWAE?)])