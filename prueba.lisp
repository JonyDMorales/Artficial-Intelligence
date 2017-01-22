;Morales Torres Jonatan
;El mono y las bananas
;(load "/home/jonyd/Escritorio/Sexto Semestre/Artificial/Ejercicios/ddr.lisp")
;(in-package #:ddr)
;(load "/home/jonyd/Escritorio/Sexto Semestre/Artificial/Ejercicios/prueba.lisp")


;; Descripcion del problema

;Para este problema se plantea una habitacion con una puerta y delante de esta un mono
;a un costado en la parte alta unas bananas y debajo un palo, recorriendo la habiatacion
;tambien encontramos un banco, el cual utilizara el mono junto con el palo para poder alcanzar
;las bananas... 

(defparameter *monkey* '(
  
  (<- (comer-platanos mono ?mpos banco ?bpos palo ?ppos)
      (mono ?mpos)
      (banco ?bpos)
      (palo ?ppos)
      (tiene-palo) 
      (mover-banco)
      (sobre-banco)
  )
  
  ;;Acciones con el palo
  (<- (tiene-palo)
      (mover-mono-palo (mono ?pos)))

  (<- (mover-mono-palo (mono ?pos))
      (palo ?pos))

  ;;Acciones con el banco
  (<- (mover-banco)
      (mover-mono-banco (mono ?pos))
      (mover-banco (banco ?pos1) (mono ?pos2)))

  (<- (mover-mono-banco (mono ?pos))
      (banco ?pos))

  (<- (mover-banco (banco ?pos) (mono ?pos))
      (platanos ?pos))

  ;;Subir al banco
  (sobre-banco)

  ;;*******Conocimiento********
  (mono centro)   ;Unicación del mono
  (palo puerta)   ;Ubicación del palo
  (banco ventana) ;Ubicción del banco
  ;;Ubicacion de los platanos
  (platanos centro)
))

(init-kb *monkey*)

#|
;;*****Consultas*****
(ask '(comer-platanos mono ?mpos banco ?bpos palo ?ppos))
(ask-trace '(comer-platanos mono ?mpos banco ?bpos palo ?ppos))

;;****Posibilidades***

;;***Mono***
  (mono puerta)   ;Unicación del mono
  (mono centro)   ;Unicación del mono
  (mono ventana)  ;Unicación del mono
;;***Palo***
  (palo puerta)   ;Ubicación del palo
  (palo centro)   ;Ubicación del palo
  (palo ventana)  ;Ubicación del palo
;;***banco***
  (banco puerta)  ;Ubicción del banco
  (banco centro)  ;Ubicción del banco
  (banco ventana) ;Ubicción del banco
|#