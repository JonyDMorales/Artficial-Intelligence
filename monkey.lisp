;Morales Torres Jonatan
;El mono y las bananas
;(load "/home/jonyd/Escritorio/Sexto Semestre/Artificial/Ejercicios/ddr.lisp")
;(in-package #:ddr)
;(load "/home/jonyd/Escritorio/Sexto Semestre/Artificial/Ejercicios/monkey.lisp")


;; Descripcion del problema

;Para este problema se plantea una habitacion con una puerta y delante de esta un mono
;a un costado en la parte alta unas bananas y debajo un vara, recorriendo la habiatacion
;tambien encontramos un banco, el cual utilizara el mono junto con el vara para poder alcanzar
;las bananas... 

(defparameter *monkey* '(
  
  ;;Consulta principal
  (<- (comer-platanos (estado-inicial mono ?mpos banco ?bpos vara ?ppos) (?accionesp ?accionesb))
      (pos mono ?mpos)    ;;Posicion inicial del mono
      (pos banco ?bpos)   ;;Posicion inicial del banco
      (pos vara ?ppos)    ;;Posicion inicial de la vara
      (tiene-vara ?accionesp)   ;;Metodo que mueve al mono por la vara
      (mover-banco ?accionesb)  ;;Metodo que mueve al mono por el banco y lo coloca en su posicion
  )
  
  ;;Acciones con el vara
  (<- (tiene-vara (?accionesp1 ?accionesp2))
      (mover-mono-vara ?accionesp1)
      (tiene-mono-vara ?accionesp2))

  ;;Mueve al mono por la vara
  (<- (mover-mono-vara (se-mueve ?pos))
      (pos vara ?pos))

  ;;Toma la vara
  (<- (tiene-mono-vara ((toma-mono-vara) tiene-vara!))
      (pos vara ?x))

  ;;Acciones con el banco
  (<- (mover-banco (?accionesb1 ?accionesb2 ?accionesb3) )
      (mover-mono-banco ?accionesb1)
      (tiene-mono-banco ?accionesb2)
      (mono-lleva-banco ?accionesb3))

  ;;Mueve al mono por el banco
  (<- (mover-mono-banco (se-mueve ?pos))
      (pos banco ?pos))

  ;;Toma mono el banco
  (<- (tiene-mono-banco ((toma-mono-banco) tiene-banco!))
      (pos vara ?x))

  ;;Movimiento final, el mono coloca el banco en la posicion de las bananas
  (<- (mono-lleva-banco ((mono-lleva-banco ?pos) subir-al-banco Logrado!))
      (pos platanos ?pos))

  ;;*******Conocimiento********
  (pos mono puerta)   ;Unicación del mono
  (pos vara puerta)   ;Ubicación del vara
  (pos banco ventana) ;Ubicción del banco
  ;;Ubicacion de los platanos
  (pos platanos centro)
))

(init-kb *monkey*)

#|
;;*****Consultas*****
(ask '(comer-platanos (estado-inicial mono ?mpos banco ?bpos vara ?ppos) ?acciones))
(ask-trace '(comer-platanos (estado-inicial mono ?mpos banco ?bpos vara ?ppos) ?acciones))

;;****Posibilidades***

;;***Mono***
  (pos mono puerta)   ;Unicación del mono
  (pos mono centro)   ;Unicación del mono
  (pos mono ventana)  ;Unicación del mono
;;***vara***
  (pos vara puerta)   ;Ubicación del vara
  (pos vara centro)   ;Ubicación del vara
  (pos vara ventana)  ;Ubicación del vara
;;***banco***
  (pos banco puerta)  ;Ubicción del banco
  (pos banco centro)  ;Ubicción del banco
  (pos banco ventana) ;Ubicción del banco
|#