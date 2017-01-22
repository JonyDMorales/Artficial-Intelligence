;Morales Torres Jonatan 

;El quinto elemento de la lista (((1 2) 3) 4 (5(6)) 'A (B C) 'D '(E (F G))).
(first(CDDDDR '(((1 2) 3) 4 (5(6)) 'A (B C) 'D '(E (F G)))))

;El numero de segundos que tiene un año bisiesto.
(* 60 60 24 366)

;Si el numero asociado a la variable x es diferente de cero y ademas menor o igual que el
;valor asociado a la variable y.
(defvar x 5)
(defvar y 6)
(and (not (equal x 0)) (<= x y))

;Una lista con las dos soluciones reales de la ecuación.
((/ (+ (* -1 7) (sqrt (- (expt 7 2) (* 4 2 5)))) (* 2 2)) 
 (/ (- (* -1 7) (sqrt (- (expt 7 2) (* 4 2 5)))) (* 2 2)))

;Escriba en notación prefija:
(+ (* 2 4) (- 6 8))
(/ (+ 5 (+ -3 4)) (+ 6 (/ 2 5)))
(sqrt (/ (+ 1.4502 (* -1 (- -4 (/ 3 8)))) 
(expt (-1) (expt (- 3 5) (/ 1 3)))))
(expt (/ (expt (/ 65.402 (sqrt -1)) (/ 1 5)) 0.17 (/ 1 7))

;Indique el resultado de evaluar cada una de las siguientes expresiones.
(cadar '((one two) three four) )
two

(append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))
((eva lisa) karl sven eva lisa karl sven)

(subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))
(EVA GITAN LISA GITAN KARIN)

(remove 'sven '(eva sven lisa sven anna))
(EVA LISA ANNA)

(butlast '(karl adam nilsson gregg alisson vilma) 3)
(KARL ADAM NILSSON)

(nth 2 '(a b c d e))
C