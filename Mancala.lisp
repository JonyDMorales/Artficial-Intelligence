;Morales Torres Jonatan
;Mancala
;(load "/home/jonyd/Escritorio/Sexto Semestre/Artificial/Ejercicios/Mancala.lisp")

(defparameter  *depth* 6)					;;Profundidad en el arbol
(defparameter  *turno* 0)					;;Define el turno de cada jugador
(defparameter  *estado-inicial* (make-array '(2 7) :initial-contents '( 	;;Estado inicial del juego
								 ((0 0 0)
								  (1 1 1) 
								  (1 1 1) 
								  (1 1 1) 
								  (1 1 1) 
								  (1 1 1) 
								  (1 1 1))	

								 ((0 0 0)
								  (1 1 1) 
								  (1 1 1) 
								  (1 1 1) 
								  (1 1 1) 
								  (1 1 1) 
								  (1 1 1)))))

;Imprime en consola un estado
(defun Display-Mancala (estado)
	(let* (
		(m1 (first  (aref estado 0 0)))
		(m2 (second (aref estado 0 0)))
		(m3 (third  (aref estado 0 0)))
		(TM (+ m1 (* m2 5) (* m3 10)))

		(hm1 (aref estado 0 1))		;;Casillas de la maquina
		(hm2 (aref estado 0 2))
		(hm3 (aref estado 0 3))
		(hm4 (aref estado 0 4))
		(hm5 (aref estado 0 5))
		(hm6 (aref estado 0 6))

		(ho1 (aref estado 1 1))		;;Casilla del oponente
		(ho2 (aref estado 1 2))
		(ho3 (aref estado 1 3))
		(ho4 (aref estado 1 4))
		(ho5 (aref estado 1 5))
		(ho6 (aref estado 1 6))

		(o1 (first  (aref estado 1 0)))
		(o2 (second (aref estado 1 0)))
		(o3 (third  (aref estado 1 0)))
		(TO (+ o1 (* o2 5) (* o3 10))))
		(format t "~%*************************************************************************************~%")
		(format t " (1 5 10) valor de las fichas ~%~%")
		(format t "     ~A                                                                          ~A ~%" TM TO)
		(format t "   Maquina  ~A ** ~A ** ~A ** ~A ** ~A ** ~A   Oponente~%" hm1 hm2 hm3 hm4 hm5 hm6)
		(format t "   (~A ~A ~A)  ~A ** ~A ** ~A ** ~A ** ~A ** ~A   (~A ~A ~A)~%" m1 m2 m3 ho1 ho2 ho3 ho4 ho5 ho6 o1 o2 o3)
		(format t "               1    **    2    **    3    **    4    **    5    **    6~%~%")
		(format t "~%~%***********************************************************************************~%~%")))

;Coloca una ficha en el estado segun su posicion
(defun Coloca-Ficha (estado pos ficha)
	(let ((aux (make-array '(2 7) :initial-element 0)))
		(dotimes (x 2)
	   		(dotimes (y 7) 
	    		(setf (aref aux x y) (aref estado x y))))	 
		(cond 
			;;Casos cuando inserto en la base
			((and (= *turno* 1) (= pos 0) (= ficha 1)) (setf (aref aux 0 pos) (list (1+ (first (aref aux 0 pos))) (second (aref aux 0 pos)) (third (aref aux 0 pos)))))
			((and (= *turno* 1) (= pos 0) (= ficha 5)) (setf (aref aux 0 pos) (list (first (aref aux 0 pos)) (1+ (second (aref aux 0 pos))) (third (aref aux 0 pos)))))
			((and (= *turno* 1) (= pos 0) (= ficha 10))(setf (aref aux 0 pos) (list (first (aref aux 0 pos)) (second (aref aux 0 pos)) (1+ (third (aref aux 0 pos))))))

			((and (= *turno* 0) (= pos 7) (= ficha 1)) (setf (aref aux 1 0) (list (1+ (first (aref aux 1 0))) (second (aref aux 1 0)) (third (aref aux 1 0)))))
			((and (= *turno* 0) (= pos 7) (= ficha 5)) (setf (aref aux 1 0) (list (first (aref aux 1 0)) (1+ (second (aref aux 1 0))) (third (aref aux 1 0)))))
			((and (= *turno* 0) (= pos 7) (= ficha 10))(setf (aref aux 1 0) (list (first (aref aux 1 0)) (second (aref aux 1 0)) (1+  (third (aref aux 1 0)))))))

		(when (= *turno* 1)
			;;Caso cuando doy una vuelta al tablero
			(if (<= pos -7) (setq pos (+ pos 13)))
			(cond
				;;Casos cuando reparto en oponente
				((= pos 0) aux)
				((and (< pos 0) (= ficha 1))  (setq pos (abs pos)) (setf (aref aux 1 pos) 
					(list  (1+ (first (aref aux 1 pos))) (second (aref aux 1 pos)) (third (aref aux 1 pos)))))
				((and (< pos 0) (= ficha 5))  (setq pos (abs pos)) (setf (aref aux 1 pos) 
					(list  (first (aref aux 1 pos)) (1+ (second (aref aux 1 pos))) (third (aref aux 1 pos)))))
				((and (< pos 0) (= ficha 10)) (setq pos (abs pos)) (setf (aref aux 1 pos) 
					(list  (first (aref aux 1 pos)) (second (aref aux 1 pos)) (1+ (third (aref aux 1 pos))))))
				;;Casos cuando reparto en maquina
				((= ficha 1) (setf (aref aux 0 pos) 
					(list  (1+ (first (aref aux 0 pos))) (second (aref aux 0 pos)) (third (aref aux 0 pos)))))
				((= ficha 5) (setf (aref aux 0 pos) 
					(list  (first (aref aux 0 pos)) (1+ (second (aref aux 0 pos))) (third (aref aux 0 pos)))))
				((= ficha 10) (setf (aref aux 0 pos) 
					(list  (first (aref aux 0 pos)) (second (aref aux 0 pos)) (1+ (third (aref aux 0 pos)))))))) 

		(when (= *turno* 0)
			;;Caso cuando doy una vuelta al tablero
			(if (> pos 12) (setq pos (- pos 12)))
			(cond
				;;Casos cuando reparto en maquina
				((= pos 7) aux)
				((and (> pos 7) (= ficha 1)) (setq pos (- 14 pos)) (setf (aref aux 0 pos) 
					(list  (1+ (first (aref aux 0 pos))) (second (aref aux 0 pos)) (third (aref aux 0 pos)))))
				((and (> pos 7) (= ficha 5)) (setq pos (- 14 pos)) (setf (aref aux 0 pos) 
					(list  (first (aref aux 0 pos)) (1+ (second (aref aux 0 pos))) (third (aref aux 0 pos)))))
				((and (> pos 7) (= ficha 10)) (setq pos (- 14 pos)) (setf (aref aux 0 pos) 
					(list  (first (aref aux 0 pos)) (second (aref aux 0 pos)) (1+ (third (aref aux 0 pos))))))
				;;Casos cuando reparto en oponente
				((= ficha 1) (setf (aref aux 1 pos) 
					(list  (1+ (first (aref aux 1 pos))) (second (aref aux 1 pos)) (third (aref aux 1 pos)))))
				((= ficha 5) (setf (aref aux 1 pos) 
					(list  (first (aref aux 1 pos)) (1+ (second (aref aux 1 pos))) (third (aref aux 1 pos)))))
				((= ficha 10) (setf (aref aux 1 pos) 
					(list  (first (aref aux 1 pos)) (second (aref aux 1 pos)) (1+ (third (aref aux 1 pos))))))))
		aux))

;Aplica un movimiento a un estado
(defun apply-mov (estado casilla orden) 
	(let ((fichastotales 0)
		(fichas 0)
		(fichas5 0)
		(fichas10 0)
		(pos casilla)
		(aux (make-array '(2 7) :initial-element 0)))

		(dotimes (x 2)
	   		(dotimes (y 7) 
	    		(setf (aref aux x y) (aref estado x y))))

		(when (= *turno* 1) 

			(setq fichas   (first  (aref aux 0 pos)))
			(setq fichas5  (second (aref aux 0 pos)))
			(setq fichas10 (third  (aref aux 0 pos)))

			(setq fichastotales (+ fichas fichas5 fichas10)))

		(when (= *turno* 0)

			(setq fichas   (first  (aref aux 1 pos)))
			(setq fichas5  (second (aref aux 1 pos)))
			(setq fichas10 (third  (aref aux 1 pos)))

			(setq fichastotales (+ fichas fichas5 fichas10)))

		(when (and (= *turno* 1) (> fichastotales 0))
			(setf (aref aux 0 pos) (list 0 0 0))
			(dotimes (i fichastotales)
				(setq aux (Coloca-Ficha aux (- pos (1+ i)) (nth i orden)))))

		(when (and (= *turno* 0) (> fichastotales 0))
			(setf (aref aux 1 pos) (list 0 0 0))
			
			(dotimes (i fichastotales)
				(setq aux (Coloca-Ficha aux (+ pos (1+ i)) (nth i orden)))))
		aux))

;Vacia todas la fichas que resten del juego la base correspondiente
(defun Vaciar-Final (estado)
	(let* ((aux (make-array '(2 7) :initial-element 0))
		(m1 (first  (aref estado 0 0)))
		(m2 (second (aref estado 0 0)))
		(m3 (third  (aref estado 0 0)))

		(o1 (first  (aref estado 1 0)))
		(o2 (second (aref estado 1 0)))
		(o3 (third  (aref estado 1 0))))

		(dotimes (x 2)
	   		(dotimes (y 7) 
	    		(setf (aref aux x y) (list 0 0 0))))

		(when (= *turno* 0)
			(setq o1 (+ o1 (- 12 (+ o1 m1))))
			(setq o2 (+ o2 (- 12 (+ o2 m2))))
			(setq o3 (+ o3 (- 12 (+ o3 m3))))
			(setf (aref aux 0 0) (list m1 m2 m3))
			(setf (aref aux 1 0) (list o1 o2 o3)))

		(when (= *turno* 1)
			(setq m1 (+ m1 (- 12 (+ o1 m1))))
			(setq m2 (+ m2 (- 12 (+ o2 m2))))
			(setq m3 (+ m3 (- 12 (+ o3 m3))))
			(setf (aref aux 0 0) (list m1 m2 m3))
			(setf (aref aux 1 0) (list o1 o2 o3)))aux))

;Predicado donde indica si el estado es final del juego
(defun Final-Juego? (estado)
	(let ((aux T))
		(labels ((fin (e i j b)
			(cond 	
				((or (null e) (> j 6)) b)
				((not (zerop (first  (aref e i j)))) (setq b NIL))
				((not (zerop (second (aref e i j)))) (setq b NIL))
				((not (zerop (third  (aref e i j)))) (setq b NIL))
				(T (fin e i (1+ j) b)))))
		(or (fin estado 0 1 aux) (fin estado 1 1 aux)))))

;Predicado que te dice si esta vacia esa casilla
(defun vacia-casilla-maquina (estado casilla)
	(and (= (first  (aref estado 0 casilla)) 0) 
		 (= (second (aref estado 0 casilla)) 0) 
		 (= (third  (aref estado 0 casilla)) 0)))

;Predicado que te dice si esta vacia esa casilla
(defun vacia-casilla-oponenete (estado casilla)
	(and (= (first  (aref estado 1 casilla)) 0) 
		 (= (second (aref estado 1 casilla)) 0) 
		 (= (third  (aref estado 1 casilla)) 0)))

;Evalua el estado con la heuristica:
(defun Evalua-Estado (estado)
	(let* ((fichastotales 0)

		(m1 (first  (aref estado 0 0)))
		(m2 (second (aref estado 0 0)))
		(m3 (third  (aref estado 0 0)))
		(TM (+ m1 (* m2 5) (* m3 10)))

		(o1 (first  (aref estado 1 0)))
		(o2 (second (aref estado 1 0)))
		(o3 (third  (aref estado 1 0)))
		(TO (+ o1 (* o2 5) (* o3 10)))) 

		(dotimes (i 2)
			(dotimes (j 6)
				(setq fichastotales (+ (first  (aref estado i (1+ j)))
										(* 5  (second (aref estado i (1+ j))))
										(* 10 (third  (aref estado i (1+ j))))))))

		(cond ((and (vacia-casilla-maquina estado 1) 
					(vacia-casilla-maquina estado 2) 
					(vacia-casilla-maquina estado 3) 
					(vacia-casilla-maquina estado 4)
		 			(vacia-casilla-maquina estado 5) 
		 			(vacia-casilla-maquina estado 6))  (setq TM (+ TM fichastotales)))

			  ((and (vacia-casilla-oponenete estado 1) 
			  		(vacia-casilla-oponenete estado 1) 
			  		(vacia-casilla-oponenete estado 1) 
			  		(vacia-casilla-oponenete estado 1)
		 			(vacia-casilla-oponenete estado 1) 
		 			(vacia-casilla-oponenete estado 1)) (setq TO (+ TO fichastotales))))

		(if (= *turno* 0) (- TM TO) (- TO TM))))

;Regresa una lista con las fichas en orden en que vas a ser insertadas
(defun Lista-Fichas (repeticiones ficha)
	(let ((lista NIL))
		(dotimes (i repeticiones lista)
			(setq lista (cons ficha lista)))))

;Heuristica que regresa un orden para mover las fichas
(defun heuristica-orden (estado casilla)
	(let ((listafichas NIL)
		  (fichastotales 0)
		  (fichas   0)
		  (fichas5  0)
		  (fichas10 0))

		(when (= *turno* 1)
			(setq fichas   (first  (aref estado 0 casilla)))
			(setq fichas5  (second (aref estado 0 casilla)))
			(setq fichas10 (third  (aref estado 0 casilla))))

		(when (= *turno* 0)
			(setq fichas   (first  (aref estado 1 casilla)))
			(setq fichas5  (second (aref estado 1 casilla)))
			(setq fichas10 (third  (aref estado 1 casilla))))

		(setq fichastotales (+ fichas fichas5 fichas10))
		
		(cond 
			((= fichastotales casilla) (setq listafichas (append (Lista-Fichas fichas 1) (Lista-Fichas fichas5 5) (Lista-Fichas fichas10 10))))
			((> fichastotales casilla) (setq listafichas (append (Lista-Fichas fichas10 10) (Lista-Fichas fichas5 5) (Lista-Fichas fichas 1))))
			(T (setq listafichas (append (Lista-Fichas fichas5 5) (Lista-Fichas fichas10 10) (Lista-Fichas fichas 1))))) 
		listafichas))

;Verifica que se pueda realizar un movimiento
(defun mov-valid? (estado casilla)
	(let ((fichas 0)
		(fichas5  0)
		(fichas10 0))
		(when (= *turno* 1)
			(setq fichas   (first  (aref estado 0 casilla)))
			(setq fichas5  (second (aref estado 0 casilla)))
			(setq fichas10 (third  (aref estado 0 casilla))))
		(when (= *turno* 0)
			(setq fichas   (first  (aref estado 1 casilla)))
			(setq fichas5  (second (aref estado 1 casilla)))
			(setq fichas10 (third  (aref estado 1 casilla))))
		(if (and (zerop fichas) (zerop fichas5) (zerop fichas10)) NIL T)))

;Regresa valor de verdad para cambiar de turno
(defun Volver-Tirar (estado nuevo)
	(let* ((B T)
		(me1 (first  (aref estado 0 0)))	;;Estado anterior
		(me2 (second (aref estado 0 0)))
		(me3 (third  (aref estado 0 0)))
		(BMe (+ me1 (* me2 5) (* me3 10)))
		(oe1 (first  (aref estado 1 0)))
		(oe2 (second (aref estado 1 0)))
		(oe3 (third  (aref estado 1 0)))
		(BOe (+ oe1 (* oe2 5) (* oe3 10)))
		(fichasA    (first  (aref estado 0 6)))
		(fichasA5   (second (aref estado 0 6)))
		(fichasA10  (third  (aref estado 0 6)))
		(fichasAA   (first  (aref estado 1 1)))
		(fichasAA5  (second (aref estado 1 1)))
		(fichasAA10 (third  (aref estado 1 1)))
		
		(mn1 (first  (aref nuevo 0 0)))		;;Estado nuevo
		(mn2 (second (aref nuevo 0 0)))
		(mn3 (third  (aref nuevo 0 0)))
		(BMn (+ mn1 (* mn2 5) (* mn3 10)))
		(on1 (first  (aref nuevo 1 0)))
		(on2 (second (aref nuevo 1 0)))
		(on3 (third  (aref nuevo 1 0)))
		(BOn (+ on1 (* on2 5) (* on3 10)))
		(fichasB    (first  (aref nuevo 0 6)))
		(fichasB5   (second (aref nuevo 0 6)))
		(fichasB10  (third  (aref nuevo 0 6)))
		(fichasBB   (first  (aref nuevo 1 1)))
		(fichasBB5  (second (aref nuevo 1 1)))
		(fichasBB10 (third  (aref nuevo 1 1))))

		(cond
			((and (= *turno* 1) (not (= BMe BMn)) (= fichasAA fichasBB) (= fichasAA5 fichasBB5) (= fichasAA10 fichasBB10)) NIL)
			((and (= *turno* 0) (not (= BOe BOn)) (= fichasA fichasB) (= fichasA5 fichasB5) (= fichasA10 fichasB10)) NIL)
			(T B))))

;Pide el siguiente movimiento al oponente
(defun Pedir-Movimiento (estado)
	(let ((casilla 0)
		(orden NIL))
	(format t "Introduce la casilla de tu siguiente movimiento: ")
	(setq casilla (read))
	(format t "Introduce el orden de las fichas en forma de lista: ejemplo (10 5 1)~%")
	(setq orden (read))
	(when (not (mov-valid? estado casilla)) (format t "~%~%No es posible realizar ese movimiento~%~%")(Pedir-Movimiento estado))
	(apply-mov estado casilla orden)))

;Resetea las variables globales
(defun reset-all ()
	(setq *depth* 6)
	(setq *turno* 0))

;NegaMax toma la mejor desicion
(defun NegaMAX-alfabeta (estado profundidad maxProf alfa beta)
	(cond ((or (Final-Juego? estado) (= profundidad maxProf)) (list -1 (Evalua-Estado estado)))  
		(T (let ((valor 0)(nuevoE nil) (MejorMov 0) (MejorVal -99999))
			(dotimes (i 6)
				(if (= (mod profundidad 2) 0) (setq *turno* 1) (setq *turno* 0))
				(when (mov-valid? estado (1+ i))
					(setq nuevoE (apply-mov estado (1+ i) (heuristica-orden estado (1+ i))))
					(cond ((and (not (Volver-Tirar estado nuevoE)) (= *turno* 1)) 
						   (setq valor (NegaMAX-alfabeta nuevoE profundidad maxProf alfa beta))) 
						(T (setq valor (NegaMAX-alfabeta nuevoE (1+ profundidad) maxProf (* -1 beta) (* -1 (max alfa MejorVal))))
						   (setf (second valor) (* -1 (second valor)))))
					(when (> (second valor) MejorVal)
						(setq MejorVal (second valor) MejorMov (1+ i))
						(if (>= MejorVal beta) (return-from NegaMAX-alfabeta (list MejorMov MejorVal))))))
			(list MejorMov MejorVal)))))

;Funcion para empezar a jugar
(defun Game-Play()
	(let ((B NIL)
		(estado *estado-inicial*)
		(nuevo NIL)
		(tirarO NIL)
		(tirarM NIL)
		(casilla 0))
		(reset-all)
		(Display-Mancala estado)
		(loop until B do
			(loop until (or B tirarO) do
				(setq *turno* 0)
				(setq nuevo (Pedir-Movimiento estado))
				(when (Final-Juego? nuevo) (setq nuevo (Vaciar-Final nuevo)) (setq B T)(setq tirarO T)(setq tirarM T))
				(Display-Mancala nuevo)
				(when (Volver-Tirar estado nuevo) (setq *turno* 1)(setq tirarO T)(setq tirarM NIL))
				(setq estado nuevo))
			(loop until (or B tirarM) do
				(setq casilla (first (NegaMAX-alfabeta estado 0 *depth* -99999 99999)))
				(setq *turno* 1)
				(setq nuevo (apply-mov nuevo casilla (heuristica-orden nuevo casilla)))
				(when (Final-Juego? nuevo) (setq nuevo (Vaciar-Final nuevo)) (setq B T)(setq tirarO T)(setq tirarM T))
				(format t "~%~%La maquina tiro la casilla ~A en orden ~A~%~%" casilla (heuristica-orden estado casilla))
				(Display-Mancala nuevo)
				(when (Volver-Tirar estado nuevo) (setq *turno* 0)(setq tirarO NIL)(setq tirarM T))
				(setq estado nuevo)))))

(Game-Play)