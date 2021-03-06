;Morales Torres Jonatan
;Mancala

(defparameter  *finalizado* 'nil)
(defparameter  *ops*  nil)
(defparameter *MAXPROF* 6)

(defparameter *scoreIA* 0)
(defparameter *scoreHM* 0)
(defparameter *tableroIA* nil)  
(defparameter *tableroHM* nil)  
(defparameter *doublemove* nil)
(defparameter *EXTRA* nil)
(defparameter *minusInf* -99999999999999)
(defparameter *Inf* 99999999999999)
;;=======================================================================
;; 		PRINT-TABLERO
;;=======================================================================

(defun print-tablero ()
	"Imprime el tablero de juego"
	(format t "---------------------------------------------------------------------------~%") 
	(format t "---------------------------------------------------------------------------~%")
	(format t "     ~A     ~A    ~A~%" *scoreIA* *tableroIA* *scoreHM*)
	(format t "----IA---- ~A --Jugador--~%" *tableroHM* )
	(format t "---------------------------------------------------------------------------~%")	
	(format t "---------------------------------------------------------------------------~%~%"))

;;=======================================================================
;; 		TOTAL, 		VACIAP,		FLIP
;;=======================================================================
(defun total (cuenca)
	"Regresa el numero total de fichas en una cuenca"
	(+ (first cuenca) (second cuenca) (third cuenca)))

(defun vaciap (cuenca)
	"Predicado: dice si una cuenca no tiene fichas por repartir"
	(and (= (first cuenca) 0) (= (second cuenca) 0) (= 0 (third cuenca))))

(defun flip (bit)  (boole  BOOLE-XOR  bit  1))
;;=======================================================================
;; 		APLICAR-JUGADA
;;=======================================================================

(defun aplicar-jugada (jugada jugador casilla)
	"Dada un jugada, un jugador y la casilla de la cual se parte, modifica los valores del tablero"
	(let ( (nivel nil) (temporal nil) ) 
		(if (eql jugador :HM) (setf (nth (1- casilla) *tableroHM*) '(0 0 0)) (setf (nth (1+ casilla) *tableroIA*) '(0 0 0)))
		(if (eql jugador :HM) (setq nivel 1) (setq nivel 0))
		(loop for i from 0 to (1- (length jugada)) do
			;(print *tableroHM*)
			;(print *tableroIA*)
			(cond ((and (eql jugador :HM) (= nivel 1) (= casilla 6)) 
					(setq *scoreHM* (+ *scoreHM* (nth i jugada)) nivel 0 casilla 5))   ;;sumar
				  ((and (eql jugador :HM) (= nivel 0) (= casilla -1)) 
				  	(setq nivel 1 casilla 0))  ;;renglon de arriba
				  ((and (eql jugador :IA) (= nivel 1) (= casilla 6)) 
				    (setq nivel 0 casilla 5))       ;;renglo de abajo
				  ((and (eql jugador :IA) (= nivel 0) (= casilla -1)) 
				  	(setq *scoreIA* (+ *scoreIA* (nth i jugada)) nivel 1 casilla 0))  ;;sumar 
				  (t 
				  	(cond  ((= nivel 1) 
				  			(setq temporal (nth casilla *tableroHM*))
				  			(case (nth i jugada)
				  				(1 (setf (nth casilla *tableroHM*) (list (1+ (first temporal)) (second temporal) (third temporal))))
				  				(5 (setf (nth casilla *tableroHM*) (list (first temporal) (1+ (second temporal)) (third temporal))))
				  				(10 (setf (nth casilla *tableroHM*) (list (first temporal) (second temporal) (1+ (third temporal))))))
				  			(incf casilla)) 
				  		(t (setq temporal (nth casilla *tableroIA*))
				  			(case (nth i jugada)
				  				(1 (setf (nth casilla *tableroIA*) (list (1+ (first temporal)) (second temporal) (third temporal))))
				  				(5 (setf (nth casilla *tableroIA*) (list (first temporal) (1+ (second temporal)) (third temporal))))
				  				(10 (setf (nth casilla *tableroIA*) (list (first temporal) (second temporal) (1+ (third temporal))))))
				  			(decf casilla))))))
		(if  (or (and (not (check-final)) (eql jugador :HM) (= casilla 5) (= nivel 0)) 
			(and (not (check-final)) (eql jugador :IA) (= casilla 0) (= nivel 1))) (setq *doublemove* t))
	))

;;=======================================================================
;; 		TURNO-JUGADOR, 		LEER-REPARTO
;;=======================================================================

(defun leer-reparto (cuenca)
	"Lee y valida la forma de repartir las fichas del jugador"
	(let ((fichas (total cuenca)) (validador nil) (salida nil) (nCuenca '(0 0 0)))
		(loop until validador do
			(loop for i from 1 to fichas do 
				(push (read) salida)
				(cond ((= (first salida) 1) (setq nCuenca (list (1+ (first nCuenca)) (second nCuenca) (third nCuenca))))
					((= (first salida) 5) (setq nCuenca (list (first nCuenca) (1+ (second nCuenca)) (third nCuenca))))
					((= (first salida) 10) (setq nCuenca (list (first nCuenca) (second nCuenca) (1+ (third nCuenca)))))))
			(cond ((equalp nCuenca cuenca) (setq validador t)) 
				(t (format t "Opcion no valida, ingrese una lista valida~%") 
					(setq salida nil nCuenca '(0 0 0)))))
		(reverse salida)))

(defun turno-jugador () 
	"Control de la jugada del jugador humano"
	(setq *doublemove* nil)
	(let ((aux nil) (jugada nil) (validador nil) (cuenca nil))
		(loop until validador do 
			(format t "Ingrese el numero de la casilla que quiere repartir (index desde 1)~%")
			(setq aux (read))
			(if (and (integerp aux) (>= aux 1) (<= aux 6) (not (vaciap (nth (1- aux) *tableroHM*)))) (setq validador t) (format t "Opcion invalida~%")))
		(setq cuenca (nth (1- aux) *tableroHM*))
		(format t "Cuenca seleccionada con ~A de 1, ~A de 5 y ~A de 10~%" (first cuenca) (second cuenca) (third cuenca))
		(format t "Escriba las fichas (valor) en el orden que se repartira~%")
		(setq jugada (leer-reparto cuenca))
		(aplicar-jugada jugada :HM aux) 
		(cond ( *doublemove* (format t "~%---------Jugada extra---------~%") (print-tablero) (turno-jugador)))))

;;=======================================================================
;; 		NEGAMAXAB,	EVALUACION,		REPATIR
;;=======================================================================

;;FORMA DEL TABLERO   (<tableroIA> <tableroHM> <scoreIA> <scoreHM>)

(defun repartir (casilla tablero jugador) 
	"Dada una posicion del tablero, elige la mejor forma de repartir las fichas en las siguientes casillas, 
	regresa una lista con las fichas a repartir"
	(let* ( (cuenca (nth casilla (nth jugador tablero))) (fichas (total cuenca)) 
		(aux casilla) (nivel 0) (anotadas 0) (entregadas 0) (movidas 0)
		(lent nil) (lano nil) (lfichas nil) (reparto nil))
		(setq nivel jugador)
		;;avanzar a la siguiente casilla
		(cond ((= nivel 1) (incf aux))
			  (t (decf aux)))
		;;determinar cuantas se van a dar a cada hueco
		
		(loop for i from 1 to fichas do
			(cond ((and (= jugador 1) (= nivel 1) (= aux 6))
					(setq nivel 0 aux 5) 
					(incf anotadas))   ;;sumar
				  ((and (= jugador 1) (= nivel 0) (= aux -1)) 
				  	(setq nivel 1 aux 0))  ;;renglon de arriba
				  ((and (= jugador 0) (= nivel 1) (= aux 6)) 
				    (setq nivel 0 aux 5))       ;;renglo de abajo
				  ((and (= jugador 0) (= nivel 0) (= aux -1)) 
				  	(setq nivel 1 aux 0) 
				  	(incf anotadas))  ;;sumar 
				  (t 
				  	(cond ((and (= nivel 1) (= jugador 1)) (incf movidas) (incf aux)) 
				  		((and (= nivel 1) (= jugador 0)) (incf entregadas) (incf aux)) 
				  		((and (= nivel 0) (= jugador 1)) (incf entregadas) (decf aux)) 
				  		((and (= nivel 0) (= jugador 0)) (incf movidas) (decf aux)) 
				  		))))
		(loop for i from 1 to (first cuenca) do (push 1 lfichas))
		(loop for i from 1 to (second cuenca) do (push 5 lfichas))
		(loop for i from 1 to (third cuenca) do (push 10 lfichas))
		(loop for i from 1 to anotadas do (push (pop lfichas) lano))
		(setq lfichas (reverse lfichas))
		(loop for i from 1 to entregadas do (push (pop lfichas) lent))
		(setq lfichas (reverse lfichas))
		(setq nivel jugador aux casilla)
		(cond ((= nivel 1) (incf aux))
			  (t (decf aux)))
		(loop for i from 1 to fichas do
			(cond ((and (= jugador 1) (= nivel 1) (= aux 6))
					(setq nivel 0 aux 5) 
					(push (pop lano) reparto))   ;;sumar
				  ((and (= jugador 1) (= nivel 0) (= aux -1)) 
				  	(setq nivel 1 aux 0))  ;;renglon de arriba
				  ((and (= jugador 0) (= nivel 1) (= aux 6)) 
				    (setq nivel 0 aux 5))       ;;renglo de abajo
				  ((and (= jugador 0) (= nivel 0) (= aux -1)) 
				  	(setq nivel 1 aux 0) 
				  	(push (pop lano) reparto))  ;;sumar 
				  (t 
				  	(cond ((and (= nivel 1) (= jugador 1)) (push (pop lfichas) reparto) (incf aux)) 
				  		((and (= nivel 1) (= jugador 0)) (push (pop lent) reparto) (incf aux)) 
				  		((and (= nivel 0) (= jugador 1)) (push (pop lent) reparto) (decf aux)) 
				  		((and (= nivel 0) (= jugador 0)) (push (pop lfichas) reparto) (decf aux)) 
				  		))))
		(reverse reparto)
	))

(defun evaluacion (tablero jugador)
	"Funcion que regresa la evaluacion de un tablero"
	;;implementar que verifique si es final y sumar la cantidad a capturar
	(let ((acum 0) (tabIA (first tablero)) (tabHM (second tablero)) (scoIA (third tablero)) (scoHM (fourth tablero))) 
		(loop for i from 0 to 5 do
			(setq acum (+ acum (first (nth i tabIA)) (* 5 (second (nth i tabIA))) (* 10 (third (nth i tabIA)))))
			(setq acum (+ acum (first (nth i tabHM)) (* 5 (second (nth i tabHM))) (* 10 (third (nth i tabHM))))))
		(cond ((and (vaciap (nth 0 tabIA)) (vaciap (nth 1 tabIA)) (vaciap (nth 2 tabIA)) (vaciap (nth 3 tabIA))
		 		(vaciap (nth 4 tabIA)) (vaciap (nth 5 tabIA))) (setq scoIA (+ scoIA acum)))
			  ((and (vaciap (nth 0 tabHM)) (vaciap (nth 1 tabHM)) (vaciap (nth 2 tabHM)) (vaciap (nth 3 tabHM))
		 		(vaciap (nth 4 tabHM)) (vaciap (nth 5 tabHM))) (setq scoHM (+ scoHM acum))))
		(if (= jugador 0) (- scoIA scoHM) (- scoHM scoIA))))

(defun check-final-WA ( tableroIA tableroHM)
	"Revisa si tras las jugadas se ha alcanzado un estado final"
	(or (and (vaciap (nth 0 tableroIA))
		 (vaciap (nth 1 tableroIA))
		 (vaciap (nth 2 tableroIA))
		 (vaciap (nth 3 tableroIA))
		 (vaciap (nth 4 tableroIA))
		 (vaciap (nth 5 tableroIA)))
	(and (vaciap (nth 0 tableroHM))
		 (vaciap (nth 1 tableroHM))
		 (vaciap (nth 2 tableroHM))
		 (vaciap (nth 3 tableroHM))
		 (vaciap (nth 4 tableroHM))
		 (vaciap (nth 5 tableroHM)))))

(defun genTablero (tablero casilla jugador)
	"Genera un nuevo tablero repartiendo una casilla"
	;;jugador 0 - IA   jugador 
	(let ((jugada (repartir casilla tablero jugador)) (nivel nil) (temporal nil)
		(tableroIA (copy-list (first tablero))) (tableroHM (copy-list (second tablero))) (scoreIA (third tablero)) (scoreHM (fourth tablero)))
		(if (= jugador 1) (setf (nth casilla tableroHM) '(0 0 0)) (setf (nth casilla tableroIA) '(0 0 0)))
		(if (= jugador 1) (setq nivel 1) (setq nivel 0))
		(cond ((= nivel 1) (incf casilla))
			  (t (decf casilla)))
		(loop for i from 0 to (1- (length jugada)) do
			(cond ((and (= jugador 1) (= nivel 1) (= casilla 6)) 
					(setq scoreHM (+ scoreHM (nth i jugada)) nivel 0 casilla 5))   ;;sumar
				  ((and (= jugador 1) (= nivel 0) (= casilla -1)) 
				  	(setq nivel 1 casilla 0))  ;;renglon de arriba
				  ((and (= jugador 0) (= nivel 1) (= casilla 6)) 
				    (setq nivel 0 casilla 5))       ;;renglo de abajo
				  ((and (= jugador 0) (= nivel 0) (= casilla -1)) 
				  	(setq scoreIA (+ scoreIA (nth i jugada)) nivel 1 casilla 0))  ;;sumar 
				  (t 
				  	(cond  ((= nivel 1) 
				  			(setq temporal (nth casilla tableroHM))
				  			(case (nth i jugada)
				  				(1 (setf (nth casilla tableroHM) (list (1+ (first temporal)) (second temporal) (third temporal))))
				  				(5 (setf (nth casilla tableroHM) (list (first temporal) (1+ (second temporal)) (third temporal))))
				  				(10 (setf (nth casilla tableroHM) (list (first temporal) (second temporal) (1+ (third temporal))))))
				  			(incf casilla)) 
				  		(t (setq temporal (nth casilla tableroIA))
				  			(case (nth i jugada)
				  				(1 (setf (nth casilla tableroIA) (list (1+ (first temporal)) (second temporal) (third temporal))))
				  				(5 (setf (nth casilla tableroIA) (list (first temporal) (1+ (second temporal)) (third temporal))))
				  				(10 (setf (nth casilla tableroIA) (list (first temporal) (second temporal) (1+ (third temporal))))))
				  			(decf casilla))))))
		(if  (or (and (not (check-final-WA tableroIA tableroHM)) (= jugador 1) (= casilla 5) (= nivel 0)) 
			(and (not (check-final-WA tableroIA tableroHM)) (= jugador 0) (= casilla 0) (= nivel 1))) (setq *EXTRA* t))
		(list tableroIA tableroHM scoreIA scoreHM)))

(defun NegaMAXab (tablero profundidad maxProfundidad alfa beta jugador)
	"Funcion de busqueda de algoritmo Nega MAx que permite encontrar la mejor jugada posible para el acual movimiento, Regresa en formato (<Casilla> <Valor>)"
	(cond ((or (check-final-WA (first tablero) (second tablero)) (= profundidad maxProfundidad)) (list -1 (evaluacion tablero jugador))) ;;se alcanzo el limite
		(t (let ((valor 0)(nuevoTablero nil) (MejorMov nil) (MejorVal *minusInf*))
			;;(format t" PADRE tablero ~A prof:~A jugador:~A~%" tablero profundidad jugador)
			(loop for i from 0 to 5 do
				(when (not (vaciap (nth i (nth jugador tablero))))
					(setq *EXTRA* nil) 
					(setq nuevoTablero (genTablero tablero i jugador))
					;;(format t "Hijo ~A prof:~A jugador~A~%" nuevoTablero profundidad jugador)
					(cond (*EXTRA* (setq valor (NegaMAXab nuevoTablero profundidad maxProfundidad alfa beta jugador))) 
						(t (setq valor (NegaMAXab nuevoTablero (1+ profundidad) maxProfundidad (* -1 beta) (* -1 (max alfa MejorVal)) (flip jugador)))
							(setf (second valor) (* -1 (second valor)))))
					;(format t "valor ~A prof:~A jugador~A~%" valor profundidad jugador)
					(when (> (second valor) MejorVal)
						(setq MejorVal (second valor) MejorMov i)
						(if (>= MejorVal beta) (return-from NegaMAXab (list MejorMov MejorVal))))))
			(list MejorMov MejorVal)))))


;;=======================================================================
;; 		TURNO-IA
;;=======================================================================

(defun turno-IA ()
	"Control de la jugada de la IA"
	(setq *doublemove* nil)
	(let ((jugada nil) (casilla nil) (copia (copy-list (list *tableroIA* *tableroHM* *scoreIA* *scoreHM*))))
		(setq casilla (NegaMAXab copia 0 *MAXPROF* *minusInf* *Inf* 0))
		(setq jugada (repartir (first casilla) (list *tableroIA* *tableroHM* *scoreIA* *scoreHM*) 0))
		(format t "Jugando ~A en orden ~A~%" (1+ (first casilla)) jugada)
		(aplicar-jugada jugada :IA (1- (first casilla)))
		(cond ( *doublemove* (format t "~%---------Jugada extra---------~%") (print-tablero) (turno-IA)))))

;;=======================================================================
;; 		CHECK-FINAL, 	ACUMULAR
;;=======================================================================

(defun acumular ()
	"Acumula los valores restantes de cada jugador y los suma a su score"
	(let ((acum 0))
		(loop for i from 0 to 5 do
			(setq acum (+ acum (first (nth i *tableroIA*)) (* 5 (second (nth i *tableroIA*))) (* 10 (third (nth i *tableroIA*)))))
			(setq acum (+ acum (first (nth i *tableroHM*)) (* 5 (second (nth i *tableroHM*))) (* 10 (third (nth i *tableroHM*)))))
		)
		(if (and (vaciap (nth 0 *tableroIA*)) (vaciap (nth 1 *tableroIA*)) (vaciap (nth 2 *tableroIA*)) (vaciap (nth 3 *tableroIA*))
		 (vaciap (nth 4 *tableroIA*)) (vaciap (nth 5 *tableroIA*))) (setq *scoreIA* (+ *scoreIA* acum)) (setq *scoreHM* (+ *scoreHM* acum)))
		(loop for i from 0 to 5 do
			(setf (nth i *tableroHM*) '(0 0 0))
			(setf (nth i *tableroIA*) '(0 0 0)))))

(defun check-final ()
	"Revisa si tras las jugadas se ha alcanzado un estado final"
	(or (and (vaciap (nth 0 *tableroIA*))
		 (vaciap (nth 1 *tableroIA*))
		 (vaciap (nth 2 *tableroIA*))
		 (vaciap (nth 3 *tableroIA*))
		 (vaciap (nth 4 *tableroIA*))
		 (vaciap (nth 5 *tableroIA*)))
	(and (vaciap (nth 0 *tableroHM*))
		 (vaciap (nth 1 *tableroHM*))
		 (vaciap (nth 2 *tableroHM*))
		 (vaciap (nth 3 *tableroHM*))
		 (vaciap (nth 4 *tableroHM*))
		 (vaciap (nth 5 *tableroHM*)))))


;;=======================================================================
;; 		START-GAME, 	RESET-ALL
;;=======================================================================

(defun reset-all () 
"Reinicia todas las variables globales para iniciar una nuevo juego..."
    (setq  *finalizado*  nil)
    (setq *MAXPROF* 6)
    (setq *scoreIA* 0)
	(setq *scoreHM* 0)
	(setq *tableroIA* nil)
	(push '(1 1 1) *tableroIA*)
	(push '(1 1 1) *tableroIA*)
	(push '(1 1 1) *tableroIA*)
	(push '(1 1 1) *tableroIA*)
	(push '(1 1 1) *tableroIA*)
	(push '(1 1 1) *tableroIA*)
	(setq *tableroHM*  nil) 
	(push '(1 1 1) *tableroHM*)
	(push '(1 1 1) *tableroHM*)
	(push '(1 1 1) *tableroHM*)
	(push '(1 1 1) *tableroHM*)
	(push '(1 1 1) *tableroHM*)
	(push '(1 1 1) *tableroHM*))

(defun start-game ()
	"Incia un nuevo juego y lleva el control de los turnos en el juego"
	(reset-all)
	(format t "---INICIA EL JUEGO---~%Jugador inicia primero~%~%")
	(print-tablero)
	(loop until  *finalizado*  do
	     (turno-jugador)
	     (if (check-final) (setq *finalizado* t) (turno-IA))
	     (print-tablero)
	     (if (check-final) (setq *finalizado* t)))
	(format t "~%~%--------------------------Juego finalizado--------------------------~%~%")
	(format t "~%--------------------------Tablero final--------------------------~%")
	(acumular)
	(print-tablero)
	(if (> *scoreIA* *scoreHM*) (format t "~%~%---------------------DERROTA---------------------~%~%") 
		(format t "~%~%---------------------VICTORIA---------------------~%~%")))

;;=======================================================================
;; 		INICIO DEL PROGRAMA
;;=======================================================================


(start-game)
Contact GitHub 