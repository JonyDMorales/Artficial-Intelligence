;Morales Torres Jonatan
;8-puzzle

;Estado final		((1 2 3)
;					(8 0 4)
;					(7 6 5))

(defparameter  *open* '())		;; Frontera de busqueda...                               
(defparameter  *memory* '())	;; Memoria de intentos previos
(defparameter  *Inicio* '#2A((2 8 3)(1 4 5)(7 0 6)))	;; Estado Inical
(defparameter  *Final* '#2A((1 2 3)(8 0 4)(7 6 5)))		;; Estado Final
(defparameter *expandidos* 0)   ;;Total de nodos expandidos

(defparameter  *ops*  '( 
	(:arriba 		(1))	;; Operadores para el problema
	(:derecha		(1)) 
	(:abajo        	(1))
	(:izquierda     (1))) )

(defparameter  *id*  0)  ;; Identificador del ultimo nodo creado, cada vez que se cree un nodo se debe incrementar
(defparameter  *current-ancestor*  NIL)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  NIL)

;Crea un nuevo nodo de la forma (id  h(e) estado antecesor operador-de-creacion)
;Llama de prueba (create-node '((1 2 3)(4 0 5)(6 7 8)) '(:arriba (1)) 1)
(defun  create-node (estado  op aptitud)
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  (1-  *id*)  estado  *current-ancestor*  (first op) aptitud))  ;;los nodos generados son descendientes de *current-ancestor*

;Aptitud segun el metodo wrong
;Llamada de prueba (Aptitud-Wrong '#2A((1 2 3)(8 0 4)(7 5 6)))
(defun Aptitud-Wrong (estado)
	(let ((apt 0))
	    (dotimes (i 3 apt)
	    	(dotimes (j 3)
	    		(if (and (not (equal (aref estado i j) (aref *Final* i j))) (> (aref estado i j) 0)) 
	    			(setq apt (+ 1 apt)))))))

;Movimientos que hacen falta de una pieza para ponerlo en su lugar
(defun manhattan (numero posi posj)
	(let ((dis 0))
		(cond
			((= 1 numero) (setq dis (+ (abs (- posi 0)) (abs (- posj 0)) )))
			((= 2 numero) (setq dis (+ (abs (- posi 0)) (abs (- posj 1)) )))
			((= 3 numero) (setq dis (+ (abs (- posi 0)) (abs (- posj 2)) )))
			((= 4 numero) (setq dis (+ (abs (- posi 1)) (abs (- posj 2)) )))
			((= 5 numero) (setq dis (+ (abs (- posi 2)) (abs (- posj 2)) )))
			((= 6 numero) (setq dis (+ (abs (- posi 2)) (abs (- posj 1)) )))
			((= 7 numero) (setq dis (+ (abs (- posi 2)) (abs (- posj 0)) )))
			((= 8 numero) (setq dis (+ (abs (- posi 1)) (abs (- posj 0)) )))
			(T 0))))

;Aptitud segun metodo left
;Llamada de prueba (Aptitud-Left '#2A((4 5 7)(6 0 2)(1 3 8)))
(defun Aptitud-Left (estado)
	(let ((apt 0))
	    (dotimes (i 3 apt)
	    	(dotimes (j 3)
	    	(setf apt (+ apt (manhattan (aref estado i j) i j))) ))))

;Aptitud segun metodo random
;Llamada de prueba (Aptitud-random)
(defun Aptitud-random ()
	(let ((ran 0))
		(setq ran (random 8))))


(defun cuadrado (x) (* x x))
;Distancia que hacen falta de una pieza para ponerlo en su lugar
(defun euclides (numero posi posj)
	(let ((dis 0))
		(cond
			((= 1 numero) (setq dis (+ (cuadrado (- posi 0)) (cuadrado (- posj 0)))))
			((= 2 numero) (setq dis (+ (cuadrado (- posi 0)) (cuadrado (- posj 1)))))
			((= 3 numero) (setq dis (+ (cuadrado (- posi 0)) (cuadrado (- posj 2)))))
			((= 4 numero) (setq dis (+ (cuadrado (- posi 1)) (cuadrado (- posj 2)))))
			((= 5 numero) (setq dis (+ (cuadrado (- posi 2)) (cuadrado (- posj 2)))))
			((= 6 numero) (setq dis (+ (cuadrado (- posi 2)) (cuadrado (- posj 1)))))
			((= 7 numero) (setq dis (+ (cuadrado (- posi 2)) (cuadrado (- posj 0)))))
			((= 8 numero) (setq dis (+ (cuadrado (- posi 1)) (cuadrado (- posj 0)))))
			(T 0))))

;Aptitud segun metodo custom (distancia euclidiana)
;Llamada de prueba (Aptitud-custom '#2A((4 5 7)(6 0 2)(1 3 8)))
(defun Aptitud-custom (estado)
	(let ((apt 0))
	    (dotimes (i 3 apt)
	    	(dotimes (j 3)
	    	(setf apt (+ apt (euclides (aref estado i j) i j)))))
	    (setq apt (floor (sqrt apt)))))

;Regresa la aptitud de un estado
(defun Aptitud (estado metodo)
	(let ((apt 0))
		(case metodo
        	(:wrong-pieces
	        	(setq apt (Aptitud-Wrong estado)))
	        (:moves-left 
		        (setq apt (Aptitud-Left estado)))
	        (:random-value
	        	(setq apt (Aptitud-random)))
	        (:custom-value
	        	(setq apt (Aptitud-custom estado)))) apt))

;Lista de insercion segun aptitud
(defun insert (nodo lista)
	(cond
		((null lista) (list nodo))
		((<= (fifth nodo) (fifth (first lista))) (cons nodo lista))
		(T (cons (first lista) (insert nodo (rest lista))))))

;Inserta por un metodo especifico un en la frontera de busqueda
;:wrong-pieces para el número de piezas en lugar incorrecto
;:moves-left para la suma de movimientos necesarios para colocar cada pieza en su lugar correcto
;:random-value para evaluación aleatoria
;:custom-value para la función propuesta por el estudiante

;Llamada de prueba (insert-to-open '#2A((8 7 3)(4 6 0)(5 1 2)) '(:arriba (1)) :wrong-pieces)
(defun insert-to-open (estado op  metodo)
	(let* ((apt (Aptitud estado metodo))
		(nodo (create-node estado op apt)))
		(setq *open* (insert nodo *open*))))

;Obtiene el primer elemento de la frontera de busqueda
;Llamada de prueba (get-from-open)
(defun get-from-open ()
	(pop  *Open*))

;Busca la posicion del cero
;Llamada de prueba (find-zero '#2A((1 2 3)(4 5 6)(7 0 8)))
(defun  find-zero (estado)
    (let ((aux NIL)
    	(found T))

    	(loop for i to 3 while found do
    		(dotimes (j 3 aux)
    			(when (zerop (aref estado i j)) 
    				(setq found NIL)
    				(setq aux (list i j))))) aux))

;Verifica que se pueda aplicar un operador
;Llamada de prueba 
;(valid-operator? '(:izquierda (1)) '#2A((1 1 3)(0 2 5)(6 7 8)))
(defun  valid-operator? (op  estado)  
  (let*  ((operador (first op))
  		(cero (find-zero estado))
  		(aux1 (first cero))
  		(aux2 (second cero)))

    (case operador 
        	(:arriba
	        	(if (< (- aux1 1) 0) NIL T))
	        (:abajo 
		        (if (>= (+ aux1 1) 3) NIL T))
	        (:izquierda
	        	(if (< (- aux2 1) 0) NIL T))
	        (:derecha
	        	(if (>= (+ aux2 1) 3) NIL T)))))

;Verifica que un estado sea valido
;Llamada de prueba (valid-state? '#2A((1 2 3)(0 4 5)(6 7 8)))
(defun  valid-state? (estado)
    (let* ((aux NIL))
    (dotimes (i 3 aux)
			(dotimes (j 3 aux)
				(if (zerop (aref estado i j)) (setq aux T))))))

;Aplica un operador a un estado 
;Llamada de prueba 
;(apply-operator '(:arriba (1)) '#2A((1 2 3)(4 0 5)(6 7 8)))
(defun  apply-operator (op  estado)
    (let*  ((cero (find-zero estado))
  			(i (first cero)) 
  			(j (second cero))
  			(aux (make-array '(3 3) :initial-element 0))
  			(valor 0)
	       	(operador (first op)))      ;; este operador es la etiqueta humana del operador...
    (dotimes (x 3)
	    (dotimes (y 3) 
	    	(setf (aref aux x y) (aref estado x y))))
	 (case operador 
	    (:arriba
	    	(setq valor (aref aux (- i 1) j))
	    	(setf (aref aux i j) valor)
	    	(setf (aref aux (- i 1) j) 0))
	    (:abajo
	    	(setq valor (aref aux (+ i 1) j))
	    	(setf (aref aux i j) valor)
	    	(setf (aref aux (+ i 1) j) 0)) 
	    (:derecha
	    	(setq valor (aref aux i (+ j 1)))
	    	(setf (aref aux i j) valor)
	    	(setf (aref aux i (+ j 1)) 0))
	    (:izquierda
	    	(setq valor (aref aux i (- j 1)))
	    	(setf (aref aux i j) valor)
	    	(setf (aref aux i (- j 1)) 0))
	    (T "error"))aux))

;Expande un estado
;(expand '#2A((1 2 3)(8 0 4)(7 5 6)))
(defun expand (estado)
     (let ((descendientes  NIL)
	     	(nuevo  NIL))
     		(setq *expandidos* (+ *expandidos* 1))
           	(dolist  (op  *Ops*  descendientes)
		 		(when (valid-operator?  op  estado) 
		 			(setq  nuevo (apply-operator op estado))
	                (setq  descendientes  (cons  (list nuevo op) descendientes))))))

;Predicado que busca por id
(defun  remember-state?  (estado  lista-memoria)
     (cond ((null  lista-memoria)  Nil)
	    ((equalp  estado  (second (first  lista-memoria)))  T)
		(T  (remember-state?  estado  (rest  lista-memoria))))  )

;Memoria, verifica que ese estano no se haya analizado antes
(defun  filter-memories (lista-estados-y-ops) 
     (cond ((null  lista-estados-y-ops)  Nil)
	    ((remember-state? (first (first  lista-estados-y-ops)) *memory*)
		    (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )

;Recupera la solucion de la frontera de busqueda
(defun extract-solution (nodo)
     (labels ((locate-node  (id  lista)
		(cond ((null  lista)  Nil)
				((eql  id  (first (first  lista))) (first  lista))
				(T  (locate-node  id (rest  lista))))))
	    (let ((current  (locate-node  (first  nodo)  *memory*)))
		(loop  while  (not (null  current))  do                        
			(push  current  *solucion*)
			(setq  current  (locate-node  (third  current) *memory*))))*solucion*))

;Despliega la Solucion
(defun  display-solution (lista-nodos)
    (format  t  "Solución con: ~A  operadores~%" (1- (length  lista-nodos)))
    (format  t  "Solución con: ~A  estados expandidos~%" *expandidos*)
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))
	       ;;else
		   (format t "\(~A\) aplicando ~A  se  obtiene  a  ~A~%"  i (fourth  nodo)  (second  nodo)))))  )  

;Resetea
(defun reset-all () 
	(setq *expandidos* 0)
    (setq  *open*  NIL)
    (setq  *memory*  NIL)
    (setq  *id*  0)
    (setq  *current-ancestor*  NIL)
    (setq  *solucion*  NIL))

;Realiza una búsqueda, por el método especificado y desde un estado inicial hasta un estado meta
;los métodos posibles son:  :wrong-pieces para el número de piezas en lugar incorrecto
;					        :moves-left para la suma de movimientos necesarios para colocar cada pieza en su lugar correcto
;		        			:random-value para evaluación aleatoria
;	        				:custom-value para la función propuesta por el estudiante
(defun  blind-search (edo-inicial  edo-meta  metodo)
  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))

      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada
			        (null *open*))  do
	   (setq  nodo    (get-from-open)   
		     estado  (second  nodo)
		     operador  (third  nodo))
	   (push  nodo  *memory*)
	   (cond    ((equalp  edo-meta  estado)  
		    		(format  t  "Éxito. Meta encontrada con un maximo de estado en frontera: ~A~%" (first  nodo))
		            (display-solution  (extract-solution  nodo))
		            (setq  meta-encontrada  T))
		        
		        (T 
		        	(setq  *current-ancestor*  (first  nodo)) 
			     	(setq  sucesores  (expand estado))
			     	(setq  sucesores  (filter-memories  sucesores))
			      	(loop for  element  in  sucesores  do
			      		(cond 
			      			((equalp  edo-meta  estado)  
		    					(format  t  "Éxito. Meta encontrada con un maximo de estado en frontera: ~A~%" (first  nodo))
		            			(display-solution  (extract-solution  nodo))
		            			(setq  meta-encontrada  T))
			      			
				    		(T (insert-to-open  (first element)  (second element)  metodo)))))))))

;Llamada a la funcion principal
(time (blind-search *Inicio* *Final* :wrong-pieces))
(time (blind-search *Inicio* *Final* :moves-left))
(time (blind-search *Inicio* *Final* :random-value))
(time (blind-search *Inicio* *Final* :custom-value))      				