;Morales Torres Jonatan
;GLOL

;Estado inicial		((1 1 1 1)(0 0 0 0))
;Estado final		((0 0 0 0)(1 1 1 1)) 

(defparameter  *open* '())		;; Frontera de busqueda...                                              
(defparameter  *memory* '())	;; Memoria de intentos previos

(defparameter  *ops*  '( 
	(:Lobo 				(1 0 0 0))	;; Operadores para el problema (Lobo Oveja Legumbres) 
	(:Oveja        		(0 1 0 0))
	(:Legumbres        	(0 0 1 0))
	(:Granjero			(0 0 0 1)) ) )

(defparameter  *id*  0)  ;; Identificador del ultimo nodo creado, cada vez que se cree un nodo se debe incrementar
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

;Crea un nuevo nodo de la forma (id estado antecesor operador-de-creacion)
;Llama de prueba (create-node '((1 1 1 0) (0 0 0 1)) '(:Lobo (1 0 0)))
(defun  create-node (estado  op)
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  (1-  *id*)  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;Inserta por un metodo especifico un en la frontera de busqueda
;Llama de prueba (insert-to-open '((1 1 1 0) (0 0 0 1)) '(:Lobo (1 0 0 0)) :depth-first)
(defun insert-to-open (estado  op  metodo) 
     (let ((nodo  (create-node  estado  op)))
         (cond ((eql  metodo :depth-first)
	                  (push  nodo  *open*))
	           ((eql  metodo :breath-first)
		          (setq *open*  (append  *open*  (list nodo))))
	   	   (T  Nil)))  )

;Obtiene el primer elemento de la frontera de busqueda
;Llamada de prueba (get-from-open)
(defun get-from-open ()
	(pop  *Open*))

;Busca de que lado se encuentra el granjero
;Llamada de prueba (barge-shore '((1 1 1 0)(0 0 0 1)))
(defun  barge-shore (estado)
     (if  (= 1 (fourth (first  estado)))  0  1))

;Verifica que se pueda aplicar un operador
;Llamada de prueba 
;(valid-operator? '(:Lobo (1 0 0 0)) '((1 0 1 0) (0 1 0 1)) )
;(valid-operator? '(:Oveja (0 1 0 0)) '((1 1 1 1) (0 0 0 0)))
;(valid-operator? '(:Legumbres (0 0 1 0)) '((1 1 1 1) (0 0 0 0)))
;(valid-operator? '(:Granjero (0 0 0 1)) '((1 1 1 0) (0 0 0 1)))
(defun  valid-operator? (op  estado)  
  (let*  (
  		(orilla  	(barge-shore  estado))                         
	    (Lobo  		(first  (nth  orilla  estado)))   
	    (Oveja 		(second (nth  orilla  estado)))
	    (Legumbres 	(third  (nth  orilla  estado))))

    (and 	(>=  Lobo  (first (second op)))              
        	(>=  Oveja (second (second op)))
        	(>=  Legumbres (third (second op)))))  )

;Verifica que un estado sea valido
;Llamada de prueba (valid-state? '((1 1 1 0) (0 0 0 1)))
(defun  valid-state? (estado)
    (let* ((orilla1  (first  estado))
	    (orilla2  (second  estado))
    	(Lo0  (first orilla1))
	    (Ov0  (second orilla1))
	    (Le0  (third orilla1))
	    (G0   (fourth orilla1))
	    (Lo1  (first orilla2))
	    (Ov1  (second orilla2))
	    (Le1  (third orilla2))
	    (G1	  (fourth orilla2)))

    (and 	(or  (/= Lo0 Ov0) (=  G0 Lo0))
    		(or  (/= Ov0 Le0) (=  G0 Ov0))
    		(or  (/= Lo1 Ov1) (=  G1 Lo1))
    		(or  (/= Ov1 Le1) (=  G1 Ov1)) )))

;Voltea un bit
(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

;Aplica un operador a un estado 
;Llamada de prueba 
;(apply-operator '(:Lobo (1 0 0 0)) '((0 0 1 0) (1 1 0 1)))
;(apply-operator '(:Oveja (0 1 0 0)) '((1 1 1 1) (0 0 0 0)))
;(apply-operator '(:Legumbres (0 0 1 0)) '((1 0 1 1) (0 1 0 0)))
;(apply-operator '(:Granjero (0 0 0 1)) '((1 1 1 1) (0 0 0 0)))
(defun  apply-operator (op  estado) 
    (let*  ((orilla1  (first  estado))
	       	(orilla2  (second  estado))
	       	(Lo0  (first orilla1))
	    	(Ov0  (second orilla1))
	    	(Le0  (third orilla1))
	    	(Lo1  (first orilla2))
	    	(Ov1  (second orilla2))
	    	(Le1  (third orilla2))
	       	(G  (barge-shore estado)) 
	       	(operador (first op)))      ;; este operador es la etiqueta humana del operador...
	 (case operador 
	    (:Lobo 
	    	(if (= G 0)  ;;siempre  restar elementos de la orilla con la barca y sumarlos en la otra orilla...
	            (list  (list  (- Lo0 1) Ov0 Le0 G)  (list  (+ Lo1 1) Ov1 Le1 (flip G)))
				(list  (list  (+ Lo0 1) Ov0 Le0 G)  (list (- Lo1 1) Ov1 Le1 (flip G)))))
	    (:Oveja   
	    	(if (= G 0)  
	            (list  (list  Lo0 (- Ov0 1) Le0 G)  (list  Lo1 (+ Ov1 1) Le1 (flip G)))
				(list  (list  Lo0 (+ Ov0 1) Le0 G)  (list Lo1 (- Ov1 1) Le1 (flip G))))) 
	    (:Legumbres  
	    	(if (= G 0)  
	 		    (list  (list  Lo0 Ov0 (- Le0 1) G)   (list  Lo1 Ov1 (+ Le1 1) (flip G)))
				(list  (list  Lo0 Ov0 (+ Le0 1) G)  (list Lo1 Ov1 (- Le1 1) (flip G)))))
	    (:Granjero 
	 		    (if (= G 0)  
	 		    (list  (list  Lo0 Ov0 Le0 G)  (list Lo1 Ov1 Le1 (flip G)))
				(list  (list  Lo0 Ov0 Le0 G)  (list Lo1 Ov1 Le1 (flip G)))))
	    (T "error"))))

;Expande un estado 
;Llamada de prueba (expand '((1 0 1 1) (0 1 0 0)))
(defun expand (estado)
     (let ((descendientes  nil)
	     	(nuevo-estado  nil))
           	(dolist  (op  *Ops*  descendientes) 
	        	(setq  nuevo-estado  (apply-operator  op estado))
		 		(when (and (valid-operator?  op  estado) 
			        (valid-state?  nuevo-estado))
	                (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))) )

;Predicado que busca por id
(defun  remember-state?  (estado  lista-memoria)
     (cond ((null  lista-memoria)  Nil)
	    ((equal  estado  (second (first  lista-memoria)))  T)
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
    (format  t  "Solución con ~A  pasos~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))
	       ;;else
		   (format t "\(~A\) aplicando ~A  se  obtiene  a  ~A~%"  i (fourth  nodo)  (second  nodo)))))  ) 

;Resetea
(defun reset-all () 
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil))

;Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
;los métodos posibles son:  :depth-first - búsqueda en profundidad
;                           :breath-first - búsqueda en anchura
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
	   ;(format  t  "Entro: ~A~%" nodo)
	   (push  nodo  *memory*)
	   (cond    ((equal  edo-meta  estado)  
		                (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
		                (display-solution  (extract-solution  nodo))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expand estado))
			     (setq  sucesores  (filter-memories  sucesores))
			      (loop for  element  in  sucesores  do
				    (insert-to-open  (first element)  (second element)  metodo))))))  )

;Llamada a la funcion principal
(defparameter  *Inicio* '((1 1 1 1) (0 0 0 0)))	;; Estado Inicial                                             
(defparameter  *Final* '((0 0 0 0) (1 1 1 1)))	;; Estado Final

(blind-search  *Final* *Inicio* :breath-first)
(blind-search *Inicio* *Final* :depth-first)