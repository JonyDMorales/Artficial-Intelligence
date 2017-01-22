;Morales Torres Jonatan
;Motor de Consultas

(defparameter *knowledge-vector* (make-array 1 :adjustable T :element-type 'list )) ;;Vector donde se ubica la base de conocimiento
(defparameter *Ruta* "/home/jonyd/Escritorio/Sexto Semestre/Artificial/Ejercicios/Base_de_Conocimiento.lisp") ;;Ruta de la base de conocimiento

;Abre una base de conocimiento y la vacia en un vector
(defun Read-Knowledge (filename)
	(with-open-file (stream filename)
    (let ((numrows (read stream)))
      	(adjust-array *knowledge-vector* numrows)
      	(read-line stream nil nil)
      	(dotimes (row numrows *knowledge-vector*)
			(setf (aref *knowledge-vector* row)(read stream nil nil))))))

;Predicado para saber si es una lista impropia
(defun lista-a? (lista)
	(if (listp (rest lista)) NIL T))

;Predicado que te indica si toda la lista es impropia
(defun lista-impropia? (lista)
  	(let ((valor T))
    
    (cond 
    	((null lista) nil)
	  	
	  	(T (dolist (elem lista valor)
	     		(cond
	       			((not (listp elem)) (setq valor nil))
	       			((lista-a? elem) (setq valor (and valor T)))
	       			(T (setq valor nil))))))))

;Predicado que verifica que una consulta sea valida
(defun consulta-valida? (entrada)
    (cond 
    	((not (listp entrada)) nil)
	  	((and (or (equal (first entrada) '+) (equal (first entrada) '-)) (lista-impropia? (rest entrada))) T)
	  	((and (or (equal (first entrada) '*) (equal (first entrada) '/)) (= 2 (length (rest entrada)))
			(lista-impropia? (second entrada))
			(lista-impropia? (third entrada))) T)
	  	((eql (first (third entrada)) 'or) T)
	  (T nil)))

;Imprime tuplas de conocimiento segun su id
(defun imprime (id)
	(dolist (i id)
		(format t "~%*************************************************************************************~%")
	    (format t "~%~A" (aref *knowledge-vector* i))))

;Predicado que te indica se se encuentra en la lista
(defun se-encuentra? (item lista)
  	(and (consp lista) (or (equal item (first lista)) (se-encuentra? item (rest lista)))))

;Obtiene la primera parte de una lista impropia
(defun getH (asc-list)
	(first asc-list))

;Obtiene la primera segunda de una lista impropia
(defun getV (asc-list)
  	(rest asc-list))

;Macro que ayuda a expadir la funcion case a operar con cadenas
(defmacro case-expand (exp &body clauses)
  (let ((temp (gensym)))
    `(let ((,temp ,exp))
    (cond ,@(mapcar #'(lambda (clause)
		(destructuring-bind (keys . clause-forms) clause
		(cond 
			((eq keys 'otherwise) `(T ,@clause-forms))
            (T (if (atom keys) (setq keys (list keys)))
                `((member ,temp ',keys :test #'equal) ,@clause-forms)))))clauses)))))

;Evalua un atributo numerico dentro de una lista
(defun Evalua-Op (asc-list atrib)
  	(let ((cad (getV asc-list))
		(valor 0)
		(op nil))

    (setq valor (parse-integer (subseq cad 1) :junk-allowed T))
 
    (cond 
    	((null valor) (setq valor (parse-integer (subseq cad 2) :junk-allowed T)) (setq op (subseq cad 0 2)))
	  	(T (setq op (char cad 0))))
    
    (case-expand op
      	("<=" (<= (getV atrib) valor))
      	(">=" (>= (getV atrib) valor))
      	("!=" (/= (getV atrib) valor))
      	(#\< (< (getV atrib) valor))
      	(#\> (> (getV atrib) valor)))))

;Predicado que verifica un operador sobre un atributo numerico
(defun OpValid? (clausula)
  	(let ((cad (getV clausula))
		(valor 0)
		(op nil))
    
    (if (or (symbolp cad)(numberp cad)) (setq cad "inválido"))
    (if (< (length cad) 2)(setq cad "inválido"))
	(setq valor (parse-integer (subseq cad 1) :junk-allowed T) op (char cad 0))
    (cond 
    	((null valor) (setq valor (parse-integer (subseq cad 2) :junk-allowed T)) (setq op (subseq cad 0 2))
	   		(cond ((null valor) nil)
		 		((equal "<=" op) T)
		 		((equal ">=" op) T)
		 		((equal "!=" op) T)
		 		(T nil)))
	  	((eql #\< op) T)
	  	((eql #\> op) T)
	  	(T nil))))

;Busca existencialmente en las tuplas de conocimiento
(defun Busca+ (proposicion univ)
  	(let ((filas (-(array-total-size univ) 1))
		(caso nil)
		(cl nil)
		(matches nil))
    
    (loop for i from 0 to filas do
	 	(let ((match nil))
	   	(setq caso (aref univ i))
	   	(dolist (clausula proposicion match)
	     	(let ((flag 0))
	     	(cond
	       
	       		((se-encuentra? clausula caso) (setq match (push 'T match)))

	       		((OpValid? clausula) (do ((n 0 (+ 1 n))) ((= n  (length caso)) match)
		  			(setq cl (nth n caso))
		  			(if (and (equal (getH cl)(getH clausula)) (numberp (getV cl)))
		      				(setq match (push (Evalua-Op clausula cl) match) flag 1)))
					(if (zerop flag) (setq match (push nil match))))

	       		(T (setq match (push nil match))))))

	   (setq match (push 'and match))
	   (setq match (eval match))
	   (if match (setq matches (push i matches)))))
	(if (null matches) (list nil nil) (list T matches))))

;Busca para todas las tuplas de conocmimiento
(defun Busca* (antecedente consecuente)
  	(let ((sub-univ nil)
		(caso nil)
		(satisf T)
		(alist nil)
		(cumplen nil)
		(excep nil))
    
    (setq sub-univ (second (Busca+ antecedente *knowledge-vector*)))
    (cond
      ((zerop (length sub-univ)) (list T nil nil))
      (T (dolist (i sub-univ satisf)
	   		(setq caso (aref *knowledge-vector* i) satisf T)
	   		(dolist (cls consecuente satisf)
	     		(cond
	       			((OpValid? cls) (do ((n 0 (+ n 1))) ((or (= n (- (length caso) 1))
			 			(null satisf)) satisf)
		  				(setq alist (nth n caso))
		  				(if (and (equal (getH alist)(getH cls)) (numberp (getV alist)))
		      				(setq satisf (and satisf (Evalua-Op cls alist))))))
	       
	       			((se-encuentra? cls caso) (setq satisf (and satisf T)))

	       			(T (setq satisf nil))))
	   		
	   		(if satisf (setq cumplen (push i cumplen))
	       	(setq excep (push i excep))))))
    (if (null excep) (list T cumplen excep) (list nil cumplen excep))))

;Busca no-existe en las tuplas de conocimiento
(defun Busca- (proposicion)
  	(let ((contraejemplos nil))
    
    (setq contraejemplos (second (Busca+ proposicion *knowledge-vector*)))
    (if (null contraejemplos) (list T nil) (list nil contraejemplos))))

;Busca no-paratodo en las tuplas de conocimiento
(defun Busca/ (antecedente consecuente)
  	(let ((contraejemplos nil))
    
    (setq contraejemplos (second (Busca* antecedente consecuente)))
    (if (null contraejemplos) (list NIL T NIL) (list T NIL contraejemplos) )))

;Imprime una lista con los id de las tuplas que cumplan con la busqueda
(defun imprime-resultado (resultado)
  	(let ((tipo nil)
		(valor nil)
		(casos nil)
		(excep nil))

    (cond
      	((= 3 (length resultado))(setq tipo 0 valor (first resultado) casos (second resultado) excep (third resultado)))
      
      	(T (setq tipo 1 valor (first resultado) casos (second resultado))))
    
    (cond
    	;;Casos todo y no todo
      	((and (= 0 tipo) valor (not (null casos))) (format t "~%True~%") (imprime excep) (format t "~%*************************************************************************************~%"))
      
      	((and (= 0 tipo) valor (null casos)) (format t "~%True~%") (imprime excep) (format t "~%*************************************************************************************~%"))
      	
      	((and (= 0 tipo) (not valor)) (format t "~%False~%") (imprime excep) (format t "~%*************************************************************************************~%"))
      	
      	;;Casos existe y no existe
      	((and (= 1 tipo) valor (not (null casos))) (format t "~%True~%") (imprime casos) (format t "~%*************************************************************************************~%"))
      	
      	((and (= 1 tipo) valor (null casos)) (format t "~%True~%") (format t "~%*************************************************************************************~%"))
       	
       	((and (= 1 tipo) (not valor) (not (null casos))) (format t "~%False~%") (imprime casos) (format t "~%*************************************************************************************~%"))
       	
       	((and (= 1 tipo) (not valor) (null casos)) (format t "~%False~%") (format t "~%*************************************************************************************~%")))))

;Cuando el usuario inserta un or
(defun Consulta-or (consulta)
	(let* ((a (second consulta))
		(b (third consulta))
		(tamaño (length b))
		(aux NIL))
		(loop for i from 1 to (1- tamaño) do
			(setq aux (cons  a (list (nth i b))))
			(cond
				((eql (first consulta) '+) (imprime-resultado (Busca+ aux *knowledge-vector*)))
				((eql (first consulta) '-) (imprime-resultado (Busca- aux)))))))

;Pide al usuario una consulta
(defun Pedir_Consulta ()
  	(let ((consulta nil))
	(format t "Introduce la consulta:~%")
	(setq consulta (read))
	(when (consulta-valida? consulta)
		(if (eql (first (third consulta)) 'or) (consulta-or consulta) 
		(cond
			((eql (first consulta) '+) (imprime-resultado (Busca+ (rest consulta) *knowledge-vector*)))
			((eql (first consulta) '-) (imprime-resultado (Busca- (rest consulta))))
			((eql (first consulta) '*) (imprime-resultado (Busca* (second consulta) (third consulta))))
			((eql (first consulta) '/) (imprime-resultado (Busca/ (second consulta) (third consulta)))))))))

;Consulta en la base de conocimiento la peticion del usuario
(defun Consultas ()
	(let ((Terminar NIL)
		(opcion NIL))
		(Read-Knowledge *Ruta*)
		(loop until Terminar do
			(format t "~%~%*********************************************~%")
			(format t "~%Formatos de las consultas:~%")
			(format t "Existe       (+ atributos..)~%")
			(format t "No Existe    (- atributos..)~%")
			(format t "Para todo    (* (atributos..) (atributos..))~%")
			(format t "No Para Todo (/ (atributos..) (atributos..))~%")
			(format t "~%~%*********************************************~%~%")
			(format t "Quieres hacer una consulta T or NIL:~%")
			(setq opcion (read))
			(if opcion (Pedir_Consulta) (setq Terminar T)))))

;Llamada a la funcion proncipal
(Consultas)