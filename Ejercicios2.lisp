;Morales Torres Jonatan 
;Los archivos estan hechos en el editor SublimeText :) 

;1
;Defina una función "ElemenInPos" que reciba tres
;argumentos elem, lista y pos. La función debe devolver
;T si elem está en a posición de lista y NIL si no lo está. 
(ElemenInPos 1 '(1 B 3 A) 0)

(defun ElemenInPos (elem lista pos)
	(let ((bool nil) (j 0))
		(dolist (i lista bool) 
			(and (equal i elem) (equal j pos)
				(setq bool T)
				(format t "~a~%" i))
			(setq j (1+ j)))))

;2  
;Escriba la función "Inicio-en" que recibe como
;augumentos una lista y un elemento cualquiera
;La función debe entregar como respuesta una copia
;de la lista original pero comenzando con la
;primera ocurrencia del elemento dado en la lista orignal.
(Inicio-en '(A B C D) 'B)

(defun Inicio-en (lista elem)
	(let ((aux '())
		(B 0))
		(dolist (i lista aux)
			(cond
				((equal i elem)  (setq aux (cons i aux)) (setq B (+ B 2)))
				((> B 1) (setq aux (cons i aux)))))
		(setq aux (reverse aux))))

;3
;Modifique la funcion anterior para que se llame "Termina-en"
;y entregue como respuesta una copia de la lista
;original pero que termina en la última ocurrencia
;del elemento dado
(Termina-en '(A B C D) 'C)

(defun Termina-en (lista elem)
	(let ((aux '())
		(B 0))
		(dolist (i lista aux)
			(cond
				((equal i elem)  (setq aux (cons i aux)) (setq B (+ B 2)))
				((< B 1) (setq aux (cons i aux)))))
		(setq aux (reverse aux))))

;4
;Construya una función "Primer-impar" que reciba
;como argumento una lista y como respuesta entregue
;otra lista conteniendo el primer elemento de la
;lista original que sea un número impar y la posición
;donde se encuentra.
(Primer-impar '(2 3 4 5))

(defun Primer-impar (lista)
	(let ((impar 1)
		(pos 0)
		(aux '()))
		(do ((i 0 (+ 1 i))) ((oddp (elt lista i)) aux)
			(setq pos (+ 1 i))
			(format t "pos: ~a ~%" pos)
			(setq impar (elt lista pos)))
		(setq aux (cons pos (cons impar lista)))))

;5
;Modifique la función del inciso anterior para que
;entregue en la lista de respuesta el "último elemento"
;de la lista que sea un número real mayor o igual
;que cero y el número de veces que dicho elemento
;se repite en toda la lista.
(ultimo '(1 2 3 3 4 5 6 3))

(defun ultimo (lista)
	(let ((veces 0)
		(num 0)
		(aux '()))
		(setq num (first (last lista)))
		(dolist (i lista aux)
			(if (equalp i num)
				(setq veces(+ 1 veces))))
		(setq aux (cons num veces))))

;6
;Escriba la función "Conteo" que recíbe como argumento
;una lista cualquiera y, como respuesta, entregue
;una celda de construccion cuya primera parte contiene 
;el conteo de elemento numéricos de la lista orignal y cuya segunda
;parte contiene el conteo de sublistas contenidas en la liista original
(Conteo '(1 2 3 A (A A) 3 (B B)))

(defun Conteo (lista)
	(let ((num 0)
		(sub 0)
		(aux '()))
		(dolist (i lista aux)
			(cond 
				((numberp i) (setq num (+ 1 num)))
				((listp i) (setq sub (+ 1 sub)))))
		(setq aux (cons num sub))))


;7
;Defina una función "Aplana" que reciba como argumento
;una lista con elementos anidados a cualquier nivel
;de profundidad y, como respuesta, entregue una lista
;conteniendo los mismos elementos pero todos ellos al nivel
;principal de profundidad.
(Aplana '(A (A B) C (D F)))

(defun Aplana (lista)
	  (let ((aux '()))
	    (do ((B T))
	      	((equal B nil) lista)
	      	(setq B nil)
	      	(dolist (i lista aux)
				(cond ((listp i)(setq B T aux (append aux i)))
		      		(T (setq aux (append aux (list i))))))
	    (setq lista aux aux '()))))

;8
;Escriba la función "Diagnal" que recibe como argumento
;una lista conteniendo m sub-listas de n elementos
;cada una de ellas y que representa una matri de mxn
;elementos. Como respuesta, está función debe devolver
;una lista conteniendo los elementos en la diagonal de 
;dicha matriz.
(defvar m 0)
(setq m (make-array '(2 4) :initial-element 2))
(diagonal m)

(defun diagonal (lista)
    (let* ((dim (array-dimensions lista))
   		(n (first dim))
   		(aux '()))
    	(do* ((i 0 (+ 1 i))) ((= i n) aux)
    		(setq aux (cons (aref lista i i) aux))
        	(format t "~a~%" aux))
    	(setq aux (reverse aux))))

;9
;Construya una función que recíba como argumento
;una lista cualquiera y como respuesta, entregue 
;una lista con el mismo numero de elementos de primer
;nivel, pero que contiene un simbolo A si el elemento
;en la posición correspondente es un atomo, un simbolo
;L si el elemento correspondiente es una lista y un simbolo N
; si el elemento en la posición correspondiente es una lista vacia.
(ALN '(A B () (A B)))

(defun ALN (lista)
	(let ((aux '()))
		(dolist (i lista aux)
			(cond
				((equal NIL i) (setq aux (cons 'N aux)))
				((atom i) (setq aux (cons 'A aux)))
				((listp i) (setq aux (cons 'L aux)))))
		(setq aux (reverse aux))))

;10 
;Defina la función "Suma-numerica" que recibe como argumento
;una lista cualquiera (no anidada) y como respuesta
;entrega la suma de exclusivamente aquellos elementos de la lista
;que son numericos.
(Suma-numerica '(1 2 3 A B 8 C D))

(defun Suma-numerica (lista)
	(let ((sum 0))
		(dolist (i lista sum) 
			(if (numberp i) (setq sum (+ i sum))))))

;11
;Escriba una función "Filtra-vocales" que reciba
;como argumento una lista y como respuesta entregue
;una copia de la lista argumento en la cual se han
;removido las letras vocales.
(Filtra-vocales '(A B C D E))

(defun Filtra-vocales (lista)
	(let ((aux '())) 
		(dolist (i lista aux)
			(cond
				((equal i 'A) NIL)
				((equal i 'E) NIL)
				((equal i 'I) NIL)
				((equal i 'O) NIL)
				((equal i 'U) NIL)
				(T (setq aux (cons i aux)))))
		(setq aux (reverse aux))))

;12
;Construya una función "Filtra-multiplos" que reciba como
;argumentos una lista y un numero entero. Como respuesta
;debe entregar una copia de la lista argumento en la
;cual se han removido todos los multiplos del entero recibido.
(Filtra-multiplos '(1 2 3 4 5 6) 2)

(defun Filtra-multiplos (lista num)
	(let ((aux '()))
		(dolist (i lista aux)
			(cond 
				((equal (mod i num) 0) NIL)
				(T (setq aux (cons i aux)))))
		(setq aux (reverse aux))))

;13
;Defina la función "Celdas" que recibe como argumento
;una lista y como respuesta entrega el numero de celdas
;de construccion que contiene la representacion interna de
;la lista de argumento.
(Celdas '(A B C.E))

(defun Celdas (lista)
	(let ((aux 0))
	    (dolist	(i lista aux)
	    	(cond
	    		((equal aux (length lista)) aux)
	    		(T (setq aux (+ 1 aux)))))))

;14
;Construya una función "Implica" con aridad indeterminada
; que implemente el operador logico de la impliación
(Implica 1 2 3 'a NIL 'b NIL 'a 'z)

(defun Implica (&rest aridad)
	(let ((aux NIL))
		(dolist (i aridad aux)
			(cond
				((equal i NIL) (setq aux NIL))
				(T (setq aux T))))))

;15
;Escriba una funci'on "Mult" que recibe como argumento
;dos listas conteniendo sub-listas numericas, representando
;matrices. La funcion debe regresar la multiplicacion
;de las dos matrices si es que estas son compatbles
;en caso de no serlo debe regresar NIL.
(defvar m1 0)
(setq m1 (make-array '(3 5) :initial-element 2))

(defvar m2 0)
(setq m2 (make-array '(5 1) :initial-element 2))

(Mult m1 m2)

(defun Mult (lista1 lista2)
	(let* 
		((dim1 (array-dimensions lista1))
		(n1 (first dim1))	
   		(m1 (second dim1))
   		(dim2 (array-dimensions lista2))
   		(n2 (first dim2))
   		(m2 (second dim2))
   		(aux (make-array `(,n1 ,m2) :initial-element 0)))
		(cond
			((not (equal n2 m1)) (format t "No es posible la multiplicacion~%"))
			(T 
				(dotimes (i n1 aux)
					(dotimes (j m2 aux)
						(dotimes (k m1 aux)
							(setf (aref aux i j) (+ (aref aux i j) (* (aref lista1 i k) (aref lista2 k j)))))))
			aux))))