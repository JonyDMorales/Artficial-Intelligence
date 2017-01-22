;Morales Torres Jonatan
;Los archivos estan hechos en el editor SublimeText :)

;1
;Collect, argumentos un predicado
;y una lista. Recursiva devuelve una lista
;en la cual se encuentran todos los elementos
;del argumento orignal para los cuales
;se cumple el predicado del primer argumento
(Collect #'+ '(1 2 3))

(defun Collect (predicado lista)
	(labels ((aux (p l)
			(cond 
				((funcall p (first l)) T)
				(T NIL))))
	(aux predicado lista)))

;2
;Palindromo, argumento una lista, recursiva
;predicado, si la lista recibida es un palindromo
;regresa T, de lo contrario NIL
(Palindromo '(s e v a n s u s n a v e s))

(defun Palindromo (lista)
	(if (atom lista) NIL)
	(labels(( aux(l lr)
		(cond 
			((and (null l)(null lr)) T)
			((null l) NIL)
			((null lr) NIL)
			((equal (first l) (first lr)) (aux (rest l) (rest lr)))
			(T () NIL))))
	(aux lista (reverse lista))))

;3
;2Palindrome, argumento una cadena, recursiva,
;no destructiva, entrtega como respuesta
;una cadena como la original, pero convertida
;en palindromo (duplicandola en orden inverso al final)
(Palindrome "hola")

(defun Palindrome (cadena)
	(if (listp cadena) NIL)
	(if (atom cadena) NIL)
	(labels(( aux(c cr l i)
		(cond 
			((> i l) cr)
			(T (setq cr (concatenate 'string cr (make-string 1 :initial-element (elt c (- l i))))) 
				(aux c cr l (+ 1 i))))))
	(aux cadena cadena (length cadena) 1)))

;4
;IterativePalindrome, iterativa, modifique la
;funcion del ejercicio anterior para que opere
;de forma estrictamente iterativa.
(IterativePalindrome "hola")

(defun IterativePalindrome (cadena)
	(if (listp cadena) NIL)
	(if (atom cadena) NIL)
	(let ((cr cadena)
		(l (length cadena)))
		(do ((i 1 (+ 1 i))) ((> i l) cr)
			(setq cr (concatenate 'string cr (make-string 1 :initial-element (elt cadena (- l i))))))))
;5
;ListRotate, augumentos una cadena, un entero
;n y una de las dos llaves :rigth o :left
;Devuelve una lista de la misma longitud
;que la original, pero rolada n posiciones
;hacia la direccion indicada por el tercer
;argumento

(defun ListRotate (lista número &key (:rigth r) (:left l))
	(cond 
		((numberp lista) NIL)
		((listp número) NIL))
	(labels(( aux(l n i)
			(cond
				((null l) l)
				((> n i) (setq l )))))
	(aux lista numero r l)))

;6
;MaxPos, argumento un arreglo bidimensional
;global conteniendo números reales iterativa
;Entrega una lista de asociacion de a forma
;en la cual cada asociacion indica el renglon
;del arreglo en que se encuentra el mayor 
;valor de la columna correcpondiente
(defvar a 0)
(setq a (make-array '(2 2) :initial-contents '((1 2)(3 4))))

(MaxPos a)

(defun MaxPos (arreglo)
	(cond 
		((numberp arreglo) NIL)
		((listp arreglo) NIL))
	(let* ((dim (array-dimensions arreglo))
		(n (first dim))	
   		(m (second dim))
		(aux '())
		(aux1 0)
		(lis '()))

		(dotimes (i n aux)
			(dotimes (j m aux)
				(cond
					((> aux1 (aref arreglo i j)) (setq aux1 aux1))
					((>= (aref arreglo i j) aux1) (setq aux1 (aref arreglo i j)))))
			(setq lis (cons i aux1))
			(setq aux (cons lis aux)))
		(reverse aux)))

;7
;Combine, argumentos una funcion fune y una lista
;recursiva efecto igual al operador reduce
;combina todos los elementos de lista 
;mediante la aplicacion de la funcion fune
(Combine #'+ '(1 0 2))

(defun Combine (función lista)
	(labels((aux(f l)
		(cond
			((null l) NIL)
			(T (cons (funcall f (first l)) (aux f (rest l))))
			)))
	(aux función lista)))

;8
;Level, argumentos una cadena y una lista anidada
;si la cadena se encuentra en lista, regresa
;el nivel de pronfundidad o anidamiento comenzando en cero
;en el que se encontro, de lo contrario regresa NIL
(Level "abc" '(1 2 ("ab" ("abc")) 3 4))

(defun Level (cadena lista)
	(cond 
		((numberp cadena) NIL)
		((numberp lista) NIL)
		((listp cadena) NIL))
	(let ((i 0))
	  (labels((aux (c l i)
		(cond
			((null l) NIL)
			((equalp c (first l)) (format t "Profundidad: ~a~%" i))
			((atom (first l)) (cons (first l) (aux c (rest l) i)))
			(T (append (aux c (first l) (+ 1 i)) (aux c (rest l) i))))))
		(aux cadena lista i))))
;9
;StrEncode, argumento una cadena, recursiva
;devuelve una lista de asociacion que indica
;el numero de veces que se repite consecuentemente
;cada elemento de la lista original
(StrEncode "aabbc")

(defun StrEncode (cadena)
	(cond 
		((numberp cadena) NIL)
		((listp cadena) NIL))
	(labels(
		(aux(c l i r)
		(cond
			((>= i l) r)
			(T (setq r (cons (cons  (elt c i) (aux2 c (elt c i) l 0 0)) r)) (aux c l (+ 1 i) r)))
		)
		(aux2 (c ch l j con)
			(cond 
				((= j l) con)
				((equalp ch (elt c j)) (setq con (+ 1 con)) (aux2 c ch l (+ 1 j) con))
				(T (aux2 c ch l (+ 1 j) con)))
		))
	(aux cadena (length cadena) 0 NIL)))

;10
;StrCypher, argumentos una cadena y otra
;cadena code de longitud 27 donde cada
;posicion corresponde a una letra del alfabeto
;incluyendo la ñ, devuelve una cadena en la que
;cada caracter del argumento original fue
;substituido por el indicado en la posicion
;correcpondiente en la cadena code.
(StrCypher "aabbc" "abcdefghijklmnñopqrstuvwxyz")

(defun StrCypher (cadena code)
	(cond 
		((numberp cadena) NIL)
		((numberp code) NIL)
		((listp code) NIL)
		((listp cadena) NIL))
	(let ((cad NIL))
		(dotimes (i (length cadena) cad)
			(dotimes (j (length code) cad)
				(cond
					((equalp (elt cadena i) (elt code j)) 
						(setq cad (concatenate 'string cad (make-string 1 :initial-element (digit-char j))))))))))

;11
;MalMult, argumentos dos arreglos m1 y m2
;de dos dimensiones y contenido valores numericos
;averigua si las matrices son compatibles
;y en caso de serlo regresa otra matriz
;con la multiplicacion de m1 y m2
(defvar ma1 0)
(setq ma1 (make-array '(2 0) :initial-element 2))

(defvar ma2 0)
(setq ma2 (make-array '(5 1) :initial-element 2))

(MaMult ma1 ma2)

(defun MaMult (lista1 lista2)
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
;12
;BTree, argumentos un número elem y tree una
;lista anidada posiblemente vacia, inserte elem
;en el arbol binario ordenado tree y regresa el
;nivel de profundidad en el que fue insertado.
;si elem ya existía en el arbol o no puede ser
;insertado por alguna otra razon, la funcion devuelve nil.
(BTree 2 '(1 3 (4 2) 3))

(defun BTree (elem tree)
	(cond 
		((numberp tree) NIL)
		((listp elem) NIL))
	(let ((i 0))
	  (labels((aux (e tr i)
		(cond
			((null tr) NIL)
			((equalp elem (first tr)) NIL)
			((< elem (first tr)) (cons (first tr) (aux elem (rest tr) i)) (format t "Profundidad: ~a~%" i))
			((atom (first tr)) (cons (first tr) (aux elem (rest tr) i)))
			(T (append (aux e (first tr) (+ 1 i)) (aux e (rest tr) i))))))
		(aux elem tree i))))

;13
;FilterSubsets, argumento una lista y una posicion
;pos en dicha lista, calcula y entrega una lista
;todos los subconjuntos de lista que contiene al
;elemento en la posicion pos.
(FilterSubsets '(A B C (A B) (C) (C A)) 4)

(defun FilterSubsets (lista pos)
	(cond 
		((numberp lista) NIL)
		((listp pos) NIL))
	(let ((aux (aref lista pos)
		(lis NIL)))
			(dotimes (i (length lista) lis)
				(cond
					((equal aux (aref lista i)) (setq lis (cons aux lis)))))
	(setq lis (reverse lis))))

;14
;Subsets, argumentos una lista y un entero k
;Calcula y entrega una lista conteniendo, en forma
;de listas, todos los subconjuntos, de cardinalidad k
;del conjunto lista
(Subsets '(A B C (A B) (C) (C A)) 2)

(defun Subsets (lista k)
	(cond 
		((numberp lista) NIL)
		((listp k) NIL))
	(let ((aux (aref lista k)
		(lis NIL)))
			(dotimes (i (length lista) lis)
				(cond
					((equal aux (aref lista k)) (setq lis (cons k lis)))))
	(setq lis (reverse lis))))

;15
;Ifpositive macro, escriba un macro para una estructura
;algoritmoica condicional que evalue una expresion
;numerica y tome acciones distintas en caso de que
;la exppresion evalue en un numero positivo y en
;caso contrario. La sintaxis para uso de esta macro debe ser:
(defstruct Persona nombre paterno (materno NIL) (edad 18))
(defvar Juan (make-Persona :nombre 'Juan :paterno 'Perez))
(defvar Carlos (make-Persona :nombre 'Carlos :paterno 'Morales :edad 20))

(Ifpositive Juan Carlos)

(defmacro Ifpositive (P1 P2) 
	`(let ()
		(if (> (Persona-edad ,P1) (Persona-edad ,P2))		;Ifpositive <expresion>
			(format t "Mayor: ~a~%" (Persona-nombre ,P1)))	;then-do <instrucciones>
			(format t "Mayor: ~a~%" (Persona-nombre ,P2))	;else-do <instrucciones> 
))