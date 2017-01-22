;Morales Torres Jonatan
;Los archivos estan hechos en el editor SublimeText :) 

;1
;Defina una función "ElemenInPos" que reciba tres
;argumentos elem, lista y pos. La función debe devolver
;T si elem está en a posición de lista y NIL si no lo está. 
(ElemenInPos 1 '(1 B 3 A) 0)

(defun ElemenInPos (elem lista pos)
	(labels((aux (e l p c) 
		(cond
			((null l) NIL)
			((and (equal (first l) e) (equal c p)) T)
			((> c p) NIL)
			(T (aux e (rest lista) p (+ 1 c) )))))
	(aux elem lista pos 0)))

;2
;Escriba la función "Inicio-en" que recibe como
;augumentos una lista y un elemento cualquiera
;La función debe entregar como respuesta una copia
;de la lista original pero comenzando con la
;primera ocurrencia del elemento dado en la lista orignal.
(Inicio-en '(A B C D) 'C)

(defun Inicio-en (lista elem)
	(labels ((aux(l e B) 
		(cond
			((null l) NIL)
			((equal (first l) e) (setq B (+ B 2)) (cons (first l) (aux (rest l) e B)) )
			((> B 1) (cons (first l) (aux (rest l) e B)))
			(T (aux (rest l) e B)))))
	(aux lista elem 0)))

;3
;Modifique la funcion anterior para que se llame "Termina-en"
;y entregue como respuesta una copia de la lista
;original pero que termina en la última ocurrencia
;del elemento dado
(Termina-en '(A B C D) 'C)

(defun Termina-en (lista elem)
	(labels ((aux(l e B) 
		(cond
			((null l) NIL)
			((equal (first l) e) (setq B (+ B 2)) (cons (first l) (aux (rest l) e B)))
			((< B 1) (cons (first l) (aux (rest l) e B)))
			(T (aux (rest l) e B)))))
	(aux lista elem 0)))
;4
;Construya una función "Primer-impar" que reciba
;como argumento una lista y como respuesta entregue
;otra lista conteniendo el primer elemento de la
;lista original que sea un número impar y la posición
;donde se encuentra.
(Primer-impar '(2 3 4 5))

(defun Primer-impar (lista)
	(labels ((aux(l B)
		(cond
			((null l) NIL)
			((and (oddp (first l)) (equal 0 B)) (aux (rest l) (+ 1 B)))
			(T (cons (first l) (aux (rest l) B))))))
	(aux lista 0)))

;5
;Modifique la función del inciso anterior para que
;entregue en la lista de respuesta el "último elemento"
;de la lista que sea un número real mayor o igual
;que cero y el número de veces que dicho elemento
;se repite en toda la lista.
(ultimo '(1 2 3 3 4 5 6 3 3))

(defun ultimo (lista)
	(labels(( aux(l n v)
		(cond
			((null l) (cons n v))
			((equal (first l) n) (aux (rest l) n (+ 1 v)))
			(T (aux (rest l) n v)))))
	(aux lista (first (last lista)) 0)))

;6
;Escriba la función "Conteo" que recíbe como argumento
;una lista cualquiera y, como respuesta, entregue
;una celda de construccion cuya primera parte contiene 
;el conteo de elemento numéricos de la lista orignal y cuya segunda
;parte contiene el conteo de sublistas contenidas en la liista original
(Conteo '(1 2 3 A (A A) 3 (B B)))

(defun Conteo (lista)
	(labels (( aux(l n s)
		(cond 
			((null l) (cons n s))
			((numberp (first l)) (aux (rest l) (+ 1 n) s))
			((listp (first l)) (aux (rest l) n (+ 1 s)))
			(T (aux (rest l) n s)))))
	(aux lista 0 0)))

;7
;Defina una función "Aplana" que reciba como argumento
;una lista con elementos anidados a cualquier nivel
;de profundidad y, como respuesta, entregue una lista
;conteniendo los mismos elementos pero todos ellos al nivel
;principal de profundidad.
(Aplana '(A (A B) C (D F)))

(defun Aplana (lista)
	(labels(( aux (l)
		(cond
			((null l) NIL)
			((atom (first l)) (cons (first l) (aux (rest l))))
			(T (append (aux (first l)) (aux (rest l)))))))
		(aux lista)))

;8
;Escriba la función "Diagnal" que recibe como argumento
;una lista conteniendo m sub-listas de n elementos
;cada una de ellas y que representa una matri de mxn
;elementos. Como respuesta, está función debe devolver
;una lista conteniendo los elementos en la diagonal de 
;dicha matriz.
(defvar m 0)
(setq m (make-array '(3 5) :initial-element 2))
(diagonal m)

(defun diagonal (lista)
    (labels ((aux (l n i)
    	(cond
    		((equal n i) NIL)
    		(T (cons (aref l i i) (aux l n (+ 1 i)))))))
    (aux lista (first (array-dimensions lista)) 0)))

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
	(labels((aux(l)
		(cond
			((null l) NIL)
			((equal NIL (first l)) (cons 'N (aux (rest l))))
			((atom (first l)) (cons 'A (aux (rest l))))
			((listp (first l)) (cons 'L (aux (rest l))))
			)))
	(aux lista)))

;10 
;Defina la función "Suma-numerica" que recibe como argumento
;una lista cualquiera (no anidada) y como respuesta
;entrega la suma de exclusivamente aquellos elementos de la lista
;que son numericos.
(Suma-numerica '(1 2 3 A B 8 C D))

(defun Suma-numerica (lista)
	(labels ((aux(l sum)
		(cond
			((null l) sum)
			((numberp (first l)) (aux (rest l) (+ sum (first l))))
			(T (aux (rest l) sum))
			)))
	(aux lista 0)))

;11
;Escriba una función "Filtra-vocales" que reciba
;como argumento una lista y como respuesta entregue
;una copia de la lista argumento en la cual se han
;removido las letras vocales.
(Filtra-vocales '(A B C D E))

(defun Filtra-vocales (lista)
	(labels((aux(l)
		(cond
			((null l) NIL)
			((equal (first l) 'A) (aux (rest l)))
			((equal (first l) 'E) (aux (rest l)))
			((equal (first l) 'I) (aux (rest l)))
			((equal (first l) 'O) (aux (rest l)))
			((equal (first l) 'U) (aux (rest l)))
			(T (cons (first l) (aux (rest l)))))))
	(aux lista)))

;12
;Construya una función "Filtra-multiplos" que reciba como
;argumentos una lista y un numero entero. Como respuesta
;debe entregar una copia de la lista argumento en la
;cual se han removido todos los multiplos del entero recibido.
(Filtra-multiplos '(1 2 3 4 5 6) 2)

(defun Filtra-multiplos (lista numero)
	(labels ((aux (l n)
		(cond
			((null l) NIL)
			((equal (mod (first l) n) 0) (aux (rest l) n))
			(T (cons (first l) (aux (rest l) n))))))
	(aux lista numero)))

;13
;Defina la función "Celdas" que recibe como argumento
;una lista y como respuesta entrega el numero de celdas
;de construccion que contiene la representacion interna de
;la lista de argumento.
(Celdas '(A B C.E))

(defun Celdas (lista)
	(labels ((aux (l i n)
		(cond 
			((null l) i)
			((equal i n) (aux (rest l) i n))
			(T (aux (rest l) (+ i 1) n)))))
	(aux lista 0 (length lista))))

;14
;Construya una función "Implica" con aridad indeterminada
; que implemente el operador logico de la impliación
(Implica 1 2 3 'a NIL 'b NIL 'a 'z)

(defun Implica (&rest aridad)
	(labels(( aux (a u l c)
		(cond
			((equal c l) u)
			((and (equal (first a) NIL) (equal u NIL)) (aux (rest a) T l (+ 1 c)))
			((equal(first a) NIL) (aux (rest a) NIL l (+ 1 c)))
			((equal u NIL) (aux (rest a) T l (+ 1 c)))
			(T (aux (rest a) T l (+ 1 c))))))
	(aux aridad NIL (length aridad) 0)))

;Si el ultimo elemento de la lista es NIL la respuesta
;Es NIl porque no importa lo que tengas anteriormente,
;De lo contrario la respuesta siempre será T
(defun Implica (&rest aridad)
	(cond
		((equal (first (last aridad)) NIL) NIl)
		(T T)))

;15
;Escriba una funci'on "Mult" que recibe como argumento
;dos listas conteniendo sub-listas numericas, representando
;matrices. La funcion debe regresar la multiplicacion
;de las dos matrices si es que estas son compatbles
;en caso de no serlo debe regresar NIL.
(defvar ma1 0)
(setq ma1 (make-array '(3 5) :initial-element 2))

(defvar ma2 0)
(setq ma2 (make-array '(5 1) :initial-element 2))

(Mult ma1 ma2)

(defun Mult (matriz1 matriz2)
	(let* 
		((dim1 (array-dimensions matriz1))
		(n1 (first dim1))	
   		(m1 (second dim1))
   		(dim2 (array-dimensions matriz2))
   		(m2 (second dim2))
   		(mf (make-array `(,n1 ,m2) :initial-element 0)))
	(labels (
		(aux3 (ma1 ma2 mf n1 m2 m1 i j k)
			(cond 
				((equal k m1) NIL)
				(T  (setf (aref mf i j) (+ (aref mf i j) (* (aref ma1 i k) (aref ma2 k j)))) 
					(aux3 ma1 ma2 mf n1 m2 m1 i j (+ k 1))))
		)
		(aux2 (ma1 ma2 mf n1 m2 m1 i j)
			(cond 
				((equal j m2) mf)
				(T (aux3 ma1 ma2 mf n1 m2 m1 i j 0) 
					(aux2 ma1 ma2 mf n1 m2 m1 i (+ 1 j))))
		)
		(aux1 (ma1 ma2 mf n1 m2 m1 i)
			(cond 
				((equal i n1) mf)
				(T (aux2 ma1 ma2 mf n1 m2 m1 i 0) 
					(aux1 ma1 ma2 mf n1 m2 m1 (+ i 1))))
		))
	(aux1 matriz1 matriz2 mf n1 m2 m1 0))))

;16
;Defina una función recursiva "Find" que reciba
;dos argumentos: elem y lista. La función debe
;devolver NIL si elem no es un elemento de lista
;de lo contrario deberá devolver la sublista que
;comienza con la primera instancia de elem
(Find '(1 2 3 4 5 6 7) 4)

(defun Find (lista elem)
	(labels ((aux(l e B) 
		(cond
			((null l) NIL)
			((equal (first l) e) (setq B (+ B 2)) (cons (first l) (aux (rest l) e B)) )
			((> B 1) (cons (first l) (aux (rest l) e B)))
			(T (aux (rest l) e B)))))
	(aux lista elem 0)))

;17
;Defina una función recursiva "Cambia" que reciba
;como argumento una lista y dos elementos elem1 elem2
;Como respuesta la función debe entregar otra lista
;parecida a la original, pero donde todas las ocurrencias
;de elem1 se substituyeron por elem2.
(Cambia '(1 2 3 4 5 3 3) 3 9)

(defun Cambia (lista elem1 elem2)
	(labels (( aux(l e1 e2)
		(cond
			((null l) NIL)
			((equal (first l) e1) (cons e2 (aux (rest l) e1 e2)))
			(T (cons (first l) (aux (rest l) e1 e2))))))
	(aux lista elem1 elem2)))

;18
;Implemente todas las opciones que ahí se presentan
;y compare su desempeño con time para  el argumento 50
(fib 10)
;A
(defun fib (n)
  (check-type n (integer 0 *))
  (if (< n 2) n
      (+ (fib (1- n)) (fib (- n 2)))))
;B
(defun fib (n)
  (check-type n (integer 0 *))
  (labels ((fib-aux (n f1 f2)
                    (if (zerop n) f1
                      (fib-aux (1- n) f2 (+ f1 f2)))))
          (fib-aux n 0 1)))
;C
(defun fib (n)
  (check-type n (integer 0 *))
  (loop for f1 = 0 then f2
        and f2 = 1 then (+ f1 f2)
        repeat n finally (return f1)))
;D
(defun fib (n)
  (check-type n (integer 0 *))
  (do ((i n (1- i))
       (f1 0 f2)
       (f2 1 (+ f1 f2)))
      ((= i 0) f1)))
;E
(defun fib (n)
  (check-type n (integer 0 *))
  (labels ((fib-aux (n k)
                    (if (zerop n)
                        (funcall k 0 1)
                      (fib-aux (1- n) (lambda (x y)
                                        (funcall k y (+ x y)))))))
          (fib-aux n #'(lambda (a b) a))))
;F
(defun fib (n)
   (labels ((fib2 (n)
                 (cond ((= n 0)
                        (values 1 0))
                       (t
                        (multiple-value-bind (val prev-val)
                                             (fib2 (- n 1))
                           (values (+ val prev-val)
                                   val))))))
      (nth-value 0 (fib2 n))))
;G
(defun fib (n)
  (check-type n (integer 0 *))
  (labels ((fib-aux (a b p q count)
                    (cond ((= count 0) b)
                          ((evenp count)
                           (fib-aux a
                                    b
                                    (+ (* p p) (* q q))
                                    (+ (* q q) (* 2 p q))
                                    (/ count 2)))
                          (t (fib-aux (+ (* b q) (* a q) (* a p))
                                      (+ (* b p) (* a q))
                                      p
                                      q
                                      (- count 1))))))
          (fib-aux 1 0 0 1 n)))
;H
(defun fib (n)
  (if (< n 2) n
    (if (oddp n) 
      (let ((k (/ (1+ n) 2)))
        (+ (expt (fib k) 2) (expt (fib (1- k)) 2)))
      (let* ((k (/ n 2)) (fk (fib k)))
        (* (+ (* 2 (fib (1- k))) fk) fk)))))
;I
(defun fib (n &optional (i 1) (previous-month 0) (this-month 1)) 
 (if (<= n i)
      this-month
    (fib n (+ 1 i) this-month (+ this-month previous-month))))

;19
;Defina una función recursiva "Mapea" que opere
;exactamente igual que la función "mapcar" de Common Lisp.
(Mapea #'+ '(1 0 2)'(4 2 5))

(defun Mapea (función lista1 lista2)
	(labels((aux(f l1 l2)
		(cond
			((and (null l1) (null l2)) NIL)
			((null l1) NIL)
			((null l2) NIL)
			(T (cons (funcall f (first l1) (first l2)) (aux f (rest l1) (rest l2))))
			)))
	(aux función lista1 lista2)))

;21
;Defina una función recursiva "Elimina" que reciba
;como argumento una lista y un número real n. La
;función debe entregar como resultado una copia de la lista
;original en la cual se hayan eliminado todos los elementos
;que no sea númericos, así como todos aquellos elementos
;numericos que sean menores o iguales a n.
(Elimina '(A 1 2 3 B 5) 2)

(defun Elimina (lista numero)
	(labels ((aux (l n)
			(cond
				((null l) NIL)
				((and (numberp (first l)) (> (first l) n)) (cons (first l) (aux (rest l) n)))
				(T (aux (rest l) n)))))
	(aux lista numero)))
;22
;Defina una función recursiva "PegaYCambia" que reciba
;como argumento dos listas lista1 lista2 y dos elementos
;elem1 elem2. Como respuesta la función debe entregar
;una lista donde concatene las dos listas originales.
;pero substituyendo todas las ocurrencias de elem1 por elem2.
(PegaYCambia '(A B C D) '( E F A A) 'A 'B)
;Para hacer mas facil esta funcion haremos uso
;de la funcion 17 Cambia..

(defun PegaYCambia (lista1 lista2 elem1 elem2)
	(append (Cambia lista1 elem1 elem2 ) (Cambia lista2 elem1 elem2)))

;23
;Defina una función "QSort" que como argumento unico
;una lista e implemente el algoritmo.
(defvar b (make-array '(1) :initial-element '(2 3 5 1 0 8 3)))
(QSort b)

(defun QSort (lista)
	(let ((r 0))
		(labels(( aux (A p q)
			(cond 
				((< p q)
					(setq r (partition A p q))
					(aux A p r)
					(aux A (+ r 1) q) (format t "~a~%" A)))))
	(aux lista 0 (- (length lista) 1)))))

(defun partition (A p q)
	(let ((pivot (aref A p))
		(i p)
		(tem 0))
		(do ((j (+ p 1) (+ j 1))) ((equal j q) i)
			(cond 
				((<= (aref A j) pivot)
					(setq i (+ 1 i))
					(setq tem (aref A i)) 
					(setf (aref A i) (aref A j))
					(setf (aref A j) tem))))
		(setq tem (aref A i))
		(setf (aref A i) (aref A p))
		(setf (aref A p) tem)))