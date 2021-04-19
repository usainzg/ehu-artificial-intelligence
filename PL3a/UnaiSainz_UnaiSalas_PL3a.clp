; Ejercicio 1: simplemente comparamos si el numero esta en el rango.
(deffunction dentroDelRango (?a ?b)
	(printout t "Introduce un numero: " crlf)
	(bind ?var1 (read))
	(while (or(< ?var1 ?a)(> ?var1 ?b))
		(printout t "No esta en el rango... " crlf)
		(bind ?var1 (read))
	)
	(printout t "Numero correcto!" crlf)
)

; Ejercicio 2: calculamos mcd con la formula del enunciado.
(deffunction mcd (?a ?b)	
	(while (not(= ?a ?b))
		(if (> ?a ?b) then 
			(bind ?a (- ?a ?b))
		else 
			(bind ?b (- ?b ?a))
		)
	)
	(return ?a)
)

; Ejercicio 3: calculamos mcm siguiendo el enunciado.
(deffunction mcm (?a ?b)	
	(bind ?var1 (mcd ?a ?b))
	(bind ?var2 (* ?a ?b))
	(bind ?res (div ?var2 ?var1))
	(return ?res)	
)

; Ejercicio 4: comprobamos que miembros no estan en ?b y si lo
; estan en ?a.
(deffunction diferencia (?a ?b)
	(progn$ (?var (create$ ?a))
		(if (not(member$ ?var ?b)) then
			(printout t ?var crlf)
		)
	)
)

; Ejercicio 5: concatenamos ?a y ?b.
(deffunction concatenacion (?a ?b)
	(bind ?res (insert$ ?a (length$ ?a) ?b))
	(return ?res)
)

; Ejercicio 6: producto cartesiano de ?a y ?b.
(deffunction cartesiano (?a ?b)
	(progn$ (?aa (create$ ?a))
		(progn$ (?bb (create$ ?b))
			(printout t ?aa " " ?bb crlf)	
		)
	)
)

; Ejercicio 7: producto escalar de la forma a1*b1 + a2*b2.
(deffunction escalar (?a ?b)
	(bind ?res 0)
	(bind ?n 1)
	(progn$ (?aa (create$ ?a))
		(bind ?res (+ (* ?aa (nth$ ?n ?b)) ?res))
		(bind ?n (+ ?n 1))	
	)
	(return ?res)
)

; Ejercicio 8: comprobamos si ?a es primo.
(deffunction es_primo (?a)
	(bind ?prim TRUE)
	(loop-for-count (?i 2 (- ?a 1)) 
		(if (= (mod ?a ?i) 0) then
			(bind ?prim FALSE)	
		)
	)
	(return ?prim)
)

; Ejercicio 8: comprobamos si ?a es capicua.
(deffunction es_capicua (?a)
	(if (< ?a 10)then (return FALSE))
	(bind ?originalA ?a)
	(bind ?revA 0)
	(while (not(= ?a 0))
		(bind ?rem (mod ?a 10))
		(bind ?revA (+ (* ?revA 10) ?rem))
		(bind ?a (div ?a 10))
	)
	(if (= ?originalA ?revA) then
		(return TRUE)
	else 
		(return FALSE)
	)
)

; Ejercicio 8: usamos las dos funciones previamente programadas
; para imprimir los X primero numeros primos y capicua.
(deffunction num_primos_y_capicua ()
	(printout t "Introduce X: " crlf)
	(bind ?a (read))
	
	(bind ?n 0)
	(bind ?aux 1)
	
	(while (< ?n ?a)
		(if (and (es_primo ?aux)(es_capicua ?aux)) then
			(printout t ?n ": "?aux crlf)
			(bind ?n (+ ?n 1))
		)
		(bind ?aux (+ ?aux 1))
	)
)

; Ejercicio 9: funcion auxiliar para el calculo de la suma de
; los digitos de un numero ?a.
(deffunction sum (?a)
	(bind ?s 0)
	(while (not(= ?a 0))
		(bind ?s (+ ?s (mod ?a 10)))
		(bind ?a (div ?a 10))	
	)
	(return ?s)
)

; Ejercicio 9: usando la funcion anterior, devolver el numero
; magico, definido en el enunciado.
(deffunction num_magico ()
	(printout t "Introduce X: " crlf)
	(bind ?a (read))
	
	(bind ?res (sum ?a))
	(while (> ?res 10)
		(bind ?res (sum ?res))
	)
	(return ?res)
)

; Ejercicio 10: siguiendo el enunciado, calculamos si el numero
; es medio (suma de inferiores igual a suma de mayores).
(deffunction esMedio (?a)
	(bind ?inf 0)
	(bind ?i 1)
	(while (< ?i ?a)
		(bind ?inf (+ ?inf ?i))
		(bind ?i (+ ?i 1))
	)

	(bind ?sup 0)
	(bind ?i (+ ?a 1))
	(while (not(= ?inf ?sup))
		(if (> ?sup ?inf) then (return FALSE))
		(bind ?sup (+ ?sup ?i))
		(bind ?i (+ ?i 1))
	)
	(return TRUE)
)