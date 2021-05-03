(defglobal
	?*dim_tablero* = 4
	?*damas_cpu* = 0
	?*damas_per* = 0
)

(deftemplate tablero
    (multislot fichas)
)

(deftemplate turno
    (slot jugador)
    (slot p)
    (slot c)
)

(defrule init_global
	(not (init_tipo tipo))
	=>
    (printout t "¡Bienvenido a las damas!" crlf)
    (printout t "=> Elige el tamanio del tablero: (4, 6, 8)" crlf)
    (bind ?var1 (read))
    (assert (init_tipo ?var1)) ; init_tipo 4/6/8 -> lanza regla de inicializar
    (printout t "=> Elige el primer turno: (c/p)" crlf)
    (bind ?var1 (read))
    (assert (init_turno ?var1))
    
    (if (eq ?var1 c) then
	    (assert (turno (jugador ?var1)(p x)(c o)))
	else
	    (assert (turno (jugador ?var1)(p o)(c x)))
	)
)

(deffunction mostrar_tablero ($?f)
	(printout t ?f crlf)
))

(defrule inicializar_tablero
    ?i <- (init_tipo ?tipo)    
    =>
    ; Tablero tipo 4x4
    (if (= ?tipo 4) then
    		(bind ?*dim_tablero* 4)
       	(assert (tablero (fichas o 1 1 o 1 3 x 4 2 x 4 4)))
       	(retract ?i)
    )
    ; Tablero tipo 6x6
    (if (= ?tipo 6) then
    		(bind ?*dim_tablero* 6)
        	(assert (tablero (fichas o 1 1 o 1 3 o 2 2 o 2 4 o 2 6 x 5 1 x 5 3 x 5 5 x 6 2 x 6 4 x 6 6)))
        	(retract ?i)
    )
    ; Tablero tipo 8x8
    (if (= ?tipo 8) then
    		(bind ?*dim_tablero* 8)
        	(assert (tablero (fichas o 1 1 o 1 3 o 2 2 o 2 4 o 2 6 o 3 1 o 3 3 o 3 5 o 3 7 x 6 2 x 6 4 x 6 6 x 6 8 x 7 1 x 7 3 x 7 5 x 7 7 x 8 2 x 8 4 x 8 6 x 8 8)))
        	(retract ?i)
    )
    (assert (imprimir si))
)

(defrule mostrar
	?imp <- (imprimir si)
	?tabl <- (tablero (fichas $?f))
	=>
	(mostrar_tablero $?f)
	(retract ?imp)
)

(deffunction mover (?fich $?f)
	(printout t "=> Moviendo..." crlf)
	(printout t "=> Tu ficha: " ?fich crlf)
	(printout t "==> Fila: " crlf)
	(bind ?fila (read))
	(printout t "==> Columna: " crlf) 
	(bind ?col (read))
	(printout t "==> Quieres mover... fila: " ?fila " col: " ?col crlf)
	
	
	(loop-for-count ()
	
	)
)

(defrule mover_per
    (tablero (fichas $?f))
    (turno (jugador p) (p ?fich))
    =>
    (printout t "=> Mueve persona" crlf)
    (mover ?fich $?f)
)

(defrule mover_cpu
    (tablero (fichas $?f))
    (turno (jugador c) (c ?fich))
    =>
    (printout t "=> Mueve cpu" crlf)
    (mover ?fich $?f)
)







