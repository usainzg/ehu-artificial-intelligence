(defglobal
    ?*DIM* = 4
    ?*COLOR_JUG* = TRUE ; TRUE = blancas; FALSE = negras
    ?*TURNO* = TRUE ; TRUE = turno jugador; FALSE = turno cpu
    ?*SYM_B* = "o"
    ?*SYM_N* = "x"
    ?*SYM_B_DAMA* = "O"
    ?*SYM_N_DAMA* = "X"
    ?*SYM_VACIO* = " "
    ?*FICHA_PEON* = "P"
    ?*FICHA_DAMA* = "D"
)

(deftemplate tablero
    (multislot negras)
    (multislot blancas)
)

; Funcion auxiliar para cambiar de turno
(deffunction cambiar_turno ()
    (bind ?*TURNO* (not ?*TURNO*)) ; Negar la variable logica, TRUE => FALSE / FALSE => TRUE
    (return ?*TURNO*)
)

; Funcion auxiliar para comprobar si posicion existe en el tablero
(deffunction existe_pos (?x ?y)
    (return (and (> ?x 0) (> ?y 0) (<= ?x ?*DIM*) (<= ?y ?*DIM*)))
)

; Funcion auxiliar para saber si elemento pertenece a vector 
(deffunction item_in_vector (?item $?vector)
    (if (member$ ?item $?vector) then
        (return TRUE)
    else
        (return FALSE)
    )
)

; crea una linea de fichas donde ?x e ?y son las
; coordenadas de la primera ficha por la izquierda
(deffunction crear_linea (?x ?y)
    (bind ?res "")
    (bind ?res (str-cat ?*FICHA_PEON* ?x ?y))
    (loop-for-count (?i ?x (- ?*DIM* 1))
        (if (eq 0 (mod ?i 2)) then
            (bind ?res (str-cat ?res " " ?*FICHA_PEON* (+ ?x ?i) ?y))
        )
    )
    (return ?res)
)

; Dibuja el tablero
(deffunction print_tablero (?blancas ?negras)
    (loop-for-count (?i 0 ?*DIM*)
        (bind ?linea "")
        (bind ?fila (- ?*DIM* ?i))
        (loop-for-count (?col 0 ?*DIM*)
            (if (= ?col 0) then ; columna etiqueta de numeros
                (bind ?linea (str-cat ?fila))
                (if (= ?fila 0) then
                    (bind ?linea (str-cat ?linea " "))
                )
            else (if (= ?fila 0) then ; fila etiqueta de numeros
                (bind ?linea (str-cat ?linea (str-cat ?col " ")))
            else ; dibujar fichas del tablero
                (bind ?posibles_fichas ; posible ficha en ?col, ?fila => puede ser peon o dama 
                    (create$ (sym-cat ?*FICHA_PEON* ?col ?fila)(sym-cat ?*FICHA_DAMA* ?col ?fila)))
                (bind ?esta_ficha FALSE)
                ; buscamos la ficha en las blancas
                (foreach ?p_fi ?posibles_fichas
                    (if (item_in_vector ?p_fi ?blancas) then
                        (bind ?tipo_ficha (sub-string 1 1 ?p_fi))
                        (if (eq ?tipo_ficha ?*FICHA_PEON*) then
                            (bind ?esta_ficha ?*SYM_B*)
                        else
                            (bind ?esta_ficha ?*SYM_B_DAMA*)
                        )
                        (break)
                    )
                )
                (if (not ?esta_ficha) then ; buscamos ficha en las negras
                    (foreach ?p_fi ?posibles_fichas
                        (if (item_in_vector ?p_fi ?negras) then
                            (bind ?tipo_ficha (sub-string 1 1 ?p_fi))
                            (if (eq ?tipo_ficha ?*FICHA_PEON*) then
                                (bind ?esta_ficha ?*SYM_N*)
                            else
                                (bind ?esta_ficha ?*SYM_N_DAMA*)
                            )
                            (break)
                        )
                    )
                )
                (if (not ?esta_ficha) then
                    (bind ?esta_ficha " ")
                )
                (bind ?linea (str-cat ?linea (str-cat "|" ?esta_ficha)))
            ))
        )
        (printout t ?linea crlf)
    )
)

; Crea el tablero inicial
(deffunction crear_tablero ()
    (bind ?negras "")
    (bind ?blancas "")
    (bind ?lineas (- (/ ?*DIM* 2) 1))
    (loop-for-count (?i 1 ?lineas)
        (if (eq 0 (mod ?i 2)) then
            (bind ?blancas (str-cat ?blancas (crear_linea 2 ?i) " "))
            (bind ?negras (str-cat ?negras (crear_linea 1 (- ?*DIM* ?i -1)) " "))
        else
            (bind ?blancas (str-cat ?blancas (crear_linea 1 ?i) " "))
            (bind ?negras (str-cat ?negras (crear_linea 2 (- ?*DIM* ?i -1)) " "))
        )
    )
    ; Cambiar las fichas a multicampos
    (bind ?negras (explode$ ?negras))
    (bind ?blancas (explode$ ?blancas))

    (printout t "====> POSICIONES INICIALES <==== " crlf)
    (printout t "=> Negras: " ?negras crlf)
    (printout t "=> Blancas: " ?blancas crlf)

    (assert(tablero (blancas ?blancas) (negras ?negras))) ; Introducimos tablero inicial
    (print_tablero ?blancas ?negras) ; Mostramos tablero inicial
)

(deffunction turno_jugador (?blancas ?negras ?color ?pieza_a_mover)
    (bind ?mov (pedir_mov ?blancas ?negras ?color ?pieza_a_mover))
    (aplicar_movimiento ?blancas ?negras ?mov ?color)
)

(deffunction turno (?blancas ?negras ?pieza_a_mover)
    (if (eq ?*TURNO* ?*COLOR_JUG*) then
        (printout t "==> TURNO DEL JUGADOR <==" crlf)
        (turno_jugador ?blancas ?negras ?*COLOR_JUG* ?pieza_a_mover)
        (return TRUE)
    else
        (printout t "==> TURNO DEL ORDENADOR <==" crlf)
        (print_tablero ?blancas ?negras)
        (return FALSE)
    )
)

(defrule iniciar_tablero
    ?f <- (init_global)
    =>
    (retract ?f)
    (crear_tablero)
)

(defrule fin_juego
    (fin_juego)
    =>
    (halt)
)

(defrule salir
    (salir)
    =>
    (printout t "====> ADIOS <====" crlf)
    (halt)
)

(deffunction pedir_param()
    (bind ?tam -1)
    (while (or (< ?tam 4) (> ?tam 9) (not (= 0 (mod ?tam 2)))) do
        (printout t "Introduce el tamaño del tablero (> 3 & < 10 & par): ")
        (bind ?tam (read))
    )
    (bind ?*DIM* ?tam)
    (bind ?exit FALSE)
    (while TRUE
        (printout t "¿Juegas con blancas o negras? (b/n): ")
        (bind ?color (read))
        (if (= 0 (str-compare "b" (lowcase ?color))) then
            (bind ?*COLOR_JUG* TRUE)
            (return)
        )
        (if (= 0 (str-compare "n" (lowcase ?color))) then
            (bind ?*COLOR_JUG* FALSE)
            (return)
        )
    )
)

(defrule pedir_param
    (inicio)
    =>
    (pedir_param)
    (assert (init_global))
    (return)
)

(deffacts inicializacion
    (inicio)
)