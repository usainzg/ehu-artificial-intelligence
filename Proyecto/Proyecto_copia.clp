; ==================> VARIABLES GLOBALES <==================

(defglobal
    ?*DIM* = 4 ; Dimension del tablero, ej: 4 => 4x4
    ?*COLOR_JUG* = TRUE ; TRUE = blancas; FALSE = negras
    ?*TURNO* = TRUE ; TRUE = turno jugador; FALSE = turno cpu
    ?*SYM_B* = "o" ; Simbolo peon blancas
    ?*SYM_B_DAMA* = "O" ; Simbolo dama blancas
    ?*SYM_N* = "x" ; Simbolo peon negras
    ?*SYM_N_DAMA* = "X" ; Simbolo dama negras
    ?*SYM_VACIO* = " " ; Simbolo vacio
    ?*FICHA_PEON* = "P" ; Tipo peon
    ?*FICHA_DAMA* = "D" ; Tipo dama
    ?*MOV_FORZADO* = FALSE ; Para indicar cuando comemos ("forzamos movimiento").
    ?*CORONADO* = FALSE ; Para indicar si un peon ha sido coronado (alcanza ultima fila del tablero).
    ?*MOV_IA* = FALSE ; Para guardar movimiento de la IA.

    ; Globales para la IA.
    ?*CONTADOR_ID* = 0
    ?*MAX_PROF* = 6
    ?*INF* = 99999
    ?*M_INF* = -99999
)

; ==================> TEMPLATES GENERALES <==================

; Template para tablero, los multicampos en negras/blancas se definen:
; => ("N11 N13") 
; Dos peones (N) en (1, 1) y (1, 3).
(deftemplate tablero
    (multislot negras)
    (multislot blancas)
)

; Tablero temporal para movimientos.
(deftemplate tablero_tmp
    (multislot blancas)
    (multislot negras)
    (slot pieza_a_mover)
)

; ==================> FUNCIONES AUXILIARES <==================

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

; Funcion auxiliar para anadir un elemento (?a) al final de un vector (?vector)
(deffunction append_to_vector(?a $?vector)
    (return (insert$ $?vector (+ 1 (length $?vector)) ?a))
)

; Funcion auxiliar que crea una linea de fichas para el tablero,
; (?x, ?y) son la posicion de la primera ficha por la izquierda.
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

; Funcion auxiliar para mostrar el tablero.
; => Columna 0, Fila 0: se usan como indicadores (etiquetas).
; => Se va mostrando el tablero de forma progresiva formando lineas y
; "pintando" las fichas que hay en ?blancas y ?negras.
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
                    (create$ (sym-cat ?*FICHA_PEON* ?col ?fila))
                )
                (bind ?esta_ficha FALSE)
                ; buscamos la ficha en las blancas
                (foreach ?p_fi ?posibles_fichas
                    (if (item_in_vector ?p_fi ?blancas) then
                        (bind ?tipo_ficha (sub-string 1 1 ?p_fi))
                        (if (eq ?tipo_ficha ?*FICHA_PEON*) then
                            (bind ?esta_ficha ?*SYM_B*)
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

; Funcion auxiliar que devuelve el tipo de la ?pieza.
(deffunction tipo_pieza (?pieza)
    (return (sub-string 1 1 ?pieza))
)

; Funcion auxiliar para calcular el numero de piezas de tipo == ?tipo.
(deffunction cuantas_piezas_tipo (?piezas ?tipo)
    (bind ?sum 0)
    (foreach ?pieza ?piezas
        (if (eq ?tipo (tipo_pieza ?pieza)) then
            (bind ?sum (+ ?sum 1))
        )
    )
    (return ?sum)
)

; Funcion auxiliar para saber cuantas damas hay.
(deffunction cuantas_damas (?piezas)
    (return (cuantas_piezas_tipo ?piezas ?*FICHA_DAMA*))
)

; Funcion auxiliar para saber cuantos peones hay.
(deffunction cuantas_peones (?piezas)
    (return (cuantas_piezas_tipo ?piezas ?*FICHA_PEON*))
)

; ==================> PARTE DEL JUEGO PARA JUGADOR <==================

; Funcion que calcula el tablero despues de haber realizado el movimiento ?mov.
; => Devuelve las fichas blancas y negras separadas por "|".
; Ejemplo: "N11 N22 | N24 N44".
(deffunction calcular_movimiento (?blancas ?negras ?mov ?color)
    (bind ?*CORONADO* FALSE)
    (bind ?long (length ?mov))

    ; Extraemos pos_origen y pos_destino.
    (bind ?pos_origen (sub-string 1 2 ?mov))
    (bind ?pos_destino (sub-string (- ?long 1) ?long ?mov))

    ; Creamos copias de las fichas:
    ; => Dependiendo del color, las aliadas y enemigas seran diferentes.
    ; TODO: cambiar ?nuevas_enemigas ?enemigas.
    (if ?color then
        (bind ?aliadas ?blancas)
        (bind ?nuevas_aliadas ?blancas)
        (bind ?enemigas ?negras)
        (bind ?nuevas_enemigas ?negras)
    else
        (bind ?aliadas ?negras)
        (bind ?nuevas_aliadas ?negras)
        (bind ?enemigas ?blancas)
        (bind ?nuevas_enemigas ?blancas)
    )

    (bind ?encontrada FALSE)
    
    ; Para guardar el indice de la ficha.
    (bind ?indice_ficha 0)
    
    ; Buscamos en las aliadas la pieza a mover.
    (loop-for-count (?i 1 (length$ ?aliadas))
        (bind ?pieza (nth$ ?i ?aliadas))
        (bind ?tipo (sub-string 1 1 ?pieza))
        (bind ?pos (sub-string 2 3 ?pieza))
        
        ; Posicion origen encontrada.
        (if (eq ?pos ?pos_origen) then
            ; Si llegamos al final del tablero despues de mover la ficha...
            (if (or (and ?color (= ?*DIM* (string-to-field (sub-string 2 2 ?pos_destino))))
                    (and (not ?color) (= 1 (string-to-field (sub-string 2 2 ?pos_destino))))) then
                
                ; Si la ficha llega al final y es un peon... "coronar" => hacer dama.
                (if (eq ?tipo ?*FICHA_PEON*) then
                    (bind ?tipo ?*FICHA_DAMA*)
                    (bind ?*CORONADO* TRUE)
                )
            )
            ; Crear la nueva ficha despues de moverla.
            (bind ?pieza_movida (sym-cat ?tipo ?pos_destino))
            (bind ?encontrada TRUE)
            
            ; Guardamos el indice de la ficha.
            (bind ?indice_ficha ?i)
            (break)
        )
    )

    ; Si encontramos la ficha...
    (if ?encontrada then
        ; Si hemos hecho dama... borramos esa pieza del nuevo tablero.
        ;(if ?*CORONADO* then
        ;    (bind ?nuevas_aliadas (delete$ ?aliadas ?indice_ficha ?indice_ficha))
        ;else
            (bind ?nuevas_aliadas (replace$ ?aliadas ?indice_ficha ?indice_ficha ?pieza_movida))
        ;)
    else
        ; Si no... error! => salir.
        (printout t "=> Error: " ?mov " no encontrado!" crlf)
        (halt)
    )

    (bind ?lista (explode$ ?mov))
    ; Si hemos capturado... 
    (if (> (length$ ?lista) 2) then
        ; TODO: hace falta?
        ; (bind ?nuevas_enemigas ?enemigas) 
        (bind ?capturadas (subseq$ ?lista 2 (- (length$ ?lista) 1))) ; Extraer capturadas.
        (foreach ?capturada ?capturadas
            (bind ?pos_capturada (str-cat ?capturada))
            (bind ?encontrada FALSE)
            (bind ?indice_ficha 0)
            
            ; Buscamos la enemiga capturada...
            (loop-for-count (?i 1 (length$ ?nuevas_enemigas))
                (bind ?pieza (nth$ ?i ?nuevas_enemigas))
                (bind ?pos (sub-string 2 3 ?pieza))
            
                ; Pieza capturada encontrada.
                (if (eq ?pos_capturada ?pos) then
                    (bind ?encontrada TRUE)
                    (bind ?indice_ficha ?i)
                    (break)
                )
            )
            
            ; Si hemos encontrado la pieza capturada... eliminarla de las nuevas enemigas.
            (if ?encontrada then
                (bind ?nuevas_enemigas (delete$ ?nuevas_enemigas ?indice_ficha ?indice_ficha))
            )
        )
    )
    
    ; Volvemos a setear los colores.
    (if ?color then
        (bind ?nuevas_blancas ?nuevas_aliadas)
        (bind ?nuevas_negras ?nuevas_enemigas)
    else
        (bind ?nuevas_negras ?nuevas_aliadas)
        (bind ?nuevas_blancas ?nuevas_enemigas)
    )

    ; Devuelve las ?nuevas_blancas | ?nuevas_negras.
    (return (str-cat (implode$ ?nuevas_blancas) "|" (implode$ ?nuevas_negras)))
)

; Funcion que aplica el movimiento y crea dos nuevos multicampo ?blancas y ?negras,
; crea un nuevo tablero a partir de estos dos nuevos vectores.
; => Devuelve el identificador del nuevo tablero.
(deffunction aplicar_movimiento (?blancas ?negras ?mov ?color)
    ; Calculamos que pasaria si aplicamos el movimiento...
    (bind ?resultado (calcular_movimiento ?blancas ?negras ?mov ?color))
    
    ; Obtener el indice del separador de las blancas y negras.
    (bind ?index_separador (str-index "|" ?resultado))
    
    ; Obtenemos las nuevas fichas despues de realizar el movimiento.
    (bind ?nuevas_blancas (explode$ (sub-string 1 (- ?index_separador 1) ?resultado)))
    (bind ?nuevas_negras (explode$ (sub-string (+ ?index_separador 1) (length ?resultado) ?resultado)))
    
    ; Creamos el tablero con las nuevas fichas.
    (if (and ?*MOV_FORZADO* (not ?*CORONADO*)) then
        ; Si hay ?*MOV_FORZADO* = TRUE, quizas se pueda seguir comiendo...
        ; Creamos un tablero temporal y seguimos investigando.
        (bind ?long (length ?mov))
        (bind ?pos_destino (sub-string (- ?long 1) ?long ?mov))
        (return (assert (tablero_tmp (blancas ?nuevas_blancas) (negras ?nuevas_negras) (pieza_a_mover ?pos_destino))))
    else
        ; No hay ?*MOV_FORZADO* = TRUE, por lo tanto... turno terminado.
        ; Introducimos el nuevo tablero.
        (cambiar_turno)
        (return (assert (tablero (blancas ?nuevas_blancas) (negras ?nuevas_negras))))
    )
)

; Funcion que calcula los movimiento de un peon.
; => Si el peon no se puede mover, devuelve multicampo vacio.
; => Si el peon no come, devuelve solo casilla destino en forma de string. 
; Ejemplo: ("22"), se mueve a (2, 2).
; => Si el peon come, devuelve la casilla comida y la casilla destino.
; Ejemplo: ("35 46"), se mueve a (4, 6) despues de comer (3, 5).
(deffunction mov_peon (?x ?y ?direccion ?atacantes ?defendientes)
    (bind ?mov (create$))
    
    ; Posiciones posibles con la direccion segun color, -1 = abajo y 1 = arriba.
    (bind ?posiciones (create$
        (sym-cat (- ?x 1) (+ ?y ?direccion))
        (sym-cat (+ ?x 1) (+ ?y ?direccion)))
    )
    
    ; Entre las posiciones posibles calculadas...
    (foreach ?pos ?posiciones
        ; Creamos ?pos_x y ?pos_y a partir de ?pos.
        (bind ?pos_x (string-to-field (sub-string 1 1 ?pos)))
        (bind ?pos_y (string-to-field (sub-string 2 2 ?pos)))
        
        ; Comprobamos si existe esa posicion en el tablero (cumple los limites).
        (if (existe_pos ?pos_x ?pos_y) then
            ; Creamos las posibles fichas que pueden estar en esa posicion,
            ; puede haber una dama o un peon.
            (bind ?posibles_piezas (create$
                (sym-cat ?*FICHA_PEON* ?pos_x ?pos_y)
                ;(sym-cat ?*FICHA_DAMA* ?pos_x ?pos_y)
                )
            )
            
            (bind ?ocupada FALSE)
            ; Hay alguna ficha defendiente entre las posibles?
            (foreach ?posible_pieza ?posibles_piezas
                (if (item_in_vector ?posible_pieza ?defendientes) then
                    (bind ?ocupada TRUE)
                    (break)
                )
            )

            ; Si hay defendiente... (comemos)
            (if ?ocupada then
                ; Miramos en la siguiente posicion.
                (bind ?dif_x (- ?pos_x ?x))
                (bind ?dif_y (- ?pos_y ?y))
                (bind ?sig_pos_x (+ ?pos_x ?dif_x))
                (bind ?sig_pos_y (+ ?pos_y ?dif_y))
                
                ; La siguiente posicion es legal? (cumple los limites).
                (if (existe_pos ?sig_pos_x ?sig_pos_y) then
                    ; Creamos las posibles fichas que pueden estar en la siguiente posicion,
                    ; puede haber una dama o un peon.
                    (bind ?sig_posibles_piezas (create$
                        (sym-cat ?*FICHA_PEON* ?sig_pos_x ?sig_pos_y)
                        ;(sym-cat ?*FICHA_DAMA* ?sig_pos_x ?sig_pos_y)
                        )
                    )

                    (bind ?sig_ocupada FALSE)
                    ; Hay alguna ficha en la posicion siguiente? (sea atacante/defendiente).
                    (foreach ?sig_posible_pieza ?sig_posibles_piezas
                        (if (or (item_in_vector ?sig_posible_pieza ?defendientes)
                                (item_in_vector ?sig_posible_pieza ?atacantes)) then
                            (bind ?sig_ocupada TRUE)
                            (break)
                        )
                    )

                    ; Si la siguiente posicion no esta ocupada...
                    (if (not ?sig_ocupada) then
                        ; Comemos ficha intermedia
                        (if (not ?*MOV_FORZADO*) then
                            ; Si los movimientos anteriores no estan forzados... vaciamos
                            ; movimientos (?mov).
                            (bind ?mov (create$))
                            (bind ?*MOV_FORZADO* TRUE)
                        )
                        (bind ?mov (append_to_vector (str-cat ?pos_x ?pos_y " " ?sig_pos_x ?sig_pos_y) ?mov))
                    ) ; Si esta ocupada... no hacemos nada.
                )
            ; No esta ocupada por defendientes... puede estar ocupada por atacante.
            else
                (bind ?ocupada FALSE)
                ; Mirar si hay alguna atacante
                (foreach ?posible_pieza ?posibles_piezas
                    (if (item_in_vector ?posible_pieza ?atacantes) then
                        (bind ?ocupada TRUE)
                        (break)
                    )
                )

                ; No hay ni defendiente ni atacante, no esta ocupada.
                (if (not ?ocupada) then
                    ; Si no hay movimiento forzado... anadimos el movimiento (normal).
                    (if (not ?*MOV_FORZADO*) then
                        (bind ?mov (append_to_vector (str-cat ?pos_x ?pos_y) ?mov))
                    )
                )
            )
        )
    )
    (return ?mov)
)

; Funcion que calcula los movimientos de las piezas,
; devuelve un multicampo con campos de la siguiente forma:
; => Strings con dos o tres valores, el primero es la posicion de la pieza origen,
; el ultimo es la posicion destino del movimiento. El central (opcional) es la posicion
; de la pieza que ha comido.
; => Ejemplo: ("11 22", "31 22", "31 42")
(deffunction movimientos (?blancas ?negras ?juegan_blancas ?pieza_a_mover)
    (bind ?*MOV_FORZADO* FALSE) ; Para saber si comemos ("forzamos").

    ; Dependiendo de quien juegue, la direccion cambia:
    ; => Blancas, hacia arriba (1).
    ; => Negras, hacia abajo (-1).
    ; Tambien cambia el modo: atancante o defendiente.
    (if ?juegan_blancas then
        (bind ?atacantes ?blancas)
        (bind ?defendientes ?negras)
        (bind ?direccion 1) ; arriba
    else
        (bind ?atacantes ?negras)
        (bind ?defendientes ?blancas)
        (bind ?direccion -1) ; abajo
    )

    (bind ?movimientos (create$))

    ; Por cada ficha atacante...
    (foreach ?pieza ?atacantes
        (if (or (not ?pieza_a_mover) (eq (sub-string 2 3 ?pieza) ?pieza_a_mover)) then
            (bind ?prev_forzado ?*MOV_FORZADO*)
            (bind ?tipo (sub-string 1 1 ?pieza))
            (bind ?x (string-to-field (sub-string 2 2 ?pieza)))
            (bind ?y (string-to-field (sub-string 3 3 ?pieza)))
            
            ; Si es peon, llamamos a mov_peon para saber los movimientos posibles.
            (bind ?mov (mov_peon ?x ?y ?direccion ?atacantes ?defendientes))
            

            ; TODO: Aqui podemos mover la dama tambien. (if (eq ?tipo ?*FICHA_DAMA*))...
            
            ; Si estamos comiendo ?*MOV_FORZADO* = TRUE, y antes no ?prev_forzado = FALSE,
            ; actualizamos ?movimientos para que este movimiento sea el unico disponible.
            ; Actualizamos tambien ?prev_forzado al valor de ?*MOV_FORZADO* (TRUE).
            (if (and ?*MOV_FORZADO* (not ?prev_forzado)) then
                (bind ?movimientos (create$))
                (bind ?prev_forzado ?*MOV_FORZADO*)
            )

            (if (eq ?prev_forzado ?*MOV_FORZADO*) then
                (foreach ?m ?mov
                    ; ?mov_completo es la concatenacion del origen y el destino (separados por un blanco).
                    (bind ?mov_completo (str-cat ?x ?y " " ?m))
                    ; Anadimos al final de ?movimiento el ?mov_completo
                    (bind ?movimientos (append_to_vector ?mov_completo ?movimientos))
                )
            )
        )
    )
    ; Devolvemos el multicampo con los movimientos completos (strings).
    (return ?movimientos)
)

; Pregunta al jugador el movimiento que quiere realizar
; devuelve una string que contiene las cordenadas de la pieza y las de
; la casilla destino.
(deffunction pedir_mov (?blancas ?negras ?juegan_blancas ?ficha_a_mover)
    ; Obtenemos los movimientos posibles de la ?ficha_a_mover,
    ; de la forma ("11 22") => primero origen, segundo destino.
    (bind ?pos_mov (movimientos ?blancas ?negras ?juegan_blancas ?ficha_a_mover))
    (while TRUE
        (print_tablero ?blancas ?negras)

        ; Mostramos los movimientos posibles (origen)
        (bind ?escritos (create$))
        (foreach ?mov ?pos_mov
            (bind ?pos_origen (sub-string 1 2 ?mov))
            (if (not (item_in_vector ?pos_origen ?escritos)) then
                (printout t "| " ?pos_origen " ")
                (bind ?escritos (append_to_vector ?pos_origen ?escritos))
            )
        )

        ; Se pide la ficha a mover (origen)
        (printout t crlf)
        (printout t "=> Que ficha quieres mover? xy: ")
        (bind ?pieza (str-cat (read)))

        ; DEBUG => TODO: UNAI!!! borrar al acabar
        (if (eq ?pieza "q") then
            (assert (salir))
            (return)
        )

        ; TODO: borrar???
        (if (eq (length ?pieza) 3) then
            (bind ?pieza (str-cat (sub-string 1 1 ?pieza) (sub-string 3 3 ?pieza)))
        )

        ; Comprobamos si la pieza introducida es una de las posibles.
        (bind ?pieza_correcta FALSE)
        (foreach ?mov ?pos_mov
            (if (eq (sub-string 1 2 ?mov) ?pieza) then ; misma posicion => entrada OK.
                (bind ?pieza_correcta TRUE)
                (break)
            )
        )

        ; Pieza (posicion) introducida es correcta.
        (if ?pieza_correcta then
            ; Mostramos posibles movimientos desde esa posicion.
            (foreach ?mov ?pos_mov
                (if (eq (sub-string 1 2 ?mov) ?pieza) then
                    (printout t "| " (sub-string (- (length ?mov) 1) (length ?mov) ?mov) " ")
                )
            )

            ; Pedimos a que posicion la quiere mover.
            (printout t crlf)
            (printout t "=> A que posicion quieres moverla? xy: ")
            (bind ?posicion (str-cat (read)))
            
            ; TODO: borrar???
            (if (eq (length ?posicion) 3) then
                (bind ?posicion (str-cat (sub-string 1 1 ?posicion) (sub-string 3 3 ?posicion)))
            )
            
            ; Comprobamos si el movimiento introducido es valido,
            ; casilla origen y destino existen en los posibles.
            ; Devolvemos la concatenacion del movimiento.
            (foreach ?mov ?pos_mov
                (bind ?long (length ?mov))
                (if (and (eq (sub-string 1 2 ?mov) ?pieza) ; misma posicion origen
                    (eq (sub-string (- ?long 1) ?long ?mov) ?posicion)) then ; misma posicion destino
                    (return (str-cat ?mov))
                )
            )
        )
    )
)

; Funcion auxiliar para el turno del jugador, llama a pedir_mov
; y guarda en ?mov el movimiento introducido.
; Aplica el movimiento introducido llamando a aplicar_movimiento.
(deffunction turno_jugador (?blancas ?negras ?color ?pieza_a_mover)
    (bind ?mov (pedir_mov ?blancas ?negras ?color ?pieza_a_mover))
    (aplicar_movimiento ?blancas ?negras ?mov ?color)
)

; Funcion auxiliar para llamar a (turno_jugador) cuando
; el turno sea del jugador (comparando ?*TURNO* con ?*COLOR_JUG*).
; => Si el turno es del jugador, llamar a turno_jugador con parametros.
; => Si el turno es del ordenador, print_tablero.
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

; Regla para saber que si hay movimientos obligatorios, dado que tablero_tmp solo se crea cuando
; se va a comer ("forzar").
(defrule turno_intermedio
    ?t <- (tablero_tmp (blancas $?b) (negras $?n) (pieza_a_mover ?p))
    =>
    ; Calculamos todos los movimientos posibles.
    (movimientos $?b $?n ?*TURNO* ?p)
    ; Si hay movimientos forzados, tomamos otro turno.
    (if ?*MOV_FORZADO* then
        (turno $?b $?n ?p)
    else ; No hay movimientos forzados, creamos tablero normal y cambiamos turno.
        (assert (tablero (blancas $?b) (negras $?n)))
        (cambiar_turno)
    )
    (retract ?t) ; Retract del tablero temporal.
)

; Regla para el turno del jugador.
(defrule turno
    ?t <- (tablero (blancas $?b) (negras $?n))
    =>
    (bind ?pos_mov (movimientos $?b $?n ?*TURNO* FALSE))
    ; No es posible ningun movimiento... fin del juego.
    (if (eq (length ?pos_mov) 0) then
        ; Comprobamos quien ha ganado...
        (bind ?damas_blancas (cuantas_damas $?b))
        (bind ?damas_negras (cuantas_damas $?n))
        
        (if (> ?damas_blancas ?damas_negras) then
            (assert(ganan_blancas))
        else
            (assert(ganan_negras))
        )
        ;(if (eq ?*TURNO* FALSE) then
        ;    (assert(ganan_blancas))
        ;else
        ;    (assert(ganan_negras))
        ;)
        (printout t "Fin del juego!" crlf )
    else
        (bind ?r (turno $?b $?n FALSE))
        ; Turno jugador completado...
        (if ?r then
            (retract ?t)
        else
            (assert (turno_ia))
            (return)
        )
    )
)

(defrule ia_movido
    ?f <- (ia_movido)
    ?t <- (tablero (blancas $?b) (negras $?n))
    =>
    (bind ?mov ?*MOV_IA*) ; Tenemos el movimiento en ?*MOV_IA*, lo dejamos en ?mov.
    (bind ?*MOV_FORZADO* FALSE)
    (bind ?*CORONADO* FALSE)
    (printout t "=> Movimiento IA: " ?mov crlf)
    (aplicar_movimiento $?b $?n ?mov (not ?*COLOR_JUG*)) ; Aplicamos movimiento.
    (bind ?*MOV_IA* FALSE) ; Reset de la variable global.
    (retract ?f) ; Eliminamos los hechos.
    (retract ?t) ; Eliminamos los hechos.
)

; ==================> PARTE DE LA IA <==================

; Representa un estado en el arbol, es final si... (su nivel == ?*MAX_PROF* y valor != FALSE).
(deftemplate estado
    (slot id) ; id del estado.
    (slot id_padre) ; id del estado padre.
    (slot nivel) ; nivel => profundidad del nodo. (Raiz en 0).
    (multislot blancas)
    (multislot negras)
    (slot movimiento) ; movimiento realizado para llegar al nodo (mov para padre => actual).
    (slot valor (default FALSE)) ; valor heuristico.
    (slot alfa (default ?*M_INF*)) ; valor alfa, para alfa-beta.
    (slot beta (default ?*INF*)) ; valor beta, para alfa-beta.
)

; Representa un estado temporal intermedio, para cuando un turno requiere mas de un
; movimiento.
(deftemplate estado_tmp
    (slot id)
    (slot id_padre)
    (slot nivel) ; nivel de los estados creados a partir de estado_tmp (el siguiente).
    (multislot blancas)
    (multislot negras)
    (slot movimiento)
    (slot pieza_a_mover)
    (slot valor (default FALSE))
)

; Representa un posible movimiento del ordenador, se guarda con el valor del heuristico.
(deftemplate posible_solucion
    (slot valor)
    (slot movimiento)
)

; Representa el control de la busqueda, tiene el nodo actual y los visitados.
(deftemplate control_busqueda
    (slot nodo_actual (default 0))
    (multislot visitados)
)

(deffunction heuristico (?blancas ?negras ?color)
    (return (random 1 1000))
)

; Funcion auxiliar para incrementar ?*CONTADOR_ID*.
(deffunction incrementar_contador ()
    (bind ?*CONTADOR_ID* (+ ?*CONTADOR_ID* 1))
    (return ?*CONTADOR_ID*)
)

; Funcion auxiliar para resetear ?*CONTADOR_ID*.
; => El valor 0, reservado para nodo raiz.
(deffunction reset_contador ()
    (bind ?*CONTADOR_ID* 1)
    (return ?*CONTADOR_ID*)
)

; Funcion auxiliar para resetear el control de la busqueda:
; => nodo_actual = 0.
; => visitados = vacio.
(deffunction reset_control_busqueda ()
    (assert (control_busqueda))
)

; Regla para hacer reset al arbol cuando se termine la busqueda.
(defrule reset_arbol
    (declare (salience 210))
    (eliminar_posibles)
    ?e <- (estado)
    =>
    (retract ?e)
)

; Regla para eliminar las posibles soluciones cuando se acaba la busqueda.
(defrule eliminar_posibles
    (declare (salience 210))
    (eliminar_posibles)
    ?f <- (posible_solucion)
    =>
    (retract ?f)
)

; Regla para determinar que la ia ha acabado.
(defrule terminado_ia
    (declare (salience 200))
    ?p <- (eliminar_posibles)
    =>
    (retract ?p)
    (assert (ia_movido))
    (return)
)

; Calcula los resultados de aplicar el movimiento y crea los hechos.
(deffunction aplicar_movimiento_ia (?blancas ?negras ?mov ?color ?id_padre ?nivel ?mov_acc)
    
    ; Calcular resultados del movimiento.
    (bind ?resultado (calcular_movimiento ?blancas ?negras ?mov ?color))

    ; Extraer fichas del resultado.
    (bind ?index_separador (str-index "|" ?resultado))
    (bind ?nuevas_blancas (explode$ (sub-string 1 (- ?index_separador 1) ?resultado)))
    (bind ?nuevas_negras (explode$ (sub-string (+ ?index_separador 1) (length ?resultado) ?resultado)))

    ; Creamos un nuevo id.
    (bind ?id ?*CONTADOR_ID*)
    (incrementar_contador)

    ; Hay movimiento parcial anterior.
    (if ?mov_acc then
        (bind ?movimiento (str-cat (sub-string 1 (- (length ?mov_acc) 3) ?mov_acc) (sub-string 3 (length ?mov) ?mov)))
    ; No hay movimiento parcial anterior.
    else
        (bind ?movimiento ?mov)
    )
    
    ; Si alguno de los lados no tiene PEONES, estado final, add heuristico.
    (if (or (= (cuantas_peones ?nuevas_blancas) 0) (= (cuantas_peones ?nuevas_negras) 0)) then
        (bind ?heur (heuristico ?nuevas_blancas ?nuevas_negras (not ?*COLOR_JUG*)))

        (return (assert (estado (id ?id) (id_padre ?id_padre) (nivel ?nivel) (valor ?heur)
                (blancas ?nuevas_blancas) (negras ?nuevas_negras) (movimiento ?movimiento))))
    ; Si movimiento forzado... puede haber mas capturas, crear estado_tmp para buscar.
    else (if (and ?*MOV_FORZADO* (not ?*CORONADO*)) then
        (bind ?long (length ?mov))
        (bind ?pos_destino (sub-string (- ?long 1) ?long ?mov))
        (return (assert (estado_tmp (id ?id_padre) (id_padre ?id_padre) (nivel ?nivel)
                (blancas ?nuevas_blancas) (negras ?nuevas_negras) (pieza_a_mover ?pos_destino) (movimiento ?movimiento))))
    ; Turno normal.
    else
        ; Profundidad maxima alcanzada... estado final, anadir heuristico.
        (if (= ?nivel ?*MAX_PROF*) then
            (bind ?heur (heuristico ?nuevas_blancas ?nuevas_negras (not ?*COLOR_JUG*)))
        else
            (bind ?heur FALSE)
        )

        (return (assert (estado (id ?id) (id_padre ?id_padre) (nivel ?nivel) (valor ?heur)
                (blancas ?nuevas_blancas) (negras ?nuevas_negras) (movimiento ?movimiento))))
    ))
)

; Regla para determinar que el árbol ha terminado de crearse.
(defrule arbol_creado
    (declare (salience 10))
    (not (recorrer_arbol))
    (not (eliminar_posibles))

    ; Hay estado final.
    (estado (nivel ?n) (valor ?valor))
    (test (not (eq ?valor FALSE )))
    =>
    ; Creamos el control para busqueda e insertamos hecho para recorrer el arbol.
    (reset_control_busqueda)
    (assert (recorrer_arbol))
)

; Regla para inicio de la IA.
(defrule inicio_ia
    ?tt <- (turno_ia)
    ?t <- (tablero (blancas $?blancas) (negras $?negras))
    =>
    ; Calcular posibles movimientos.
    (bind ?movimientos (movimientos $?blancas $?negras (not ?*COLOR_JUG*) FALSE))

    ; Solo un movimiento posible?
    ; => Evitar realizar la busqueda...
    (bind ?unica_posib FALSE)
    (bind ?buscar TRUE)
    (bind $?nuevas_blancas $?blancas)
    (bind $?nuevas_negras $?negras)

    ; Mientras solo un movimiento sea posible...
    (while (and (= 1 (length$ ?movimientos)) ?buscar)
        (bind ?mov (nth$ 1 ?movimientos))
        (if (not ?unica_posib) then
            (bind ?unica_posib ?mov)
        )

        ; Se puede continuar el movimiento capturando piezas, anadir la ultima captura
        ; al movimiento (hasta ahora).
        (if (and ?*MOV_FORZADO* (not ?*CORONADO*)) then
            (if (not (eq ?unica_posib ?mov)) then
                (bind ?unica_posib (str-cat (sub-string 1 (- (length ?unica_posib) 3) ?unica_posib) (sub-string 3 (length ?mov) ?mov)))
            )

            ; Calcular nuevas posiciones y movimientos.
            (bind ?pieza (sub-string (- (length ?mov) 1) (length ?mov) ?mov))
            (bind ?res (calcular_movimiento $?nuevas_blancas $?nuevas_negras ?mov (not ?*COLOR_JUG*)))
            (bind ?index_separador (str-index "|" ?res))
            (bind $?nuevas_blancas (explode$ (sub-string 1 (- ?index_separador 1) ?res)))
            (bind $?nuevas_negras (explode$ (sub-string (+ ?index_separador 1) (length ?res) ?res)))
            (bind ?movimientos (movimientos $?nuevas_blancas $?nuevas_negras (not ?*COLOR_JUG*) ?pieza))
        ; Siguiente movimiento es normal, el movimiento realizado es la unica posibilidad y no hace falta arbol.
        else
            (bind ?buscar FALSE)
        )
    )

    ; Mas de un movimiento encontrado...
    (if ?unica_posib then
        ; Solo buscamos si ?*MOV_FORZADO* y no ?*CORONADO*.
        (bind ?buscar (and ?*MOV_FORZADO* (not ?*CORONADO*)))
    )

    ; Solo un movimiento... realizar movimiento y finalizar.
    (if (not ?buscar) then
        (bind ?*MOV_IA* ?unica_posib)
        (assert (eliminar_posibles))
    ; Hay que realizar la busqueda (hay IA)...
    else
        (bind ?num_piezas (+ (length$ $?blancas) (length $?negras)))
        ; Dependiendo del numero de piezas... cambiamos la profundidad de busqueda.
        ; TODO: cambiar para mas valores? PROBAR!
        (if (>= ?num_piezas 10) then
            (bind ?*MAX_PROF* 4)
        else
            (bind ?*MAX_PROF* 6)
        )

        (printout t "=> Profundidad: " ?*MAX_PROF* crlf)
        
        ; Creamos el nodo raiz.
        (assert (estado (id 0) (id_padre FALSE) (nivel 0) (blancas $?blancas) (negras $?negras) (movimiento FALSE)))
        
        ; Resetear el contador de ids.
        (reset_contador)
    )
    (retract ?tt)
)

; Regla para crear nodos del árbol.
(defrule crear_arbol
    (declare (salience 30))
    (not (recorrer_arbol))
    (not (eliminar_posibles))

    ; Si hay estado no final...
    ; => No esta en ?*PROF_MAX*, se compara nivel.
    ; => No tiene valor heuristico.
    ?e <- (estado (id ?id) (nivel ?n) (blancas $?blancas) (negras $?negras) (valor ?valor))
    (test (and (< ?n ?*MAX_PROF*) (not ?valor)))
    =>
    (bind ?nuevo_nivel (+ ?n 1))
    
    ; Dependiendo del nivel... el siguiente nivel es IA o jugador.
    (if (= 0 (mod ?nuevo_nivel 2)) then
        (bind ?color ?*COLOR_JUG*)
    else
        (bind ?color (not ?*COLOR_JUG*))
    )

    ; Calculamos los movimientos.
    (bind ?movimientos (movimientos $?blancas $?negras ?color FALSE))
    
    ; Por cada movimiento... creamos nodo del arbol.
    (foreach ?mov ?movimientos
        (aplicar_movimiento_ia $?blancas $?negras ?mov ?color ?id ?nuevo_nivel FALSE)
    )
)

; Regla para crear nodos a partir de estados intermedios.
(defrule continuar_mov
    (declare (salience 35))
    (not (recorrer_arbol))
    (not (eliminar_posibles))

    ; Si existe un estado_tmp y nivel es menor o igual a ?*MAX_PROF*...
    ?e <- (estado_tmp (id ?id) (id_padre ?id_padre) (nivel ?nivel) (blancas $?blancas)
                    (negras $?negras) (pieza_a_mover ?pieza) (movimiento ?movimiento))
    (test (<= ?nivel ?*MAX_PROF*))
    =>
    ; Miramos si es movimiento de IA o jugador.
    (if (= 0 (mod ?nivel 2)) then
        (bind ?color ?*COLOR_JUG*)
    else
        (bind ?color (not ?*COLOR_JUG*))
    )

    ; Calculamos todos los movimientos.
    (bind ?movimientos (movimientos $?blancas $?negras ?color ?pieza))
    
    ; Si hay movimientos forzados... crear los nuevos estados.
    (if ?*MOV_FORZADO* then
        (foreach ?mov ?movimientos
            (aplicar_movimiento_ia $?blancas $?negras ?mov ?color ?id ?nivel ?movimiento)
        )
    ; Si no hay movimientos forzados... crear tablero normal. 
    else
        (bind ?movimientos_opp (movimientos $?blancas $?negras (not ?color) FALSE))
        
        ; Si estamos en profundidad maxima... calcular heuristico.
        (if (or (= ?nivel ?*MAX_PROF*) (eq 0 (length$ ?movimientos_opp))) then
            (bind ?heur (heuristico $?blancas $?negras (not ?*COLOR_JUG*)))
        else
            (bind ?heur FALSE)
        )
        (assert (estado (id ?*CONTADOR_ID*) (id_padre ?id_padre) (nivel ?nivel)
                (blancas $?blancas) (negras $?negras) (valor ?heur) (movimiento ?movimiento)))
        (incrementar_contador)
    )

    ; Eliminar el estado_tmp
    (retract ?e)
)

; Regla para explorar arbol "hacia abajo".
(defrule bajar
    (declare (salience 110))
    (recorrer_arbol)

    ?control <- (control_busqueda (nodo_actual ?nodo_actual) (visitados $?visitados))
    ?actual <- (estado (id ?id_a) (alfa ?alfa) (beta ?beta))
    ?hijo <- (estado (id ?id_h) (id_padre ?id_padre) (nivel ?nivel_h))
    
    ; Nodo actual tiene un hijo no visitado.
    (test (and (eq ?id_padre ?id_a ?nodo_actual) (not (item_in_vector ?id_h $?visitados))))
    =>
    ; Si el hijo no visitado no esta en la profundida maxima... hacer nodo actual.
    (if (not (eq ?nivel_h ?*MAX_PROF*)) then
        (bind $?visitados (append_to_vector ?id_h $?visitados))
        (modify ?control (nodo_actual ?id_h) (visitados ?visitados))

        ; Setear ?alf y ?beta del padre.
        (modify ?hijo (alfa ?alfa) (beta ?beta))
    )
)

; Regla para "subir" el valor de un nodo a su padre ("propagar").
(defrule subir
    (declare (salience 120))
    (recorrer_arbol)

    ?control <- (control_busqueda (nodo_actual ?nodo_actual) (visitados $?visitados))
    ?actual <- (estado (id ?id_a) (id_padre ?id_abuelo) (nivel ?nivel_a) (valor ?valor_a)
        (alfa ?alfa_a) (beta ?beta_a))
    ?hijo <- (estado (id ?id_h) (id_padre ?id_padre) (nivel ?nivel_h) (valor ?valor_h) 
        (movimiento ?mov) (alfa ?alfa_h) (beta ?beta_h))
    
    ; Nodo actual con hijo con valor == FALSE.
    (test (not (eq ?valor_h FALSE)))
    (test (eq ?id_padre ?id_a ?nodo_actual))
    =>
    ; Para saber si es nodo min/max.
    (bind ?max (= 0 (mod ?nivel_a 2)))

    ; Nodo actual es MAX.
    (if ?max then
        ; Setear a valor por defecto, -INF.
        (if (not ?valor_a) then
            (bind ?valor_a ?*M_INF*)
        )
        
        ; Nuevo valor:
        ; => Max (valor_previo, valor_hijo).
        (bind ?nuevo_valor_a (max ?valor_a ?valor_h))

        ; Nuevo valor Alfa:
        ; => Max (valor_previo_alfa, nuevo_valor_nodo).
        (bind ?nuevo_alfa_a (max ?alfa_a ?nuevo_valor_a))

        ; Nuevo valor Beta:
        ; => Se mantiene igual.
        (bind ?nuevo_beta_a ?beta_a)
    ; Nodo actual es MIN.    
    else
        ; Setear a valor por defecto, INF.
        (if (not ?valor_a) then
            (bind ?valor_a ?*INF*)
        )

        ; Nuevo valor:
        ; => Min (valor_previo, valor_hijo).
        (bind ?nuevo_valor_a (min ?valor_a ?valor_h))

        ; Nuevo valor Alfa:
        ; => Se mantiene igual.
        (bind ?nuevo_alfa_a ?alfa_a)

        ; Nuevo valor Beta:
        ; => Min (valor_previo_beta, nuevo_valor_nodo).
        (bind ?nuevo_beta_a (min ?beta_a ?nuevo_valor_a))
    )

    ; Modificamos los valores.
    (modify ?actual (valor ?nuevo_valor_a) (alfa ?nuevo_alfa_a) (beta ?nuevo_beta_a))

    ; Si Alfa > Beta:
    ; => Podamos, omitimos resto de hijos y el padre del actual es nuevo actual.
    (if (> ?nuevo_alfa_a ?nuevo_beta_a) then
        (printout t "=> PODANDO... " ?nuevo_alfa_a " " ?nuevo_beta_a)
        (bind ?nodo_actual ?id_abuelo)
        (if (not ?nodo_actual) then
            (bind ?nodo_actual 0)
        )
        (modify ?control (nodo_actual ?nodo_actual))
    )

    ; Si Nodo actual es raiz:
    ; => Crear una posible solucion con valor y movimiento del hijo.
    (if (= 0 ?id_a) then   
        (assert (posible_solucion (valor ?valor_h) (movimiento ?mov)))
    )

    ; Eliminar hijo.
    (retract ?hijo)
)

; Regla para "subir" el puntero al nodo actual.
(defrule subir_nodo_actual
    (declare (salience 100))
    ?f <- (recorrer_arbol)

    ?control <- (control_busqueda (nodo_actual ?nodo_actual))
    ?actual <- (estado (id ?id_a) (id_padre ?id_padre) (nivel ?nivel) (valor ?valor)
                (alfa ?alfa_a) (beta ?beta_a))
    
    ; Nodo actual no es nodo raiz.
    (test (eq ?id_a ?nodo_actual))
    (test (not (eq ?id_padre FALSE)))
    =>
    (bind ?nodo_actual ?id_padre)
    
    ; El padre del actual se convierte en actual.
    (modify ?control (nodo_actual ?nodo_actual))
)

; Regla cuando se ha terminado la búsqueda.
(defrule fin_busqueda
    (declare (salience 90))
    ; Estamos recorriendo el arbol.
    ?f <- (recorrer_arbol)

    ; Nodo actual == nodo raiz.
    ?control <- (control_busqueda (nodo_actual 0) (visitados $?visitados))
    ?origen <- (estado (id 0) (valor ?valor_final))
    ?solucion <- (posible_solucion (valor ?valor) (movimiento ?mov))
    
    ; Una posible solucion con mismo valor que el nodo raiz.
    (test (eq ?valor ?valor_final))
    =>
    ; Guardamos el movimiento actual.
    (bind ?*MOV_IA* ?mov)

    ; Terminamos busqueda y eliminamos posibles soluciones.
    (retract ?f)
    (retract ?control)
    (assert (eliminar_posibles))
)

; ==================> PARTE INICIAL DEL JUEGO Y REGLAS GLOBALES <==================

; Funcion auxiiar para crear el tablero inicial.
; => ?lineas se refiere al numero de lineas con fichas inicial. Es decir, 
; para un tablero de DIM=4... (4/2)-1 = 1 linea con fichas para cada color.
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
    
    ; Cambiar las fichas de strings multicampos
    (bind ?negras (explode$ ?negras))
    (bind ?blancas (explode$ ?blancas))

    (printout t "====> POSICIONES INICIALES <==== " crlf)
    (printout t "=> Negras: " ?negras crlf)
    (printout t "=> Blancas: " ?blancas crlf)

    (assert(tablero (blancas ?blancas) (negras ?negras))) ; Introducimos tablero inicial
    (printout t "====> TABLERO INICIAL <====" crlf)
    (print_tablero ?blancas ?negras) ; Mostramos tablero inicial
    (printout t "===========================" crlf)
)

; Regla que inicia el tablero cuando se detecta (init_global),
; este hecho se introduce en la regla pedir_param, despues de 
; llamar a su funcion. Se elimina el hecho (init_global) y se
; llama a la funcion crear_tablero.
(defrule iniciar_tablero
    ?f <- (init_global)
    =>
    (retract ?f)
    (crear_tablero)
)

; TODO: cambiar salience y orden de las reglas???
(defrule ganan_blancas
    (declare(salience 101))
    ?w <- (tablero (blancas $?b) (negras $?n))
    (ganan_blancas)
    =>
    (assert(fin_juego))
    (print_tablero $?b $?n)
    (printout t "Han ganado las blancas!!!" crlf)
    (retract ?w)
)

(defrule ganan_negras
    (declare(salience 102))
    ?m <- (tablero (blancas $?b) (negras $?n))
    (ganan_negras)
    =>
    (assert(fin_juego))
    (print_tablero $?b $?n)
    (printout t "Han ganado las negras!!!" crlf)
    (retract ?m)
)

; Regla para detectar final de juego, se activa con el hecho
; (fin_juego) y para la ejecucion (halt).
(defrule fin_juego
    (fin_juego)
    =>
    (halt)
)

; Regla que se activa cuando se detecta el hecho (salir).
(defrule salir
    (salir)
    =>
    (printout t "====> ADIOS <====" crlf)
    (halt)
)

; Funcion auxiliar para pedir al juegador los parametros.
; => Setea las dimensiones el tablero ?*DIM* y el color del jugador ?*COLOR_JUG*.
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

; Cuando (inicio) exista, comenzamos el juego pidiendo parametros
; al jugador, despues activamos (init_global).
(defrule pedir_param
    ?i <- (inicio)
    =>
    (pedir_param)
    (assert (init_global))
    (retract ?i)
    (return)
)

; Se activa (inicio) para comenzar el juego
(deffacts inicializacion
    (inicio)
)