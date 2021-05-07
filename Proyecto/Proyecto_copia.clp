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
)

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
        ; Reemplazamos la pieza original por la movida ("la nueva").
        (bind ?nuevas_aliadas (replace$ ?aliadas ?indice_ficha ?indice_ficha ?pieza_movida))
    else
        ; Si no... error! => salir.
        (printout t "=> Error: " ?mov " no encontrado!" crlf)
        (halt)
    )

    (bind ?lista (explode$ ?mov))
    ; Si hemos capturado... 
    (if (> (length$ ?lista) 2) then
        (bind ?nuevas_enemigas ?enemigas) ; TODO: hace falta?
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
                (sym-cat ?*FICHA_DAMA* ?pos_x ?pos_y))
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
                        (sym-cat ?*FICHA_DAMA* ?sig_pos_x ?sig_pos_y))
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
            (if (eq ?tipo ?*FICHA_PEON*) then
                (bind ?mov (mov_peon ?x ?y ?direccion ?atacantes ?defendientes))
            )

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
        (printout t "Que ficha quieres mover? xy: ")
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
            (printout t "A que posicion quieres moverla? xy: ")
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

(defrule turno
    ?t <- (tablero (blancas $?b) (negras $?n))
    =>
    (bind ?pos_mov (movimientos $?b $?n ?*TURNO* FALSE))
    ; No es posible ningun movimiento... fin del juego.
    (if (eq (length ?pos_mov) 0) then
        ; Comprobamos quien ha ganado...
        (if (eq ?*TURNO* FALSE) then
            (assert(ganaronblancas))
        else
            (assert(ganaronnegras))
    )
    (printout t "Fin del juego!" crlf )
    else
        (bind ?r (turno $?b $?n FALSE))
        ; Turno jugador completado...
        (if ?r then
            (retract ?t)
        else
            (printout t "TURNO DE LA IA...!!!" crlf)
            (return)
        )
    )
)

; TODO: cambiar salience y orden de las reglas???
(defrule ganaronblancas
    (declare(salience 101))
    ?w <- (tablero (blancas $?b) (negras $?n))
    (ganaronblancas)
    =>
    (assert(findejuego))
    (print_tablero $?b $?n)
    (printout t "Han ganado las blancas!!!" crlf)
    (retract ?w)
)

(defrule ganaronnegras
    (declare(salience 102))
    ?m <- (tablero (blancas $?b) (negras $?n))
    (ganaronnegras)
    =>
    (assert(findejuego))
    (print_tablero $?b $?n)
    (printout t "Han ganado las negras!!!" crlf)
    (retract ?m)
)


; ==========> Parte inicial del juego <==========

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
    (print_tablero ?blancas ?negras) ; Mostramos tablero inicial
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
    (inicio)
    =>
    (pedir_param)
    (assert (init_global))
    (return)
)

; Se activa (inicio) para comenzar el juego
(deffacts inicializacion
    (inicio)
)