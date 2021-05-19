; La duración media de las actividades de Ana fue 4.0 
; La duración media de las actividades de Juan fue 3.0 

(deftemplate persona
	(slot nombre)
	(slot ciudad)
)

(deftemplate actividad
	(slot nombre)
	(slot ciudad)
	(slot duracion)
)

(deftemplate duracion_persona
	(slot duracion_media)
	(slot nombre)
)

(defrule media
	?p <- (persona (nombre ?nom) (ciudad ?ciu))
	=>
	(bind ?n 0)
	(bind ?suma 0)
	(do-for-all-facts ((?ac actividad)) (eq ?ac:ciudad ?ciu)
		(bind ?n (+ ?n 1))
		(bind ?suma (+ ?suma ?ac:duracion))
	)
	(assert (duracion_persona (duracion_media (/ ?suma ?n)) (nombre ?nom)))
	(retract ?p)
)

(defrule imprimir
	(not (persona (nombre ?n) (ciudad ?c)))
	?media <- (duracion_persona (duracion_media ?d_m) (nombre ?nomb))
	=>
	(printout t "La duracion media de las actividades de " ?nomb " fue " ?d_m crlf)
	(retract ?media)
)

(deffacts personas 
	(persona (nombre Juan) (ciudad Paris)) 
	(persona (nombre Ana) (ciudad Edimburgo))) 

(deffacts actividades 
	(actividad (nombre Torre_Eiffel) (ciudad Paris) (duracion 2)) 
	(actividad (nombre Castillo_de_Edimburgo) (ciudad Edimburgo) (duracion 5)) 
	(actividad (nombre Louvre) (ciudad Paris) (duracion 6)) 
	(actividad (nombre Montmartre) (ciudad Paris) (duracion 1)) 
	(actividad (nombre Royal_Mile) (ciudad Edimburgo) (duracion 3))) 
