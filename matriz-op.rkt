#lang racket

; m columnas
; n filas

(define (vector-ini m miembro)
	(cond
		((zero? m) '())
		(else
			(cons miembro (vector-ini (- m 1) miembro)))
	)
)

(define (matriz-ini m n miembro)
	(cond 
		((zero? n) '())
		(else
			(cons (vector-ini m miembro) (matriz-ini m (- n 1) miembro)))
	)
)

(define (vector-buscar m vector)
	(cond
		((zero? m) (car vector))
		(else
			(vector-buscar (- m 1) (cdr vector)))
	)
)

(define (vector-marcar a m vector)
	(cond
		((>= m (length vector)) (error "Indice afuera de vector"))
		((zero? m) (cons a (cdr vector)))
		(else
			(cons (car vector) (vector-marcar a (- m 1) (cdr vector))))
	)
)

(define (matriz-marcar a m n matriz)
	(cond
		((>= n (length matriz)) (error "Indice afuera de vector de vectores"))
		((zero? n) (cons (vector-marcar a m (car matriz)) (cdr matriz)))
		(else
			(cons (car matriz) (matriz-marcar a m (- n 1) (cdr matriz))))
	)
)
