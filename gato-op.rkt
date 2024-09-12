#lang racket

(require "matriz-op.rkt")

;(define (diag-no? matriz marcador m n)
;	((equal? marcador (matriz-buscar m n) #t)
		
(define (diagonal? matriz marcador m n)
	(cond
		((>= m (length (car matriz)))
			(error "Indice afuera de vector"))
		((>= n (length matriz))
			(error "Indice afuera de vector de vectores"))
		(else
			(or
				(diag-no? matriz marcador m n) ; por definir
				(diag-ne? matriz marcador m n)
				(diag-so? matriz marcador m n)
				(diag-se? matriz marcador m n)
			)
		)
	)
)

; asume que el marcador de "vacio" es #f
(define (empate? matriz)
	(cond
		((member #f (append* matriz)) #f)
		(else #t)
	)
)

