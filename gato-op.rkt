#lang racket

(require "matriz-op.rkt")

(define (diagonal? matriz marcador m n)
	(or
		(and ; soroeste-noreste
			(equal? marcador (matriz-buscar m n matriz))
			(equal? marcador (matriz-buscar (+ m 1) (+ n 1) matriz))
			(equal? marcador (matriz-buscar (- m 1) (- n 1) matriz))
		)
		(and ; noroeste-sureste
			(equal? marcador (matriz-buscar m n matriz))
			(equal? marcador (matriz-buscar (- m 1) (+ n 1) matriz))
			(equal? marcador (matriz-buscar (+ m 1) (- n 1) matriz))
		)
	)
)

(define (gane? matriz marcador m n)
	(cond
		((> m (length (car matriz)))
			(error "Indice afuera de vector"))
		((> n (length matriz))
			(error "Indice afuera de vector de vectores"))
		(else
			(define (gane-diag? j i)
				(cond
					((zero? i) #f)
					(else
						(cond
							((zero? j)
								(gane-diag? m (- i 1)))
							(else
								(diagonal? matriz marcador j i))
						)
					)
				)
			)

			(gane-diag? (- m 2) (- n 2))
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

