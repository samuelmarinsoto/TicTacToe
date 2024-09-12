#lang racket

(require "matriz-op.rkt")

(define (diagonal? matriz marcador m n)
	(cond
		(or
			(and
				(equal? marcador (matriz-buscar m n))
				(equal? marcador (matriz-buscar (+ m 1) (+ n 1)))
				(equal? marcador (matriz-buscar (- m 1) (- n 1)))
			)
			(and
				(equal? marcador (matriz-buscar m n))
				(equal? marcador (matriz-buscar (- m 1) (+ n 1)))
				(equal? marcador (matriz-buscar (+ m 1) (- n 1)))
			)
		)
	)
)
			
	

(define (gane? matriz marcador m n)
	(cond
		((>= m (length (car matriz)))
			(error "Indice afuera de vector"))
		((>= n (length matriz))
			(error "Indice afuera de vector de vectores"))
		(define (gane-aux? j i)
			((zero? i) #f)
			(else
				(cond
					((zero? j)
						(gane-aux? m (- i 1)))
					(else
						(diagonal? matriz marcador j i))
				)
			)
		)
		
		(gane-aux? (- m 1) (- n 1))
	)
)
	
; asume que el marcador de "vacio" es #f
(define (empate? matriz)
	(cond
		((member #f (append* matriz)) #f)
		(else #t)
	)
)

