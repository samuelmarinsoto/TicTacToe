#lang racket

(require "matriz-op.rkt")

(define (diag-no? matriz marcador m n)
	(cond
		(cond
			((> n m)
				(diag-no? matriz marcador m m))
			((> m n)
				(diag-no? matriz marcador n n))
			(else
				((zero? m) #t)
				(else
					(cond
						((not (equal? marcador (matriz-buscar m m))) 
							#f)
						(else
							(diag-no? matriz marcador (- m 1) (- m 1)))
					)
				)
			)				
		)		
	)
)

(define (diag-ne? matriz marcador m n)
	(define (diag-ne-aux? j i)
		(cond
			((= i n) #t)
			((< j 0) #t)
			(else
				(cond
					((not (equal? marcador (matriz-buscar j i)))
						#f)
					(else
						(diag-ne-aux (- j 1) (+ i 1)))
				)
			)
		)
	)

	(diag-ne-aux? (- m 1) 0)
)

(define (diagonal? matriz marcador m n)
	(cond
		((>= m (length (car matriz)))
			(error "Indice afuera de vector"))
		((>= n (length matriz))
			(error "Indice afuera de vector de vectores"))
		(else
			(or
				(diag-no? matriz marcador m n)
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

