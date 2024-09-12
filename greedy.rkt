#lang racket
      
;; Function to retrieve an element at position (m, n) in the matrix
(define (matriz-buscar m n matriz)
	(if (and (>= m 0) (< m (length matriz)))
		(let ((row (list-ref matriz m)))
			(if (and (>= n 0) (< n (length row)))
				(list-ref row n)
				(error "Column index out of bounds")))
		(error "Row index out of bounds")))

;; Function to place the new element based on the longest sequence found
(define (place-nuevo-elemento matriz marcador)
	(define (in-bounds? m n)
		(and (>= m 0) (< m (length matriz))
			(let ((matrix-cols (length (list-ref matriz 0))))
				(and (>= n 0) (< n matrix-cols)))))

	(define (find-adjacent-free m n)
		(let ((directions '((0 1) (1 0) (0 -1) (-1 0) (1 1) (-1 -1) (1 -1) (-1 1))))
			(filter (lambda (dir)
				(let* ((dm (first dir))
					   (dn (second dir))
					   (new-m (+ m dm))
					   (new-n (+ n dn)))
					(and (in-bounds? new-m new-n)
						(equal? (matriz-buscar new-m new-n matriz) #f))))
				directions)))

	(define (place-adjacent m n)
		(let ((adjacent-free (find-adjacent-free m n)))
			(if (null? adjacent-free)
				(begin
					(printf "No adjacent free space found for element at (~a, ~a)\n" m n)
					#f)
				(let* ((dir (first adjacent-free))
					   (dm (first dir))
					   (dn (second dir))
					   (new-m (+ m dm))
					   (new-n (+ n dn)))
					(printf "Placing new element at (~a, ~a)\n" new-m new-n)
					(cons new-m new-n))))) ; Return coordinates as a pair

	(let loop ((m 0) (n 0))
		(cond
			((>= m (length matriz)) #f)
			((>= n (length (list-ref matriz 0))) (loop (+ m 1) 0))
			((equal? (matriz-buscar m n matriz) marcador)
				(let ((result (place-adjacent m n)))
					(if result
						result
						(loop m (+ n 1)))))
			(else (loop m (+ n 1))))))

                                    
;; Example usage:
;; Initialize a matrix (replace with actual initialization)
;(define matriz '((#f #f #f #f) (o x o #f) (o x #f #f) (#f #f #f #f)))

;; Place a new element 'x'
;(define updated-matriz (place-nuevo-elemento matriz 'o))
;(displayln updated-matriz)
