#lang racket

;; Define a function to place an element in a given position in the matrix
    (define (matriz-poner matriz m n elemento)
      (cond
        [(>= m (length matriz)) 
         (error "Row index out of bounds.")]
        [(>= n (length (first matriz))) 
         (error "Column index out of bounds.")]
        [else 
         (let ([row (list-ref matriz m)])
           (let ([new-row (vector-poner elemento n row)])
             (append (take matriz m) 
                     (list new-row) 
                     (drop matriz (+ m 1)))))]))
    
    ;; Define a function to update a specific index in a vector (list)
    (define (vector-poner elemento n vector)
      (cond
        [(zero? n) (cons elemento (cdr vector))]
        [(>= n (length vector)) (error "Index out of bounds.")]
        [else (cons (car vector) (vector-poner elemento (- n 1) (cdr vector)))]))
        
;; Function to retrieve an element at position (m, n) in the matrix
(define (matriz-buscar m n matriz)
    (if (and (>= m 0) (< m (length matriz)))
        (let ((row (list-ref matriz m)))
          (if (and (>= n 0) (< n (length row)))
              (list-ref row n)
              (error "Column index out of bounds")))
        (error "Row index out of bounds")))
  
;; Function to find a free spot adjacent to a given position and place the element
(define (place-adjacent matriz marcador m n)
  (cond
    ;; Check above
    [(and (> n 0) (equal? (matriz-buscar matriz (- m 1) n) #f))
     (set! matriz (replace-matrix-element matriz (- m 1) n marcador))]
    
    ;; Check below
    [(and (< n (- (length matriz) 1)) (equal? (matriz-buscar matriz (+ m 1) n) #f))
     (set! matriz (replace-matrix-element matriz (+ m 1) n marcador))]

    ;; Check left
    [(and (> m 0) (equal? (matriz-buscar matriz m (- n 1)) #f))
     (set! matriz (replace-matrix-element matriz m (- n 1) marcador))]

    ;; Check right
    [(and (< m (- (length (first matriz)) 1)) (equal? (matriz-buscar matriz m (+ n 1)) #f))
     (set! matriz (replace-matrix-element matriz m (+ n 1) marcador))]

    ;; If no adjacent free spots, return the matrix unchanged
    [else matriz]))

;; Helper function to replace an element at position (m, n) in the matrix
(define (replace-matrix-element matriz m n nuevo-valor)
  (map (lambda (fila i)
         (if (= i m)
             (map (lambda (elem j)
                    (if (= j n) nuevo-valor elem))
                  fila (build-list (length fila) identity))
             fila))
       matriz (build-list (length matriz) identity)))

;; Find the longest horizontal sequence
(define (find-sequence-for-row matriz marcador row)
  (let loop ((col 0) (max-length 0) (current-length 0))
    (cond
      [(= col (length (first matriz))) max-length]
      [(equal? (matriz-buscar matriz row col) marcador)
       (loop (+ col 1) (max max-length (+ current-length 1)) (+ current-length 1))]
      [else
       (loop (+ col 1) max-length 0)])))

;; Find the longest vertical sequence
(define (find-sequence-for-col matriz marcador col)
  (let loop ((row 0) (max-length 0) (current-length 0))
    (cond
      [(= row (length matriz)) max-length]
      [(equal? (matriz-buscar matriz row col) marcador)
       (loop (+ row 1) (max max-length (+ current-length 1)) (+ current-length 1))]
      [else
       (loop (+ row 1) max-length 0)])))

;; Find the longest diagonal sequence
(define (find-sequence-for-diag matriz marcador)
  (let loop ((row 0) (col 0) (max-length 0) (current-length 0))
    (cond
      [(or (= row (length matriz)) (= col (length (first matriz)))) max-length]
      [(equal? (matriz-buscar matriz row col) marcador)
       (loop (+ row 1) (+ col 1) (max max-length (+ current-length 1)) (+ current-length 1))]
      [else
       (loop (+ row 1) (+ col 1) max-length 0)])))

;; Function to find the longest sequence in any direction
(define (find-longest-sequence matriz marcador)
  (let* ((num-rows (length matriz))
         (num-cols (length (first matriz)))
         (max-horizontal (apply max (map (lambda (row) (find-sequence-for-row matriz marcador row)) (range 0 num-rows))))
         (max-vertical (apply max (map (lambda (col) (find-sequence-for-col matriz marcador col)) (range 0 num-cols))))
         (max-diagonal (find-sequence-for-diag matriz marcador)))
    (max max-horizontal max-vertical max-diagonal)))

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
                                    
;; Helper function to replace element in matrix
(define (matriz-replace matriz row col value)
  (let ((new-row (vector-poner value col (list-ref matriz row))))
    (vector-poner new-row row matriz)))

;; Example usage:
;; Initialize a matrix (replace with actual initialization)
(define matriz '((#f #f #f #f) (o x o #f) (o x #f #f) (#f #f #f #f)))

;; Place a new element 'x'
(define updated-matriz (place-nuevo-elemento matriz 'o))
(displayln updated-matriz)
