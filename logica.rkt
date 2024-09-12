#lang racket

(require racket/gui/base)

;; Proveer las funciones para que puedan ser usadas externamente
(provide
  create-matriz    ;; Crea una matriz de ceros
  print-matriz     ;; Imprime la matriz en consola
  insert-symbol     ;; Inserta un token en el tablero
  check-gane        ;; Verifica si hay un ganador
  check-tie        ;; Verifica si hay empate
  place-nuevo-elemento ;; (greedy)
)

;;; -------- Lógica del juego ---------

;; Obtiene una fila específica de una matriz
(define (get-fila row board)
  (list-ref board row) ;; Devuelve la fila en la posición indicada
)

;; Obtiene una columna específica de la matriz
(define (get-col col board)
  (cond 
    [(null? board) '()] ;; Si la matriz está vacía, devuelve una lista vacía
    [else 
      ;; Añade el elemento de la columna en la primera fila al resultado, 
      ;; luego recurre al resto de la matriz
      (cons (list-ref (car board) col) (get-col col (cdr board)))
    ]
  )
)

;; Obtiene los elementos de la diagonal derecha desde la posición (row, col)
(define (get-right-diagonal row col board)
  (define (helper row col acc)
    (cond
      [(or (>= row (length board)) 
           (>= col (length (list-ref board 0)))) 
       acc] ;; Si sale de los límites, devuelve lo acumulado
      [else 
        ;; Recurre avanzando en fila y columna, acumulando elementos
        (helper (+ 1 row) (+ 1 col) (cons (list-ref (list-ref board row) col) acc))
      ]
    )
  )
  (helper row col '()) ;; Inicializa el acumulador como una lista vacía
)

;; Obtiene los elementos de la diagonal izquierda desde la posición (row, col)
(define (get-left-diagonal row col board)
  (define (helper row col acc)
    (cond
      [(or (>= row (length board)) 
           (>= col (length (car board))) 
           (< col 0)) 
       acc] ;; Si sale de los límites, devuelve lo acumulado
      [else
        ;; Recurre avanzando en fila y retrocediendo en columna
        (helper (+ 1 row) (- col 1) (cons (list-ref (list-ref board row) col) acc))
      ]
    )
  )
  (helper row col '()) ;; Inicializa el acumulador como una lista vacía
)

;; Verifica si hay una secuencia consecutiva de 3 o más elementos iguales
(define (check-consecutive lst elem)
  (define (check-consecutive-aux count lst)
    (cond 
      ((or (null? lst) (>= count 3)) (>= count 3)) ;; Si hay 3 o más, devuelve #t
      ((equal? (car lst) elem) 
        ;; Si el elemento es igual, incrementa el contador y sigue verificando
        (check-consecutive-aux (+ 1 count) (cdr lst)))
      (else 
        ;; Reinicia el contador si no es igual
        (check-consecutive-aux 0 (cdr lst)))
    )
  )
  (check-consecutive-aux 0 lst) ;; Inicializa el contador en 0
)

;; Verifica si hay un ganador (una secuencia de 4 elementos iguales)
(define (check-gane board player)
  (define (check-row row)
    (check-consecutive (get-fila row board) player)) ;; Verifica filas
  (define (check-col col)
    (check-consecutive (get-col col board) player)) ;; Verifica columnas
  (define (check-right-diagonal row col)
    (check-consecutive (get-right-diagonal row col board) player)) ;; Verifica diagonal derecha
  (define (check-left-diagonal row col)
    (check-consecutive (get-left-diagonal row col board) player)) ;; Verifica diagonal izquierda
  (define (check-all i j)
    (cond
      [(>= i (length board)) #f] ;; Si todas las filas han sido verificadas, devuelve #f
      [(>= j (length (car board))) 
        (check-all (+ 1 i) 0)] ;; Pasa a la siguiente fila
      [(or (check-row i) 
           (check-col j) 
           (check-right-diagonal i j) 
           (check-left-diagonal i j)) 
        #t] ;; Si se encuentra una secuencia, devuelve #t
      [else (check-all i (+ 1 j))] ;; Continúa la búsqueda
    )
  )
  (check-all 0 0) ;; Inicia en la posición (0,0)
)

;; Verifica si hay un empate (es decir, si no hay celdas vacías)
(define (check-tie lst)
  (cond 
    ((null? lst) #t) ;; Si la lista está vacía, es empate
    ((member 0 (car lst)) #f) ;; Si hay algún 0, no es empate
    (else (check-tie (cdr lst)))) ;; Recurre al resto de la lista
)

;; Reemplaza un elemento en una lista por un valor dado su índice
(define (replace-ele-list pos lst val)
  (cond
    ((null? lst) '()) ;; Si la lista está vacía, devuelve una lista vacía
    ((= pos 0) (cons val (cdr lst))) ;; Si es la posición correcta, reemplaza el elemento
    (else (cons (car lst) (replace-ele-list (- pos 1) (cdr lst) val))) ;; Recurre al siguiente elemento
  )
)

;; Reemplaza un valor en una matriz dado su índice de fila y columna
(define (replace-ele-matriz i j board val)
  (replace-ele-list i board (replace-ele-list j (get-fila i board) val))
)

;; Devuelve el elemento en la posición (i,j) de una matriz
(define (get-ele-matrix i j board)
  (list-ref (list-ref board i) j)
)

;; Inserta un token en la columna dada según las reglas del juego
(define (insert-symbol row col board player)
  (if (zero? (get-ele-matrix row col board)) 
    (replace-ele-matriz row col board player) 
    board)) ;; Inserta el token si la celda está vacía

;; Crea una matriz de n filas y m columnas llena de ceros
(define (create-matriz n m)
  (define (create-row m)
    (cond 
      ((= m 0) '()) ;; Si no hay más columnas, devuelve una lista vacía
      (else (cons 0 (create-row (- m 1))))) ;; Crea una fila de ceros
  )
  (cond 
    ((= n 0) '()) ;; Si no hay más filas, devuelve una lista vacía
    (else (cons (create-row m) (create-matriz (- n 1) m)))) ;; Crea la matriz fila por fila
)

;; Imprime una matriz en consola de manera legible
(define (print-matriz matrix)
  (cond
    ((null? matrix) (newline)) ;; Si la matriz está vacía, añade un salto de línea
    (else 
      (begin
        (display (car matrix)) ;; Muestra la primera fila
        (newline) 
        (print-matriz (cdr matrix))))) ;; Recurre a las siguientes filas
)

;; Obtiene un valor específico de una lista según índices dados.
(define (getij lst index num)
  (cond
    ((= num 1)
     (car (list-ref lst index)))
    (else
     (cadr (list-ref lst index)))))

;; Función que busca un valor en una matriz dadas las coordenadas m y n.
(define (matriz-buscar m n matriz)
  (if (and (>= m 0) (< m (length matriz)))  ;; Verifica si m está dentro de los límites.
      ((lambda (row)
         (if (and (>= n 0) (< n (length row)))  ;; Verifica si n está dentro de los límites de la fila.
             (list-ref row n)  ;; Devuelve el valor en la posición n de la fila.
             (error "Column index out of bounds")))  ;; Error si el índice de columna está fuera de límites.
       (list-ref matriz m))  ;; Obtiene la fila en la posición m.
      (error "Row index out of bounds")))  ;; Error si el índice de fila está fuera de límites.

;; Función para colocar un nuevo elemento en la matriz basado en 
;; la secuencia más larga encontrada.
;; El greedy
(define (place-nuevo-elemento matriz marcador)
  ;; Verifica si las coordenadas (m, n) están dentro de los límites de la matriz.
  (define (in-bounds? m n)
    (and (>= m 0) (< m (length matriz))
         ((lambda (matrix-cols) (and (>= n 0) (< n matrix-cols)))  ;; Verifica si n está dentro de la columna.
          (length (list-ref matriz 0)))))

  ;; Encuentra los espacios adyacentes libres alrededor de (m, n).
  (define (find-adjacent-free m n)
    ((lambda (directions)
       (filter (lambda (dir)
                 ((lambda (dm dn new-m new-n)
                    (and (in-bounds? new-m new-n)  ;; Verifica si las nuevas coordenadas están dentro de los límites.
                         (equal? (matriz-buscar new-m new-n matriz) 0)))  ;; Verifica si el espacio está libre.
                  (first dir) (second dir) (+ m (first dir)) (+ n (second dir))))
               directions))  ;; Recorre las direcciones posibles.
     '((0 1) (1 0) (0 -1) (-1 0) (1 1) (-1 -1) (1 -1) (-1 1))))  ;; Lista de direcciones posibles.

  ;; Coloca el nuevo elemento en un espacio adyacente libre.
  (define (place-adjacent m n)
    ((lambda (adjacent-free)
       (printf "Adjacent free spaces for (~a, ~a): ~a\n" m n adjacent-free)  ;; Imprime los espacios libres (depuración).
       (if (null? adjacent-free)
           (begin
             (printf "No adjacent free space found for element at (~a, ~a)\n" m n)  ;; Mensaje si no hay espacios libres.
             #f)  ;; Si no hay espacios libres, devuelve falso.
           ((lambda (dir dm dn new-m new-n)
              (printf "Placing new element at (~a, ~a)\n" new-m new-n)  ;; Imprime la posición donde coloca el nuevo elemento.
              (cons new-m new-n))  ;; Devuelve la nueva posición.
            (first adjacent-free) (first (first adjacent-free))
            (second (first adjacent-free))
            (+ m (first (first adjacent-free)))
            (+ n (second (first adjacent-free))))))  ;; Calcula las nuevas coordenadas.
     (find-adjacent-free m n)))  ;; Encuentra los espacios adyacentes libres.

  ;; Definiendo una función recursiva llamada "loop" para iterar sobre la matriz.
  (define loop
    (lambda (m n)
      (cond
        ((>= m (length matriz)) #f)  ;; Si m está fuera de los límites de la matriz, detiene la búsqueda.
        ((>= n (length (list-ref matriz 0))) (loop (+ m 1) 0))  ;; Si n está fuera de los límites, pasa a la siguiente fila.
        (else
         ((lambda (current-value)
            (printf "Checking position (~a, ~a): ~a\n" m n current-value)  ;; Imprime la posición y su valor (depuración).
            (if (= current-value marcador)  ;; Si encuentra el marcador, intenta colocar el elemento adyacente.
                ((lambda (result)
                   (if result result (loop m (+ n 1))))  ;; Si encuentra un resultado válido, lo devuelve, sino continúa.
                 (place-adjacent m n))  ;; Llama a "place-adjacent" para colocar el elemento.
                (loop m (+ n 1))))  ;; Si no encuentra el marcador, sigue buscando.
          (matriz-buscar m n matriz))))))  ;; Busca el valor en la posición actual.

  ;; Comienza el bucle desde la posición inicial (0, 0).
  (loop 0 0))
