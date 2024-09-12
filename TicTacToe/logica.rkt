#lang racket

(require racket/gui/base)

(provide
         create-matrix
         print-matrix
         insert-token
         check-win
         check-tie
         greedy
         place-nuevo-elemento
)

;;; -------- Logica del juego ---------

;; Esta funcion obtiene una fila especifica de una matriz.
  ;;
  ;; Entradas:
  ;; - row: el indice de la fila que se quiere obtener
  ;; - board: la matriz representada como una lista de listas
  ;;
  ;; Retorna:
  ;; - una lista que representa la fila especifica de la matriz
(define (get-row row board)
    (list-ref board row)
)

;; Obtiene los elementos de una columna especifica en un tablero
  ;;
  ;; Entradas:
  ;; - col: numero de columna (indice iniciando en 0)
  ;; - board: lista de listas que representa el tablero
  ;;
  ;; Retorna:
  ;; - Lista con los elementos en la columna especificada
(define (get-col col board)
  (cond 
  [(null? board) 
    '()
  ]
  [else 
    (cons (list-ref (car board) col) 
          (get-col col (cdr board)))
  ]
  )
)


;; Obtiene los elementos de la diagonal derecha secundaria
    ;;  que pasa por la posicion (row, col)
    ;;
    ;; Entradas:
    ;; - row: indice de fila de la posicion inicial (iniciando en 0)
    ;; - col: indice de columna de la posicion inicial (iniciando en 0)
    ;; - board: una lista de listas que representa el tablero
    ;;
    ;; Retorna:
    ;; - Una lista con los elementos en la diagonal secundaria
(define (get-right-diagonal row col board)
    (define (helper row col acc)
        (cond
        [(or (>= row (length board)) 
          (>= col (length (list-ref board 0)))) 

          acc]
        [else 
            (helper 
                (+ 1 row) (+ 1 col) 
                (cons (list-ref (list-ref board row) col) acc))
        ]
        )
    )
    (helper row col '())
)


;; Obtiene los elementos de la diagonal izquierda secundaria
    ;; que pasa por la posicion (row, col) de una matriz dada.
    ;;
    ;; Entradas:
    ;; - row: indice de fila de la posicion inicial (iniciando en 0).
    ;; - col: indice de columna de la posicion inicial (iniciando en 0).
    ;; - board: una lista de listas que representa la matriz.
    ;;
    ;; Retorna:
    ;; - Una lista con los elementos en la diagonal izquierda secundaria.
(define (get-left-diagonal row col board)
  (define (helper row col acc)
    (cond
    [(or    (>= row (length board)) 
            (>= col (length (car board))) 
            (< col 0))
        acc]
    [else
    (helper (+ 1 row) 
            (- col 1) 
            (cons (list-ref (list-ref board row) col) 
        acc))
    ]))
  (helper row col '())
)

;; Esta funcion verifica si hay un numero consecutivo de elementos
  ;; iguales en una lista.
  ;;
  ;; Entradas:
  ;; - lst: una lista de elementos
  ;; - elem: el elemento que se busca contar
  ;;
  ;; Retorna:
  ;; - #t si hay al menos 4 elementos consecutivos iguales en la lista,
  ;;  #f en otro caso
(define (check-consecutive lst elem)
    (define (check-consecutive-aux count lst)
        (cond 
        ((or (null? lst)  (>= count 3))
           (>= count 3)
        )
        ((equal? (car lst) elem) 
            (check-consecutive-aux (+ 1 count) (cdr lst))
        )
        (else 
            (check-consecutive-aux 0 (cdr lst))
        ))
    )
    (check-consecutive-aux 0 lst)
)

;; Esta funcion verifica si hay una secuencia de cuatro elementos 
  ;; iguales  en una fila, columna o diagonal de una tabla de cuatro
  ;;  en linea.
  ;;
  ;; Entradas:
  ;; - board: una lista de listas que representa la tabla de cuatro 
  ;; en linea
  ;; - player: un numero que representa el jugador a verificar (1 o 2)
  ;;
  ;; Retorna:
  ;; - #t si hay una secuencia de cuatro elementos iguales del jugador
  ;; en una fila, columna o diagonal de la tabla
  ;; - #f en otro caso
(define (check-win board player)
  (define (check-row row)
    (check-consecutive (get-row row board) player))
  (define (check-col col)
    (check-consecutive (get-col col board) player))
  (define (check-right-diagonal row col)
    (check-consecutive (get-right-diagonal row col board) player))
  (define (check-left-diagonal row col)
    (check-consecutive (get-left-diagonal row col board) player))
  (define (check-all i j)
    (cond
    [(>= i (length board)) 
        #f
    ]
    [(>= j (length (car board)))
        (check-all (+ 1 i) 0 )
    ]
    [(or    (check-row i) 
            (check-col j)
            (check-right-diagonal i j)
            (check-left-diagonal i j))
        #t
    ]
    [else 
        (check-all i (+ 1 j))
    ]
    )
  )
  (check-all 0 0)
)

;; Esta funcion verifica si hay un empate en el tablero, es decir,
  ;;  si no hay 
  ;; ninguna celda vacia en la matriz
  ;;
  ;; Entradas:
  ;; - lst: una matriz que representa el tablero de juego
  ;;
  ;; Retorna:
  ;; - #t si no hay celdas vacias en el tablero (empate)
  ;; - #f en otro caso
(define (check-tie lst)
  (cond ((null? lst) #t)
        ((member 0 (car lst)) #f)
        (else (check-tie (cdr lst))))
)

;; Esta funcion reemplaza un elemento en una lista con otro valor 
  ;; dado su indice.
  ;;
  ;; Entradas:
  ;; - pos: el indice del elemento a reemplazar
  ;; - lst: la lista en la cual se quiere reemplazar el elemento
  ;; - val: el valor que se quiere colocar en la posicion dada por el 
  ;; indice
  ;;
  ;; Retorna:
  ;; - Una nueva lista donde se ha reemplazado el elemento en la 
  ;; posicion dada por el indice con el valor dado.
(define (replace-ele-list pos lst val)
  (cond
    ((null? lst) '())
    ((= pos 0) (cons val (cdr lst)))
    (else (cons (car lst)
                (replace-ele-list (- pos 1) (cdr lst) val))
          )
    )
)

;; Esta funcion reemplaza un valor en una matriz dada su posicion en
  ;; las filas y columnas.
  ;;
  ;; Entradas:
  ;; - i: la fila donde se encuentra el elemento a reemplazar
  ;; - j: la columna donde se encuentra el elemento a reemplazar
  ;; - board: la matriz en la que se desea hacer el reemplazo
  ;; - val: el valor que se quiere colocar en la posicion (i,j) de la matriz
  ;;
  ;; Retorna:
  ;; - Una nueva matriz con el elemento (i,j) reemplazado por el valor dado.
(define (replace-ele-matrix i j board val)
  (replace-ele-list i board (replace-ele-list j (get-row i board) val))
)

;; Esta funcion retorna el elemento en la posicion (i,j) de una matriz.
  ;;
  ;; Entradas:
  ;; - i: la fila donde se encuentra el elemento deseado
  ;; - j: la columna donde se encuentra el elemento deseado
  ;; - board: la matriz de la cual se quiere obtener el elemento
  ;;
  ;; Retorna:
  ;; - El elemento en la posicion (i,j) de la matriz.
(define (get-ele-matrix i j board)
  (list-ref (list-ref board i) j)
)

;; Esta funcion inserta un token en la columna dada en la matriz, de
  ;; acuerdo a la regla de un juego de cuatro en linea. El token se
  ;; inserta en la fila mas baja disponible.
  ;;
  ;; Entradas:
  ;; - col: la columna en la que se desea insertar el token
  ;; - board: la matriz que representa el tablero de juego
  ;; - player: el valor del jugador que esta insertando el token
  ;;
  ;; Retorna:
  ;; - Una nueva matriz con el token insertado en la columna dada, en 
  ;; la fila mas baja disponible.
(define (insert-token row col board player)
  (if (zero? (get-ele-matrix row col board)) 
      (replace-ele-matrix row col board player) 
      board))

;; Esta funcion crea una matriz de n filas y m columnas llena de ceros
  ;;
  ;; Entradas:
  ;; - n: numero de filas de la matriz
  ;; - m: numero de columnas de la matriz
  ;;
  ;; Retorna:
  ;; - Una matriz de n filas y m columnas llena de ceros
(define (create-matrix n m)
  (define (create-row m)
    (cond ((= m 0) '())
          (else (cons 0 (create-row (- m 1))))))

  (cond ((= n 0) '())
        (else (cons (create-row m)
                    (create-matrix (- n 1) m))))
)

;; Esta funcion imprime una matriz de manera legible en la consola
  ;;
  ;; Entradas:
  ;; - matrix: la matriz que se desea imprimir
  ;;
  ;; Retorna:
  ;; - Nada, ya que su funcion es solo imprimir en la consola
(define (print-matrix matrix)
  (cond
    ((null? matrix) (newline))
    (else (begin
            (display (car matrix))
            (newline)
            (print-matrix (cdr matrix)))))) 



;;; -------- Greddy Alorithm  ---------

;; Esta funcion verifica si el jugador tiene 3 fichas aliadas 
  ;; consecutivas en una fila horizontal hacia la izquierda, tomando 
  ;; en cuenta la posicion del tablero y los valores de los jugadores.
  ;; Entradas:
  ;; - tablero: matriz que representa el tablero del juego
  ;; - jugador: valor del jugador que se esta buscando (1 o 2)
  ;; - enem: valor del jugador enemigo (1 o 2)
  ;; - i: indice de fila actual
  ;; - j: indice de columna actual
  ;; - cont_h: contador de columna actual para moverse horizontalmente
  ;; - cont_f: contador de fichas aliadas consecutivas
  ;; - punt: contador de puntos acumulados hasta ahora
  ;;
  ;; Retorna:
  ;; - Un valor numerico que representa los puntos acumulados por el 
  ;; jugador,
  ;;  donde 1000 significa que el jugador ha ganado, 0 significa que no
  ;;  ha encontrado 3 
  ;; fichas aliadas consecutivas en esa direccion y cualquier otro 
  ;; numero positivo representa los puntos acumulados hasta ahora.
(define (ver_izq tablero jugador enem i j cont_h cont_f punt)
  (cond
    ((equal? cont_f 3) 1000) ;3 fichas aliadas alrededor, 1000 puntos ya que es gane
    ((or (< cont_h 0) (< cont_h (- j 3)))
     (ver_der tablero jugador enem i j (+ j 1) cont_f punt)) ;verifica no se haya salido del rango a verificar, pasa a verificar lado opuesto.
    ((equal? (list-ref (list-ref tablero i) cont_h) enem)
     (ver_der tablero jugador enem i j (+ j 1) cont_f (- punt 5))) ;Si se topa con ficha enemiga, resta puntos y pasa a verificar lado opuesto
    ((equal? (list-ref (list-ref tablero i) cont_h) 0)
     (ver_izq tablero jugador enem i j (- cont_h 1) cont_f (+ punt 3))) ;Topo con espacio vacio, suma puntos pero no muchos.
    (else
     (ver_izq tablero jugador enem i j (- cont_h 1) (+ cont_f 1) (+ punt 15))) ;Topo con ficha aliada, suma el doble de puntos que vacio.
    )
)

;; Esta funcion continua el trabajo de la funcion anterior hacia la 
  ;; derecha de la fila, verificando si el jugador tiene 3 fichas aliadas 
  ;; consecutivas en una fila horizontal hacia la derecha, tomando en 
  ;; cuenta la posicion del tablero y los valores de los jugadores.
  ;; Entradas:
  ;; - tablero: matriz que representa el tablero del juego
  ;; - jugador: valor del jugador que se esta buscando (1 o 2)
  ;; - enem: valor del jugador enemigo (1 o 2)
  ;; - i: indice de fila actual
  ;; - j: indice de columna actual
  ;; - cont_h: contador de columna actual para moverse horizontalmente
  ;; - cont_f: contador de fichas aliadas consecutivas
  ;; - punt: contador de puntos acumulados hasta ahora
  ;;
  ;; Retorna:
  ;; - Un valor numerico que representa los puntos acumulados por el 
  ;; jugador, donde 1000 significa que el jugador ha ganado, 0 significa
  ;;  que no ha encontrado 3 fichas aliadas consecutivas en esa 
  ;; direccion y cualquier otro numero positivo representa los puntos
  ;; acumulados hasta ahora.
(define (ver_der tablero jugador enem i j cont_h cont_f punt)
  (cond
    ((equal? cont_f 3) 1000) ;3 fichas aliadas alrededor, 1000 puntos ya que es gane
    ((or (= cont_h (length (list-ref tablero i))) (> cont_h (+ j 3))) ;verifica no se haya salido del rango a verificar
     punt)
    ((equal? (list-ref (list-ref tablero i) cont_h) enem) ;Si se topa con ficha enemiga, resta puntos y finaliza
     (- punt 5))
    ((equal? (list-ref (list-ref tablero i) cont_h) 0) ;Valor igual a 0 suma puntos, pero pocos.
     (ver_der tablero jugador enem i j (+ cont_h 1) cont_f (+ punt 3)))
    (else
     (ver_der tablero jugador enem i j (+ cont_h 1) (+ cont_f 1) (+ punt 15))) ;Ficha aliada encontrada, suma mas puntos.
    )
)

;; Esta funcion verifica si el jugador tiene 3 fichas aliadas 
  ;; consecutivas en una diagonal hacia arriba y hacia la izquierda,
  ;;  tomando en cuenta la posicion del tablero y los valores de los 
  ;; jugadores.
  ;; Entradas:
  ;; - tablero: matriz que representa el tablero del juego
  ;; - jugador: valor del jugador que se esta buscando (1 o 2)
  ;; - enem: valor del jugador enemigo (1 o 2)
  ;; - i: indice de fila actual
  ;; - j: indice de columna actual
  ;; - cont_h: contador de columna actual para moverse horizontalmente
  ;; - cont_v: contador de fila actual para moverse verticalmente
  ;; - cont_f: contador de fichas aliadas consecutivas
  ;; - punt: contador de puntos acumulados hasta ahora
  ;;
  ;; Retorna:
  ;; - Un valor numerico que representa los puntos acumulados por el 
  ;; jugador, donde 1000 significa que el jugador ha ganado, 0 significa 
  ;; que no ha encontrado 3 fichas aliadas consecutivas en esa direccion 
  ;; y cualquier otro numero positivo representa los puntos acumulados
  ;;  hasta ahora.
(define (ver_izqa tablero jugador enem i j cont_h cont_v cont_f punt)
  (cond
    ((equal? cont_f 3) 1000) ;3 fichas aliadas alrededor, 1000 puntos ya que es gane
    ((or (< cont_h 0) (< cont_h (- j 3)) (< cont_v 0) (< cont_v (- i 3)))
     (ver_derb tablero jugador enem i j (+ j 1) (+ i 1) cont_f punt)) ;verifica no se haya salido del rango a verificar, pasa a verificar lado opuesto.
    ((equal? (list-ref (list-ref tablero cont_v) cont_h) enem)
     (ver_derb tablero jugador enem i j (+ j 1) (+ i 1) cont_f (- punt 5))) ;Si se topa con ficha enemiga, resta puntos y pasa a verificar lado opuesto
    ((equal? (list-ref (list-ref tablero cont_v) cont_h) 0)
     (ver_izqa tablero jugador enem i j (- cont_h 1) (- cont_v 1) cont_f (+ punt 3))) ;Topo con espacio vacio, suma puntos pero no muchos.
    (else
     (ver_izqa tablero jugador enem i j (- cont_h 1) (- cont_v 1) 
      (+ cont_f 1) (+ punt 15))) ;Topo con ficha aliada, suma el doble de puntos que vacio.
    )
)

;; Esta funcion continua el trabajo de la funcion anterior hacia el 
  ;; lado derecho de la fila, verificando si el jugador tiene 3 fichas
  ;;  aliadas consecutivas en una fila horizontal hacia la derecha, 
  ;; tomando en cuenta la posicion del tablero y los valores de los jugadores.
  ;; Entradas:
  ;; - tablero: matriz que representa el tablero del juego
  ;; - jugador: valor del jugador que se esta buscando (1 o 2)
  ;; - enem: valor del jugador enemigo (1 o 2)
  ;; - i: indice de fila actual
  ;; - j: indice de columna actual
  ;; - cont_h: contador de columna actual para moverse horizontalmente
  ;; - cont_v: contador de fila actual para moverse verticalmente
  ;; - cont_f: contador de fichas aliadas consecutivas
  ;; - punt: contador de puntos acumulados hasta ahora
  ;;
  ;; Retorna:
  ;; - Un valor numerico que representa los puntos acumulados por el 
  ;; jugador, donde 1000 significa que el jugador ha ganado, 0 
  ;; significa que no ha encontrado 3 fichas aliadas consecutivas en 
  ;; esa direccion y cualquier otro numero positivo representa los 
  ;; puntos acumulados hasta ahora.
(define (ver_derb tablero jugador enem i j cont_h cont_v cont_f punt)
  (cond
    ((equal? cont_f 3) 1000) ;3 fichas aliadas alrededor, 1000 puntos ya que es gane
    ((or (= cont_h (length (list-ref tablero i))) 
      (> cont_h (+ j 3)) 
      (= cont_v (length tablero)) (> cont_v (+ i 3))) ;verifica no se haya salido del rango a verificar
     punt)
    ((equal? (list-ref (list-ref tablero cont_v) cont_h) enem) ;Si se topa con ficha enemiga, resta puntos y finaliza
     (- punt 5))
    ((equal? (list-ref (list-ref tablero cont_v) cont_h) 0) ;Valor igual a 0 suma puntos, pero pocos.
     (ver_derb tablero jugador enem i j (+ cont_h 1) (+ cont_v 1) cont_f (+ punt 3)))
    (else
     (ver_derb tablero jugador enem i j (+ cont_h 1) (+ cont_v 1) (+ cont_f 1) (+ punt 15))) ;Ficha aliada encontrada, suma mas puntos.
    )
)

;; Esta funcion verifica si el jugador tiene 3 fichas aliadas 
  ;; consecutivas en una diagonal hacia arriba y hacia la izquierda,
  ;;  tomando en cuenta la posicion del tablero y los valores de 
  ;; los jugadores.
  ;; Entradas:
  ;; - tablero: matriz que representa el tablero del juego
  ;; - jugador: valor del jugador que se esta buscando (1 o 2)
  ;; - enem: valor del jugador enemigo (1 o 2)
  ;; - i: indice de fila actual
  ;; - j: indice de columna actual
  ;; - cont_h: contador de columna actual para moverse horizontalmente
  ;; - cont_v: contador de fila actual para moverse verticalmente
  ;; - cont_f: contador de fichas aliadas consecutivas
  ;; - punt: contador de puntos acumulados hasta ahora
  ;;
  ;; Retorna:
  ;; - Un valor numerico que representa los puntos acumulados por el 
  ;; jugador, donde 1000 significa que el jugador ha ganado, 0 significa 
  ;; que no ha encontrado 3 fichas aliadas consecutivas en esa direccion
  ;;  y cualquier otro numero positivo representa los puntos acumulados
  ;;  hasta ahora.
(define (ver_izqb tablero jugador enem i j cont_h cont_v cont_f punt)
  (cond
    ((equal? cont_f 3) 1000) ;3 fichas aliadas alrededor, 1000 puntos ya que es gane
    ((or (< cont_h 0) (< cont_h (- j 3)) (= cont_v (length tablero)) 
      (> cont_v (+ i 3)))
     (ver_dera tablero jugador enem i j (+ j 1) (- i 1) cont_f punt)) ;verifica no se haya salido del rango a verificar, pasa a verificar lado opuesto.
    ((equal? (list-ref (list-ref tablero cont_v) cont_h) enem)
     (ver_dera tablero jugador enem i j (+ j 1) (- i 1) cont_f (- punt 5))) ;Si se topa con ficha enemiga, resta puntos y pasa a verificar lado opuesto
    ((equal? (list-ref (list-ref tablero cont_v) cont_h) 0)
     (ver_izqb tablero jugador enem i j (- cont_h 1) 
            (+ cont_v 1) cont_f (+ punt 3))) ;Topo con espacio vacio, suma puntos pero no muchos.
    (else
     (ver_izqb tablero jugador enem i j 
        (- cont_h 1) 
        (+ cont_v 1) (+ cont_f 1) (+ punt 15))) ;Topo con ficha aliada, suma el doble de puntos que vacio.
    )
)

;; Esta funcion continua el trabajo de la funcion anterior hacia el 
  ;; lado derecho de la fila, verificando si el jugador tiene 3 
  ;; fichas aliadas consecutivas en una fila horizontal hacia la 
  ;; derecha, tomando en cuenta la posicion del tablero y los 
  ;; valores de los jugadores.
  ;; Entradas:
  ;; - tablero: matriz que representa el tablero del juego
  ;; - jugador: valor del jugador que se esta buscando (1 o 2)
  ;; - enem: valor del jugador enemigo (1 o 2)
  ;; - i: indice de fila actual
  ;; - j: indice de columna actual
  ;; - cont_h: contador de columna actual para moverse horizontalmente
  ;; - cont_v: contador de fila actual para moverse verticalmente
  ;; - cont_f: contador de fichas aliadas consecutivas
  ;; - punt: contador de puntos acumulados hasta ahora
  ;;
  ;; Retorna:
  ;; - Un valor numerico que representa los puntos acumulados 
  ;; por el jugador, donde 1000 significa que el jugador ha ganado,
  ;;  0 significa que no ha encontrado 3 fichas aliadas consecutivas
  ;;  en esa direccion y cualquier otro numero positivo representa 
  ;; los puntos acumulados hasta ahora.
(define (ver_dera tablero jugador enem i j cont_h cont_v cont_f punt)
  (cond
    ((equal? cont_f 3) 1000) ;3 fichas aliadas alrededor, 1000 puntos ya que es gane
    ((or (= cont_h (length (list-ref tablero i))) (> cont_h (+ j 3)) 
          (< cont_v 0) (< cont_v (- i 3))) ;verifica no se haya salido del rango a verificar
     punt)
    ((equal? (list-ref (list-ref tablero cont_v) cont_h) enem) ;Si se topa con ficha enemiga, resta puntos y finaliza
     (- punt 5))
    ((equal? (list-ref (list-ref tablero cont_v) cont_h) 0) ;Valor igual a 0 suma puntos, pero pocos.
     (ver_dera tablero jugador enem i j (+ cont_h 1) (- cont_v 1) cont_f (+ punt 3)))
    (else
     (ver_dera tablero jugador enem i j (+ cont_h 1) (- cont_v 1) 
     (+ cont_f 1) (+ punt 15))) ;Ficha aliada encontrada, suma mas puntos.
    )
)

;; Esta funcion continua el trabajo de la funcion anterior hacia 
  ;; abajo de la columna, verificando si el jugador tiene 3 fichas 
  ;; aliadas consecutivas en una columna hacia abajo, tomando en 
  ;; cuenta la posicion del tablero y los valores de los jugadores.
  ;; Entradas:
  ;; - tablero: matriz que representa el tablero del juego
  ;; - jugador: valor del jugador que se esta buscando (1 o 2)
  ;; - enem: valor del jugador enemigo (1 o 2)
  ;; - i: indice de fila actual
  ;; - j: indice de columna actual
  ;; - cont_v: contador de fila actual para moverse verticalmente
  ;; - cont_f: contador de fichas aliadas consecutivas
  ;; - punt: contador de puntos acumulados hasta ahora
  ;;
  ;; Retorna:
  ;; - Un valor numerico que representa los puntos acumulados por 
  ;; el jugador, donde 1000 significa que el jugador ha ganado, 
  ;; 0 significa que no ha encontrado 3 fichas aliadas consecutivas
  ;;  en esa direccion y cualquier otro numero positivo representa 
  ;; los puntos acumulados hasta ahora.
(define (ver_b tablero jugador enem i j cont_v cont_f punt)
  (cond
    ((equal? cont_f 3) 1000) ;3 fichas aliadas alrededor, 1000 puntos ya que es gane
    ((or (= cont_v (length tablero)) (> cont_v (+ j 3))) ;verifica no se haya salido del rango a verificar
     punt)
    ((equal? (list-ref (list-ref tablero cont_v) j) enem) ;Si se topa con ficha enemiga, resta puntos y finaliza
     (- punt 5))
    ((equal? (list-ref (list-ref tablero cont_v) j) 0) ;Valor igual a 0 suma puntos, pero pocos.
     (ver_b tablero jugador enem i j (+ cont_v 1) cont_f (+ punt 3)))
    (else
     (ver_b tablero jugador enem i j (+ cont_v 1) (+ cont_f 1) (+ punt 15))) ;Ficha aliada encontrada, suma mas puntos.
    )
)

;; Esta funcion se encarga de obtener el valor en una posicion
  ;; especifica del tablero. Es una abreviatura para "tablero get".
  ;; Entradas:
  ;; - board: matriz que representa el tablero del juego
  ;; - i: indice de fila de la posicion que se quiere obtener
  ;; - j: indice de columna de la posicion que se quiere obtener
  ;;
  ;; Retorna:
  ;; - Un valor numerico que representa el valor de la posicion 
  ;; en el tablero.
(define (mget board i j)
  (list-ref (list-ref board i) j)
)

;-----------------------
;       El Voraz
;-----------------------

;; Esta funcion representa una estrategia de juego llamada "Greedy" 
  ;; (Avaro). Consiste en seleccionar la opcion que maximice el puntaje 
  ;; del jugador en cada jugada, sin considerar el futuro a largo plazo.
  ;; Entradas:
  ;; - board: matriz que representa el tablero del juego
  ;; - player: valor del jugador que esta jugando (1 o 2)
  ;; - enemy: valor del jugador enemigo (1 o 2)
  ;;
  ;; Retorna:
  ;; - Una solucion optima para el jugador actual, en terminos de 
  ;; maximizar su puntaje en la siguiente jugada.
(define (greedy board player enemy)
  (solution
   (selection
    (objective board player enemy
               (feasibility board))))
)

;-----------------------
;      Viabilidad
;-----------------------

;; Esta funcion verifica la factibilidad de las posibles jugadas 
  ;; en el tablero, es decir, si una columna aun tiene espacios 
  ;; disponibles para colocar fichas.
  ;; Entradas:
  ;; - board: matriz que representa el tablero del juego
  ;;
  ;; Retorna:
  ;; - Una lista con los indices de las columnas que aun tienen
  ;;  espacios disponibles para colocar fichas.
(define (feasibility board)
  (fea_aux board (length board) (length (list-ref board 0)) 
                (- (length board) 1) 0 '() )
)

;;Esta funcion auxiliar es utilizada por la funcion "feasibility". 
  ;; Se encarga de verificar la factibilidad de las posibles jugadas 
  ;; en una columna del tablero, es decir, si aun tiene espacios 
  ;; disponibles para colocar fichas.
  ;; Entradas:
  ;; - board: matriz que representa el tablero del juego
  ;; - i: indice de fila actual
  ;; - j: indice de columna actual
  ;; - cont_v: contador de fila actual para moverse verticalmente
  ;; - cont_h: contador de columna actual para moverse horizontalmente
  ;; - res: lista de los indices de las columnas que aun tienen 
  ;; espacios disponibles para colocar fichas
  ;;
  ;; Retorna:
  ;; - Una lista con los indices de las columnas que aun tienen 
  ;; espacios disponibles para colocar fichas.
(define (fea_aux board i j cont_v cont_h res)
  (cond
    ((< cont_v 0)
     (fea_aux board i j (- i 1) (+ cont_h 1) res))
    ((= cont_h j)
     res)
    ((= (mget board cont_v cont_h) 0)
     (fea_aux board i j (- i 1) (+ cont_h 1) 
                  (append res (list (list cont_v cont_h)))))
    (else
     (fea_aux board i j (- cont_v 1) cont_h res))
   )
)
    
;; Esta funcion se encarga de obtener un valor especifico de una
  ;;  lista en base a los indices dados.
  ;; Entradas:
  ;; - lst: lista que contiene los valores que se desean obtener
  ;; - index: indice de la lista que se desea obtener (fila)
  ;; - num: indice del valor que se desea obtener (columna)
  ;;
  ;; Retorna:
  ;; - Un valor especifico de la lista en base a los indices dados.
(define (getij lst index num)
  (cond
    ((= num 1)
     (car (list-ref lst index)))
    (else
     (cadr (list-ref lst index)))
  )
)

;; Esta funcion representa una parte importante del algoritmo del 
  ;; juego. Se encarga de calcular el puntaje de una posible jugada
  ;;  en el tablero, teniendo en cuenta la posicion actual del jugador
  ;;  y del enemigo, asi como la factibilidad de las jugadas.
  ;; Entradas:
  ;; - board: matriz que representa el tablero del juego
  ;; - player: valor del jugador que esta jugando (1 o 2)
  ;; - enemy: valor del jugador enemigo (1 o 2)
  ;; - fea_res: resultado de la funcion "feasibility", que contiene 
  ;; los indices de las columnas con espacios disponibles
  ;; - index: indice de la columna actual en "fea_res"
  ;;
  ;; Retorna:
  ;; - Un valor numerico que representa el puntaje de la posible 
  ;; jugada en esa columna del tablero.
(define (calculator board player enemy fea_res index)
  (ver_izq board player enemy (getij fea_res index 1) (getij fea_res index 2) 
  (- (getij fea_res index 2) 1) 0
           (ver_izqa board player enemy (getij fea_res index 1) 
           (getij fea_res index 2) (- (getij fea_res index 2) 1) 
           (- (getij fea_res index 1) 1) 0
                     (ver_izqb board player enemy 
                     (getij fea_res index 1) (getij fea_res index 2) 
                     (- (getij fea_res index 2) 1) 
                     (+ (getij fea_res index 1) 1) 0
                               (ver_b board player enemy 
                               (getij fea_res index 1) 
                               (getij fea_res index 2) 
                               (+ (getij fea_res index 1) 1) 0 0)))
 )
)

;-----------------------
; Conjunto de Candidatos
;-----------------------

;; Esta funcion se utiliza para obtener los posibles movimientos 
  ;; o candidatos para una jugada en el tablero en la columna 
  ;; especificada.
  ;; Entradas:
  ;; - board: matriz que representa el tablero del juego
  ;; - player: valor del jugador que esta jugando (1 o 2)
  ;; - enemy: valor del jugador enemigo (1 o 2)
  ;; - fea_res: resultado de la funcion "feasibility", que contiene 
  ;; los indices de las columnas con espacios disponibles
  ;; - index: indice de la columna actual en "fea_res"
  ;;
  ;; Retorna:
  ;; - Una lista de los posibles movimientos/candidatos para una 
  ;; jugada en la columna especificada del tablero.
(define (candidates board player enemy fea_res index)
  (candidates_aux board player enemy (- (getij fea_res index 1) 1) 
  (getij fea_res index 2))
)

;Esta funcion es una funcion auxiliar utilizada por la funcion 
  ;; "candidates". Se encarga de encontrar posibles movimientos en 
  ;; una columna especifica del tablero, siguiendo diferentes 
  ;; direcciones (izquierda, arriba-izquierda, abajo-izquierda y 
  ;; abajo) y comprobando si las casillas correspondientes son 
  ;; validas para una jugada.
  ;; Entradas:
  ;; - board: matriz que representa el tablero del juego
  ;; - player: valor del jugador que esta jugando (1 o 2)
  ;; - enemy: valor del jugador enemigo (1 o 2)
  ;; - i: indice de fila en la matriz que representa el tablero
  ;; - j: indice de columna en la matriz que representa el tablero
  ;;
  ;; Retorna:
  ;; - Una lista de los posibles movimientos/candidatos para una 
  ;; jugada en la columna especifica del tablero. Si no hay 
  ;; movimientos validos, retorna 0.
(define (candidates_aux board player enemy i j)
  (cond
    ((< i 0) 0)
    (else
     (ver_izq board player enemy i j (- j 1) 0
           (ver_izqa board player enemy i j (- j 1) (- i 1) 0
                     (ver_izqb board player enemy i j (- j 1) (+ i 1) 0
               (ver_b board player enemy i j (+ i 1) 0 0))))
   )
  )
)

;-----------------------
;       Objetivo
;-----------------------

;; Esta funcion se encarga de calcular el objetivo del 
  ;; jugador actual, es decir, el puntaje total que se puede 
  ;; obtener a partir de las jugadas posibles en cada columna.
  ;; Se utiliza la funcion "obj_aux" como funcion auxiliar, 
  ;; la cual recursivamente va sumando los puntajes obtenidos
  ;;  en cada columna.
  ;; Entradas:
  ;; - board: matriz que representa el tablero del juego
  ;; - player: valor del jugador que esta jugando (1 o 2)
  ;; - enemy: valor del jugador enemigo (1 o 2)
  ;; - fea_res: resultado de la funcion "feasibility", 
  ;; que contiene los indices de las columnas con espacios
  ;;  disponibles
  ;;
  ;; Retorna:
  ;; - El puntaje total que se puede obtener a partir de las 
  ;; jugadas posibles en cada columna.
(define (objective board player enemy fea_res)
  (obj_aux board player enemy fea_res 0 '())
)

;; Esta funcion es una funcion auxiliar utilizada por la funcion
  ;;  "objective". Se encarga de calcular el puntaje de cada 
  ;; posible jugada en cada columna del tablero y sumarlos para 
  ;; obtener el puntaje total del jugador actual.
  ;; Para ello, utiliza las funciones "calculator" y "candidates"
  ;;  para obtener el puntaje de una jugada y los posibles movimientos
  ;;  en una columna dada, respectivamente.
  ;; La funcion utiliza recursion para iterar sobre todas las columnas
  ;;  con espacios disponibles, almacenando en una lista el puntaje y 
  ;; las coordenadas de cada posible jugada.
  ;;
  ;; Entradas:
  ;; - board: matriz que representa el tablero del juego
  ;; - player: valor del jugador que esta jugando (1 o 2)
  ;; - enemy: valor del jugador enemigo (1 o 2)
  ;; - fea_res: resultado de la funcion "feasibility", que contiene
  ;;  los indices de las columnas con espacios disponibles
  ;; - index: indice actual de la columna en "fea_res"
  ;; - res: lista acumulativa de los puntajes y coordenadas de 
  ;; cada posible jugada
  ;;
  ;; Retorna:
  ;; - Una lista con los puntajes y coordenadas de cada posible jugada 
  ;; en cada columna del tablero. Cada elemento de la lista es a su 
  ;; vez una lista con tres valores: las coordenadas de la 
  ;; jugada (fila y columna) y el puntaje obtenido.
(define (obj_aux board player enemy fea_res index res)
  (cond
    ((= (length res) (length fea_res))
     res)
    (else
     (obj_aux board player enemy fea_res (+ index 1)
              (append res (list (list (getij fea_res index 1) 
                          (getij fea_res index 2)
                          (balance
                           (calculator board player enemy fea_res index)
                           (calculator board enemy player fea_res index)
                           (candidates board enemy player fea_res index)
                           )))))
     )
  )
)

;; Esta funcion se encarga de balancear los puntajes obtenidos 
  ;; en una jugada, tomando en cuenta tanto el puntaje del jugador 
  ;; actual como el del jugador enemigo, y sumandoles un puntaje 
  ;; adicional si la jugada permite conseguir una posicion ganadora 
  ;; o evita una posicion perdedora.
  ;;
  ;; Entradas:
  ;; - playerscore: puntaje obtenido por el jugador actual en una jugada
  ;; - enemyscore: puntaje obtenido por el jugador enemigo en una jugada
  ;; - futurescore: puntaje adicional si la jugada permite conseguir
  ;;  una posicion ganadora o evita una posicion perdedora
  ;;
  ;; Retorna:
  ;; - El puntaje balanceado de la jugada, teniendo en cuenta los 
  ;; puntajes del jugador actual y enemigo, asi como la posibilidad
  ;;  de conseguir una posicion ganadora o evitar una perdedora. 
  ;; Si la jugada no cumple con ninguna de estas condiciones, 
  ;; retorna el puntaje del jugador actual sin modificaciones.
(define (balance playerscore enemyscore futurescore)
  (cond ((> playerscore 900) 1000)
        ((> enemyscore 900) 500)
        ((> futurescore 900) -500)
        (else playerscore)
  )
)


;-----------------------
;       Seleccion
;-----------------------

;; Esta funcion implementa el algoritmo de ordenamiento Selection 
  ;; Sort para ordenar una lista de puntos en orden descendente, 
  ;; de manera que la primera posicion de la lista tenga el puntaje 
  ;; mas alto.
  ;; Entradas:
  ;; - lst: lista de puntos a ordenar
  ;; Retorna:
  ;; - Una nueva lista con los mismos elementos que la lista de 
  ;; entrada, pero ordenados en orden descendente segun el puntaje
  ;;  de cada elemento.
(define (selection lst)
  (define (car-get-points lst2)
    (caddar lst2)
  )
  (define (selection_aux lst2 max-lst)
    (cond 
      ((null? lst2)
       max-lst)
      ((> (car-get-points lst2) (car-get-points max-lst))
       (selection_aux (cdr lst2) (list (car lst2))))
      ((= (car-get-points lst2) (car-get-points max-lst))
       (selection_aux (cdr lst2) (append max-lst (list (car lst2)))))
      (else
       (selection_aux (cdr lst2) max-lst))
    )
  )
  (cond
    ((null? lst)
     '())
    (else
     (selection_aux (cdr lst) (list (car lst))))
  )
)



;-----------------------
;       Solucion
;-----------------------

;; Esta funcion toma una lista de listas, donde cada 
  ;; sublista representa un nodo en un arbol, y retorna el 
  ;; valor almacenado en el nodo hoja mas profundo del arbol.

  ;; Entradas:
  ;; - lst: una lista de listas que representa un arbol. Cada
  ;;  sublista debe contener tres elementos: un identificador 
  ;; unico del nodo, el valor almacenado en el nodo, y una 
  ;; lista de subnodos del nodo.

  ;; Retorna:
  ;; - El valor almacenado en el nodo hoja mas profundo del arbol.
(define (solution lst)
  (cond
    ((= (length lst) 1) (cadr (car lst)))
    (else
     (solution_aux lst)
     )
  )
)

;; Esta funcion toma una lista y devuelve el valor en la posicion 
  ;; aleatoria generada por el programa.
  ;;
  ;; Entradas:
  ;; - lst: una lista de elementos.
  ;;
  ;; Retorna:
  ;; - El valor en la posicion aleatoria de la lista.
  ;;
  ;; Ejemplo:
  ;; (solution_aux '(1 2 3 4 5)) => 3
(define (solution_aux lst)
  (define random-index (random (length lst)))
  (cadr (list-ref lst random-index ))
)




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
					     (equal? (matriz-buscar new-m new-n matriz) 0))))
				directions)))

	(define (place-adjacent m n)
		(let ((adjacent-free (find-adjacent-free m n)))
			(printf "Adjacent free spaces for (~a, ~a): ~a\n" m n adjacent-free) ; Debugging output
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
			(else
				(let ((current-value (matriz-buscar m n matriz)))
					(printf "Checking position (~a, ~a): ~a\n" m n current-value) ; Debugging output
					(if (= current-value marcador)
						(let ((result (place-adjacent m n)))
							(if result
								result
								(loop m (+ n 1))))
						(loop m (+ n 1))))))))

                                    


