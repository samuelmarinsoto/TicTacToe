#lang racket

(require racket/gui/base)
(require "logica.rkt")


;; -------- Interfaz del Juego ---------


;; --------- Inicio -------------

;; Crea ventana inicial
(define frame1 
  (new frame% [label "Inicio"] [width 500] [height 300]
  )
)

;; Crea un label para mostrar un mensaje
(new message% 
    [parent frame1] 
    [label "Escoja el tamaño de la matriz"] 
)

;; Slider con opciones para el tamaño de la fila
(define row-slider (new slider%
                        [label "Fila:   "]
                        [min-value 3]
                        [max-value 10]
                        [parent frame1]
                    )
)

;; Slider con opciones para el tamaño de la columna
(define col-slider (new slider%
                        [label "Columna: "]
                        [min-value 3]
                        [max-value 10]
                        [parent frame1]
                    )
)

;; Llamada de accion del boton listo
(define (boton-listo-acc b e)
  (start-juego 
              (send row-slider get-value)
              (send col-slider get-value)
  )
)

;; Crea el boton listo   
(new button% [label "Listo"]
    [parent frame1]
    [callback boton-listo-acc])

;; Muestra el frame 1
(send frame1 show #t)


;; --------- Juego -------------

;; Crea ventana 2 que muestra el juego
(define frame2 (new frame% [label "Tic-Tac-Toe"] [width 500] 
                          [height 500])
)

;; Crea un label con un mensaje
(define msg (new message% [parent frame2] 
            [label "Turno Jugador =          "] )
)

;; Crea un panel para guardar el panel de botones
(define panel2 
  (new horizontal-panel% [parent frame2])
)

;; Decide cual jugador juega (1 jugador, 2 computadora)
(define jugador-actual 1)

;; define tablero del juego
(define board '())

;; La matriz de botones
(define matriz-botones '())

;; Cambia el turno del jugador actual, actualiza el mensaje en pantalla
(define (cambiar-jugador p)
  (cond
    [(equal? 1 p) 
      (set! jugador-actual 2)
      (send msg set-label "Turno Computadora: O")
    ]
    [else
      (set! jugador-actual 1)
      (send msg set-label "Turno Jugador: X")
    ]
  )
  (send msg refresh)
)

;; Botones de la matriz de botones
(define Boton-tablero
  (class button%
    (init-field [row 0] [col 0])
    (super-new)
    (define/public (get-row)
      row)
    (define/public (get-col)
      col)
  )
)


;; Inicio del juego
;; Inicializa tablero tamano nxm
;; Establece al jugador con turno inicial
;; muestra pantalla de juego y esconde frame1
(define (start-juego n m)
  (set! jugador-actual 1)
  (set! board (create-matriz n m))
  (send msg set-label "Turno Jugador: X")
  (send msg refresh)
  (create-board-panel board) ; crea el panel de botones
  (send frame1 show #f)
  (send frame2 show #t)
)

;; Crea la matriz de botones y coloca cada boton en su posicion respectiva. 
(define (create-board-panel board)
  (define panel (new vertical-panel% [parent panel2]))
  (define (create-row-panel i)
    (define row-panel (new horizontal-panel% [parent panel]))
    (define row-buttons '())
    (define (create-button j)
      (define b (new Boton-tablero [parent row-panel]
                            [label "-"]
                            [callback button-grid-callback]
                            [row i]
                            [col j])
      )
      (set! row-buttons (append row-buttons (list b)))
      (cond 
      ((< j (- (length (list-ref board i)) 1)) 
             (create-button (+ j 1)))
      )
    )
    (create-button 0)
    (set! matriz-botones (append matriz-botones (list row-buttons)))
    (cond 
    ((< i (- (length board) 1))
           (create-row-panel (+ i 1)))
    )
  )
  (create-row-panel 0)
  panel
)

;; Realiza un impresion por consola de los botones del juego
(define (print-buttons matrix)
  (for ([i (in-range (length matrix))])
    (for ([j (in-range (length (list-ref matrix i)))])
    (define b (list-ref (list-ref matrix i) j))
    (display (format "(~a, ~a) " (send b get-row) (send b get-col)))
    )
    (newline)
  )
) 

;; Verfica si hay condicion de juego terminado(chequea por empate o gane)
(define (status-partida)
  (define winner (if (equal? jugador-actual 1) "El Jugador" "La Computadora"))
  (cond
    [(check-gane board jugador-actual)
     (display (format "¡~a ha ganado el juego!" winner))
     (define frame3 (new frame% [label "Fin del Juego"]
                        [width 500]
                        [height 500]))
     (new message% [parent frame3]
                   [label (format "¡~a ha ganado el juego!" winner)])
     (send frame2 show #f)
     (send frame3 show #t)
    ]
    ((check-tie board)
      (display "Empate: Ninguno ganó")
      (define frame3 (new frame% [label "Fin del Juego"] 
      [width 500] 
      [height 500]))
      (new message% [parent frame3]
      [label "El juego ha quedado en empate"] )
      (send frame2 show #f)
      (send frame3 show #t)
     )
    (else #f)
  )
)

;; Funcion que se ejecuta cuando un boton se presiona
;; Actualiza el tablero con el movimiento hecho por el jugador o computadora
;; verifica estado del juego
;; Permite que la computadora juegue con el algoritmo greedy
(define (button-grid-callback b e)
  (set! board (insert-symbol (send b get-row)(send b get-col) board jugador-actual))

  (displayln (format "Jugador ~a presiono boton (~a, ~a)" 
                                      jugador-actual
                                      (send b get-row) 
                                      (send b get-col)))
  (print-matriz board)

  (update-board-panel)
   (when (not (status-partida))
  ;; Juega Greedy
  (cambiar-jugador jugador-actual)
  (define voraz(place-nuevo-elemento board 1))
  (set! board (insert-symbol (car voraz) (cdr voraz) board jugador-actual))
  
  (displayln (format "La Computadora usa ficha ~a puso en la fila ~a y columna ~a" 
                                      jugador-actual
                                     (car (place-nuevo-elemento board 1))
                                     (cdr (place-nuevo-elemento board 1))))
  (print-matriz board)
  (update-board-panel)
  (status-partida)
  ;; Sede turno player
  (cambiar-jugador jugador-actual)
  )
  )
 

;; Actualiza como se ve la matriz de botones cada vez que se juega por el jugador o computadora
(define (update-board-panel)
  (define (update-board-panel-aux i j)
    (cond
      [(>= i (length board)) #f] ; Caso base: Si i es mayor que la longitud de la lista, termina la recursión
      [(>= j (length (list-ref board i))) ; Si j es mayor que la longitud de la sublista i-ésima, pasa a la siguiente sublista
        (update-board-panel-aux (+ i 1) 0)]
      [else ; Si no, actualiza el botón correspondiente y pasa al siguiente elemento de la sublista
        (define button (list-ref (list-ref matriz-botones i) j))
        (cond
          [(equal? 0 (list-ref (list-ref board i) j))
            (send button set-label " ")
            (send button enable #t)]
          [(equal? 1 (list-ref (list-ref board i) j))
            (send button set-label "X")
            (send button enable #f)]
          [(equal? 2 (list-ref (list-ref board i) j))
            (send button set-label "O")
            (send button enable #f)]
        )
        (update-board-panel-aux i (+ j 1))
      ]
    )
  )
  (update-board-panel-aux 0 0)
)
