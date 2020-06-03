;;--------------------------------------------------------------------------------------------------;;
;Randald Vilegas Brenes
;Laboratorio 2
;Lenguaje Funcional
;;--------------------------------------------------------------------------------------------------;;

;;--------------------------------------------------------------------------------------------------;;
;función que se llame aplanar-map que recibe una lista con posibles listas
;y devuelve una lista con los elementos aplanados. (30%)
;;solucion #1
(define (aplanar-map lst)
  (cond [(null? lst) '()]
        [(pair? lst)(append (aplanar-map (car lst)) (aplanar-map (cdr lst)))]
        (else(list lst))))

;;--------------------------------------------------------------------------------------------------;;
;Escriba una función que reciba una posición de una matriz (par ordenado ‘(x y) ),
;una cantidad de elementos y que devuelva tolas las cadenas de caracteres
;en formato string que pueden venir en todas las direcciones tanto horizontal
;como vertical (4 en total).
;RESTRICCIONES:
;- Debe devolver solo aquellas cadenas con la cantidad de caracteres igual a lo que se indica por parámetro.
;- Debe utilizarse map para elaborar esta función
;- No debe manejarse ninguna variable global
;Nota: existen funciones para convertir de string a lista de caracteres y viceversa. (70%)
;solucion #2
;;--------------------------------------------------------------------------------------------------;;
;;metodo para crear matriz de caracteres random 
(define (random-matrix size)
  (define letters "abcdefghijklmnñopqrstuvwxyz")
  (map (lambda (x) (build-list size (lambda (y)(list-ref (string->list letters)(random 24)))))
       (build-list size (lambda (z) '()))))
 
;;--------------------------------------------------------------------------------------------------;;
;;metodo para crear una matriz de índices con tamaño específico
(define (make-index-matrix size)
  (map (lambda(x) (combine x 0 (- size 1)))
       (build-list size values)))

;;--------------------------------------------------------------------------------------------------;;
;;metodo utilizado en (make-index-matrix size)
(define (combine i j max)
  (cond ((equal? j max)
         (list (list i j)))
        (else
         (cons (list i j)
               (combine i (+ j 1) max)))))

;;--------------------------------------------------------------------------------------------------;;
;; Metodo para obtener un valor en la matrix
(define (get-pos x y matrix)
  (cond ((and
          (and (>= x 0) (< x (length matrix)))
          (and (>= y 0) (< y (length (list-ref matrix x)))))
         (list-ref (list-ref matrix x) y))
        (else
         '())))

;;--------------------------------------------------------------------------------------------------;;
;Metodo para buscar de [arr-aba]
(define (buscar-3 x y len matrix)
  (cond ((equal? (- len 1) 0)
         (list (get-pos x y matrix)))
        (else
         (append
          (list (get-pos x y matrix))
          (buscar-3 (+ x 1) y (- len 1) matrix)))))

;;--------------------------------------------------------------------------------------------------;;
;Metodo para buscar de [aba-arr]
(define (buscar-4 x y len matrix)
  (cond ((equal? (- len 1) 0)
         (list (get-pos x y matrix)))
        (else
         (append
          (list (get-pos x y matrix))
          (buscar-4 (- x 1) y (- len 1) matrix)))))


;;--------------------------------------------------------------------------------------------------;;
;Metodo para buscar de [izq-der]
(define (buscar-5 x y len matrix)
  (cond ((equal? (- len 1) 0)
         (list (get-pos x y matrix)))
        (else
         (append
          (list (get-pos x y matrix))
          (buscar-5  x (- y 1) (- len 1) matrix)))))

;;--------------------------------------------------------------------------------------------------;;
;Metodo para buscar de [der-izq]
(define (buscar-6 x y len matrix)
  (cond ((equal? (- len 1) 0)
         (list (get-pos x y matrix)))
        (else
         (append
          (list (get-pos x y matrix))
          (buscar-6  x (+ y 1) (- len 1) matrix)))))

;;--------------------------------------------------------------------------------------------------;;
;metodo para buscar en todas las posiciones
(define (buscar-todos i j tamano-buscar m)
  (printf "\n MATRIZ => ~a" m)
  (printf "\n (I,J) => ~a, ~a" i j)
  (let ((a (buscar-3 i j tamano-buscar m))
        (b (buscar-4 i j tamano-buscar m))
        (c (buscar-5 i j tamano-buscar m))
        (d (buscar-6 i j tamano-buscar m))
        (e '())
        )
   
    
    (set! e (list a b c d))

    (printf "\n>>Lista = ~a" e))

  )

  

;;--------------------------------------------------------------------------------------------------;;
;Prueba de metodos
(aplanar-map '(1 2 (3 (4 (8 8))) (5 (6))))
(buscar-todos 0 0 2 (random-matrix 6)); parametros:(i j tamano-buscar tamano-matrix)

















