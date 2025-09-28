#lang racket

;elimina elementos duplicados en una lista
(define (eliminar-duplicados lista)
  (cond [(empty? lista) null]
        [else (eliminar-dup lista
                            )]))

(define (eliminar-dup lista )
  (cond [(empty? lista) null]
        [else (cons (car lista)
                    (eliminar-dup (eliminar-elem (car lista)
                                                 (cdr lista))))]))

;elimina un todos los elemento de una lista
(define (eliminar-elem elemento lista)
  (cond [(empty? lista) null]
        [(not (equal? elemento (car lista)))(cons (car lista)
                                                  (eliminar-elem elemento
                                                                 (cdr lista)))]
        [else (eliminar-elem elemento (cdr lista))]))

(define (build-freq-table string)
 (sort (frecuencias (string-to-list string))
                  (lambda (x y) (< (cdr x) (cdr y)))))

;devuelve la lista con la cantidad de cada palabra (palabra cantidad)
(define (frecuencias lista)
  (map (lambda (x) (cons (string->symbol (string x))
                         (contar x
                                 lista)))
       (eliminar-duplicados lista)))

;cuenta la cantidad de ocurrencias de una palabra en la lista
(define (contar palabra lista)
  (tam (filter (lambda (x) (eq? palabra x))
               lista)))

;obtiene la cantidad de elementos de una lista
(define (tam lista)
  (tam-aux lista 0))

(define (tam-aux lista t)
  (cond [(empty? lista) t]
        [else (tam-aux (cdr lista)
                       (+ t
                          1))]))

;convierte un STRING a una lista de caracteres
(define (string-to-list string)
  (string-to-list-aux string 0))

(define (string-to-list-aux string i)
  (cond [(< i (string-length string)) (cons (string-ref string i)
                             (string-to-list-aux string
                                                  (+ 1 i)))]
        [else null] ))

;forest=> '((c . 1) (d . 1) (b . 2) (r . 2) (a . 5))
;(cdar forest)=> 1
;(caar forest)=> c

;construye el arbol binario
(define (build-tree forest)
  (cond [(= 1 (length forest)) (car forest)]
        [else (build-tree (insertar-nodo (combinar-pares (car forest)
                                                         (cadr forest)) (cddr forest)))]))

;inserta un nodo en una lista
(define (insertar-nodo nodo lista)
  (cond [(empty? lista) (list nodo)]
        [(< (obtener-frecuencia nodo)
            (obtener-frecuencia (car nodo))) (cons nodo
                                                    lista)]
        [else (cons (car lista)
                    (insertar-nodo nodo
                                   (cdr lista)))]))

;obtiene la frecuencia de un
;nodo intermedio o de una hoja
(define (obtener-frecuencia nodo)
  (cond [(es-hoja? nodo) (cond [(es-hoja? (car nodo)) (cdr (car nodo))]
                               [else (cdr nodo)])]
        [else 0]))

;genera un nodo intermedio
;> (combinar-pares '(c . 1) '(d . 1))
;'((cd . 2) (c . 1) (d . 1))
(define (combinar-pares elem1 elem2)
  (list (cons (string->symbol (string-append (symbol->string (cond [(es-hoja? elem1) (car elem1)]
                                                                   [else (caar elem1)]))
                                             (symbol->string (cond [(es-hoja? elem2) (car elem2)]
                                                                   [else (caar elem2)]))))
              (+ (cond [(es-hoja? elem1) (cdr elem1)]
                       [else (cdar elem1)])
                 (cond [(es-hoja? elem2) (cdr elem2)]
                       [else (cdar elem2)])))
        elem1
        elem2))

;verifica si un nodo es
;hoja o nodo intermedio.
(define (es-hoja? nodo)
  (and (pair? nodo)
       (symbol? (car nodo))
       (number? (cdr nodo))))




