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
  (cond [(< i (my-string-length string)) (cons (my-string-ref string i)
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
  (cond [(< (cond [(es-hoja? elem1) (cdr elem1)]
                  [else (cdar elem1)])
            (cond [(es-hoja? elem2) (cdr elem2)]
                  [else (cdar elem2)])) (list (cons (string->symbol (my-string-append (symbol->string (cond [(es-hoja? elem1) (car elem1)]
                                                                                                              [else (caar elem1)]))
                                                                                        (symbol->string (cond [(es-hoja? elem2) (car elem2)]
                                                                                                              [else (caar elem2)]))))
                                                         (+ (cond [(es-hoja? elem1) (cdr elem1)]
                                                                  [else (cdar elem1)])
                                                            (cond [(es-hoja? elem2) (cdr elem2)]
                                                                  [else (cdar elem2)])))
                                              elem1
                                              elem2)]
        [else (list (cons (string->symbol (my-string-append (symbol->string (cond [(es-hoja? elem2) (car elem2)]
                                                                               [else (caar elem2)]))
                                                         (symbol->string (cond [(es-hoja? elem1) (car elem1)]
                                                                               [else (caar elem1)]))))
                          (+ (cond [(es-hoja? elem1) (cdr elem1)]
                                   [else (cdar elem1)])
                             (cond [(es-hoja? elem2) (cdr elem2)]
                                   [else (cdar elem2)])))
                    elem2
                    elem1)]))

;verifica si un nodo es
;hoja o nodo intermedio.
(define (es-hoja? nodo)
  (and (pair? nodo)
       (symbol? (car nodo))
       (number? (cdr nodo))))


;verifica si una lista
;posee solo un elemento.
(define (un-elemento? lista)
  (cond [(empty? (cdr lista)) #t]
        [else #f]))

;dibuja la estructura
;de acuerdo a las aristas
;recorridas.
(define (identacion aristas un-elemento?)
  (map (lambda (x) (display "   ")) aristas)
  (display (cond [(empty? aristas) ""]
                 [(and un-elemento?
                       (not (empty? aristas))) "└─ "]
                 [else "├─ "])))

;genera el arbol a
;partir de un string
(define (tree string)
  (build-tree (build-freq-table string)))

;imprime el arbol de
;forma jerarquica
(define (print-tree tree)
  (print-tree-aux tree null #t))

(define (print-tree-aux tree aristas un-elemento?)
  (identacion aristas un-elemento?)
  (cond [(es-hoja? tree) (displayln tree)]
        [else (displayln (car tree))])
  (cond [(not (es-hoja? tree)) (print-tree-aux (cadr tree) (append aristas (list #f)) #f)
                              (print-tree-aux (caddr tree) (append aristas (list #t)) #t)]))


;redefinicion de length
;para strings
(define (my-string-length string)
  (tam (string->list string)))

;redefinicion de string-ref
;para strings
(define (my-string-ref string i)
  (get-char (string->list string) i))

;metodo proveido por el
;profesor via classroom.
(define (get-char lista n)
  (cond [(eq? 0 n) (car lista)]
        [else (get-char (cdr lista) (- n 1))]))

;convierte '(h o l a)
;a "hola"
(define (symbol-to-string lista)
  (apply string-append (map symbol->string lista)))

;convierte "01101001110011110110100"
; a '(0 1 1 0 1 0 0 1 1 1 0 0 1 1 1 1 0 1 1 0 1 0 0) 
(define (binario-to-list binario)
  (map (lambda (x) (- (char->integer x)
                      (char->integer #\0))) (string-to-list binario)))

(define (decode arbol binario)
  (symbol-to-string (decode-aux arbol
                                arbol
                                (binario-to-list binario)
                                null)))

(define (decode-aux arbol actual binario R)
  (cond [(empty? binario) (reverse R)]
        [else (cond [(eq? (car binario) 0) (cond [(es-hoja? (cadr actual)) (decode-aux arbol
                                                                                       arbol
                                                                                       (cdr binario)
                                                                                       (cons (car (cadr actual))
                                                                                             R))]
                                                 [else (decode-aux arbol
                                                                   (cadr actual)
                                                                   (cdr binario)
                                                                   R)])]
                    [else (cond [(es-hoja? (caddr actual)) (decode-aux arbol
                                                                       arbol
                                                                       (cdr binario)
                                                                       (cons (car (caddr actual))
                                                                             R))]
                                [else (decode-aux arbol
                                                  (caddr actual)
                                                  (cdr binario)
                                                  R)])])]))

(define (my-string-append string1 string2)
  (list->string (append (string->list string1)
          (string->list string2))))

; Función para generar códigos binarios del árbol de Huffman
(define (generate-codes arbol)
  (cond
    ((es-hoja? arbol) (list (cons (car arbol) "0")))
    (else (codigos-aux arbol ""))))

; Función auxiliar para generar códigos recursivamente
(define (codigos-aux nodo codigo-actual)
  (cond
    ((es-hoja? nodo) (list (cons (car nodo) codigo-actual)))
    (else (append (codigos-aux (cadr nodo) (my-string-append codigo-actual "0"))
                  (codigos-aux (caddr nodo) (my-string-append codigo-actual "1"))))))

; Función para buscar el código de un símbolo
(define (obtener-codigo simbolo tabla-codigos)
  (cond
    ((null? tabla-codigos) "")
    ((equal? simbolo (car (car tabla-codigos))) (cdr (car tabla-codigos)))
    (else (obtener-codigo simbolo (cdr tabla-codigos)))))

; Función principal para codificar un string
(define (encode texto arbol)
  (codificar-string (convertir-chars-a-simbolos (string-to-list texto)) (generate-codes arbol)))

; Función para convertir caracteres a símbolos
(define (convertir-chars-a-simbolos lista-chars)
  (cond
    ((null? lista-chars) null)
    (else (cons (string->symbol (string (car lista-chars)))
                (convertir-chars-a-simbolos (cdr lista-chars))))))

; Función auxiliar para codificar lista de caracteres
(define (codificar-string lista-chars tabla-codigos)
  (cond
    ((null? lista-chars) "")
    (else (my-string-append (obtener-codigo (car lista-chars) tabla-codigos)
                           (codificar-string (cdr lista-chars) tabla-codigos)))))

;funciones a ejecutar
;1- (build-freq-table "tres tristes tigres tragaban trigo en un trigal")
;
;2- (build-tree forest)
;
;3- (generate-codes arbol)
;
;4- (encode texto arbol)
;
;5- (decode arbol binario)
;
;6- (print-tree tree)