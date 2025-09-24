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
                  (lambda (x y) (> (cdr x) (cdr y)))))

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

;forest=> '(("a" . 5) ("b" . 2) ("r" . 2) ("c" . 1) ("d" . 1))
;(cdar forest)=> 5
;(caar forest)=> "a"

(define (build-tree forest)
  (cond [(empty? lista) null]
        [else (cons (caar forest) (cons (caadr forest) (+)))]))



