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

(define (analizar lista)
  (displayln (frecuencias lista))
  (displayln (car (mas-repetida (frecuencias lista)))))

;devuelve la lista con la cantidad de cada palabra (palabra cantidad)
(define (frecuencias lista)
  (map (lambda (x) (cons x
                         (cons (contar x
                                         lista)
                                 null)))
       (eliminar-duplicados lista)))

;obtiene la palabra mas repetida (palabra cantidad)
(define (mas-repetida lista)
  (mas-repetida-aux lista (car lista)))

(define (mas-repetida-aux lista M)
  (cond [(empty? lista) M]
        [(> (cadar lista) (cadr M)) (mas-repetida-aux (cdr lista)
                                                      (car lista))]
        [else (mas-repetida-aux (cdr lista)
                                M)]))

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
