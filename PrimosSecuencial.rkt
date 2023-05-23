#lang racket

;Secuencial
(define (es-primo? n)
  ;Si n es menor que 2 devuelve falso
  (if (< n 2) 
      #f
      ;Para i desde 2 hasta sqrt(n)
      (let bucle ((i 2))
        (if (> i (sqrt n)) 
            #t
            ;Devuelve falso si n es divisible entre i exactamente
            (if (= (remainder n i) 0) 
                #f
                (bucle (add1 i)))))))

;Función que realiza la suma
(define (suma-primos-hasta n)
  (let bucle ((i 2) (suma 0))
    (if (>= i n)
        suma
        (bucle (add1 i) (if (es-primo? i) (+ suma i) suma)))))

(time (display (suma-primos-hasta 5000000)))
(newline)
