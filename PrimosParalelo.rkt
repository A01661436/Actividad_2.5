#lang racket

;Paralelo
(require racket/future)

(define (es-primo? n)
  ;Si n es menor que 2 devuelve falso
  (if (< n 2) 
      #f
      ;Para i desde 2 hasta sqrt(n)
      (let bucle ((i 2))
        (if (> i (sqrt n)) 
            #t
            ;Devuelve falso si n es divisible entre i exactamente
            (if (zero? (remainder n i))
                #f
                (bucle (add1 i)))))))

;Comienza la suma de primos
(define (suma-primos-en-rango inicio fin)
  (let bucle ((i inicio) (suma 0))
    (if (> i fin)
        suma
        (bucle (add1 i) (if (es-primo? i) (+ i suma) suma)))))

;processor-count cuenta la cantidad de procesadores utilizados
(define (suma-primos-hasta n)
  (define futuros
    (for/list ([i (in-range (processor-count))])
      (define inicio (+ (* i (quotient n (processor-count))) (if (= i 0) 2 0)))
      (define fin (if (= i (- (processor-count) 1)) n (+ inicio (quotient n (processor-count)))))
      (future (λ () (suma-primos-en-rango inicio fin)))))
  (for/sum ([f (in-list futuros)]) (touch f)))

(time (display (suma-primos-hasta 5000000)))
(display (processor-count))
(newline)


