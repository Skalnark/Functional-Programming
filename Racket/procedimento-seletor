#lang racket

(require racket/class)

(define point%
  (class object%
    
    (field (a 0)) ; Inicia em 0
    (field (b 0)) ; Inicia em 0
    
    (define/public x
      (case-lambda
        [() a]
        [(value) (set! a value)]))
    
    (define/public y
      (case-lambda
        [() b]
        [(value) (set! b value)]))
    
    (define/public r
      (case-lambda
        [() (sqrt (+ (expt a 2) (expt b 2)))]
        [(value) (values (set! a (round (* (cos (theta->rad)) value)))
                         (set! b (round (* (sin (theta->rad)) value))))]))
    
    (define/public theta
      (case-lambda
        [() (radians->degrees (calc-theta a b))]
        [(value) (values (x (round (* (r) (cos (degrees->radians value)))))
                         (y (round (* (r) (sin (degrees->radians value))))))]))
    
    (define/private (theta->rad) (calc-theta a b))
    
    (define/private (calc-theta x y) ;Função que realiza o cálculo de theta
      (define pi 3.14159265359)
      (define razão (/ y x))
      (cond
        [(> x 0) (atan razão)]
        [(and (< x 0) (>= y 0)) (+ (atan razão) pi)]
        [(and (< x 0) (< y 0)) (- (atan razão) pi)]
        [(and (zero? x) (> y 0) (/ pi 2))]
        [(and (zero? x) (< y 0) (/ pi -2))]
        [(and (zero? x) (zero? y)) 0]))
    
    (super-new)
    ))


(define ponto (new point%)) ; Cria instância de point

(send ponto x 1) ;set x = 1
(send ponto y 1) ;set y = 1
(println "convertendo (1, 1) para coordenadas polares: ")
(cons (send ponto r) (send ponto theta))

(send ponto r 1) ;set r = 1
(send ponto theta 90) ;set theta = 1
(println "convertendo (1, 90) para coordenadas cartesianas: ")
(cons (send ponto x) (send ponto y))
