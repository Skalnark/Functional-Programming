#lang racket

;; 1 e 2
(define a 24)
(define b 14)
;; 3.1
(+ a b)
;; 3.2
(- a b)
;; 3.3
(+ a (+ (* 3 b) 7))
;; 3.4
(/ (+ a b) 2)
;; 3.5
(sqrt (+ a b))
(/ 2 (+ ( / 1 a) (/ 1 b)))
;; 4
(define soma-medias (+ (/ 2 (+ ( / 1 a) (/ 1 b))) (/ (+ a b) 2)))
;; 5
(if (= soma-medias 49)
    "teste 1 ok"
    "teste 1 falhou")
;; 6
(define (quadrado x) (* x x))
;; 7
(define (delta a b c) (- (quadrado b) (* 4 (* a c))))
(define (raiz-positiva a b c)(/ (+ (* -1 b) (sqrt (delta a b c))) (* a 2)))
;; 8
(define (potencia-positiva x y)(if (= y 1)
                                x                                
                                (* x (potencia-positiva x (- y 1)))))
(define (potencia-negativa x y)(if (= y -1)
                                (/ x x)
                                (/ (potencia-negativa x (+ y 1)) x)))

(define (potencia x y)(
  if (= y 0)
     1
     (if (> y 0)
         (potencia-positiva x y)
         (potencia-negativa x y))))
     