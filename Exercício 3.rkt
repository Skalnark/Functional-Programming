#lang racket

(require rackunit rackunit/text-ui)

;;
;; Tarefa 3
;;
;; Nesta tarefa faremos um pouco de revisão mas também vamos explorar
;; algumas técnicas novas.
;;

;; Vimos antes como escrever funções recursivas para lidar com listas, baseando
;; a estrutura do código na estrutura recursiva das listas.

;; Lembrando: uma lista satisfaz um dos seguintes casos:
;; 1. é a lista vazia, '()
;; 2. é o cons de um elemento na frente de uma outra lista, (cons x lst)
;;    onde x é o elemento e lst a lista

;; Nas primeiras duas questões podemos continuar com essa estrutura:

;; --- Questão 1 ----------------------------

;; Escreva uma função remove-primeiro tal que
;; (remove-primeiro x lst) remove a primeira ocorrência do elemento x
;; na lista lst (se houver), retornando uma nova lista com o resultado.
;; Veja os testes para exemplos.

(define (remove-primeiro x lst)
  (if (empty? lst) '()
   ;;(if (equal? x (first lst)) (rest lst)
    ;;(cons (first lst) (remove-primeiro x (rest lst))))))
  (tail-remove-primeiro x lst '())))

(define (tail-remove-primeiro x lst acc)
  (cond
    [(empty? lst) (reverse acc)]
    [(equal? (first lst) x) (append (reverse acc) (rest lst))]
    [(tail-remove-primeiro x (rest lst) (cons (first lst) acc))]))
   

(define-test-suite test-remove-primeiro
  (test-equal? "lista vazia"
               (remove-primeiro 5 '())              '())
  
  (test-equal? "uma ocorrência"
               (remove-primeiro 5 '(1 3 5 7))       '(1 3 7))
  
  (test-equal? "múltiplas ocorrências"
               (remove-primeiro 5 '(1 3 5 7 5 9))   '(1 3 7 5 9))
  
  (test-equal? "nenhuma ocorrência"
               (remove-primeiro 3 '(11 7 23 55 42)) '(11 7 23 55 42)))


;; --- Questão 2 ----------------------------

;; Escreva uma função remove-todos tal que
;; (remove-todos x lst) remove todas as ocorrencias do elemento x
;; na lista lst (se houver), retornando uma nova lista com o resultado.
(define (remove-todos x lst)
  (if (empty? lst) '()
      ;;(if (equal? x (first lst)) (remove-todos x (rest lst))
          ;;(cons (first lst) (remove-todos x (rest lst))))))
      (tail-remove-todos x lst '())))

(define (tail-remove-todos x lst acm)
  (cond
    [(empty? lst) (reverse acm)]
    [(equal? x (first lst)) (tail-remove-todos x (rest lst) acm)]
    [(tail-remove-todos x (rest lst) (cons (first lst) acm))]))

(define-test-suite test-remove-todos
  (test-equal? "lista vazia"           (remove-todos 5 '())              '())
  (test-equal? "uma ocorrência"        (remove-todos 5 '(1 3 5 7))       '(1 3 7))
  (test-equal? "múltiplas ocorrências" (remove-todos 5 '(1 3 5 7 5 9))   '(1 3 7 9))
  (test-equal? "nenhuma ocorrência"    (remove-todos 3 '(11 7 23 55 42)) '(11 7 23 55 42)))


;; --- Questão 3 ----------------------------

;; As funções remove-primeiro e remove-todos, acima, funcionam apenas para
;; listas de números, ou também funcionam para listas de outros tipos de
;; elementos, como strings? Funciona com listas heterogêneas (com elementos
;; de tipos diferentes na mesma lista)? Faça alguns testes que demonstram se
;; funcionam ou não. 

(define-test-suite test-lista-generica
  (test-equal? "lista de strings" (remove-todos "a" (list "a" "b" "c" "a" "d" "a")) '("b" "c" "d"))
  (test-equal? "lista heterogênea" (remove-todos 3 (list "a" '() 3 "três" 3 '(3 3 3))) '("a" () "três" (3 3 3)))
  (test-equal? "lista de listas" (remove-todos '(1 2 3) (list '("a" "b" "c") '(1 2 3) '() '(3 2 1) '(1 2 3))) '(("a" "b" "c") () (3 2 1))))

;; --- Questão 4 ----------------------------

;; Listas podem ser usadas como base para a criação de várias outras estruturas
;; de dados. Embora raramente uma implementação baseada em listas seja a mais
;; rápida, pode ser utilizada para conjuntos de dados pequenos e é fácil de criar
;; em uma linguagem funcional.

;; Uma estrutura de dados que pode ser construída em cima das listas são conjuntos.
;; Conjuntos são similares às listas, mas podem ter apenas uma ocorrência de cada
;; elemento. Algumas operações normalmente usadas com conjuntos são a união,
;; intersecção, diferença de conjuntos e teste de pertencimento.

;; Para o teste de pertencimento podemos continuar usando a receita recursiva baseada
;; na estrutura das listas:

;; Escreva uma função pertence? tal que
;; (pertence? x lst) retorna #t se o elemento x aparece na lista (conjunto) lst
(define (pertence? x lst)
  (if (empty? lst) #f
      (if (equal? x (first lst)) #t
          (pertence? x (rest lst)))))

(define-test-suite test-pertence?
  (test-false "lista vazia"    (pertence? 5 '()))
  (test-true  "3 pertence"     (pertence? 3 '(1 2 3 4 5)))
  (test-false "9 não pertence" (pertence? 9 '(1 2 3 4 5)))
  (test-true  "5 pertence"     (pertence? 5 '(1 2 3 4 5)))
  (test-true  "lista de lista" (pertence? '(1 2) (list '(1 2) '(2 3) '(3 4)))))


;; --- Questão 5 ----------------------------

;; Infelizmente nem sempre podemos usar a mesma receita para recursividade. Um caso
;; comum são funções que devem combinar duas listas de alguma forma (como é o caso
;; das operações de união, intersecção e diferença de conjuntos).

;; Para praticar a ideia primeiro, escreva uma função combine tal que
;; (combine l1 l2) retorna uma lista de pares (listas de dois elementos) onde o primeiro
;; elemento de cada par vem de l1 e o segundo de l2. O número de pares deve ser igual ao
;; tamanho da menor lista. Veja os testes para exemplos.
(define (combine l1 l2)
  (if (empty? l1) '()
      (if (empty? l2) '()
          ;;(cons (list (first l1) (first l2)) (combine (rest l1) (rest l2))))))
          (tail-combine l1 l2 '()))))
(define (tail-combine l1 l2 acm)
  (cond
    [(empty? l1) (reverse acm)]
    [(empty? l2) (reverse acm)]
    [(tail-combine (rest l1) (rest l2) (cons (list (first l1) (first l2)) acm))]))

(define-test-suite test-combine
  (test-equal? "listas de mesmo tamanho"
               (combine '(1 2 3) '(10 20 30))  '((1 10) (2 20) (3 30)))
  
  (test-equal? "listas de tamanho diferente"
               (combine '(1)     '(55 33 11))  '((1 55)))
  
  (test-equal? "primeira lista vazia"
               (combine '()      '(1 2 3))     '())
  
  (test-equal? "segunda lista vazia"
               (combine '(1 2 3) '())          '())
  
  (test-equal? "segunda lista menor"
               (combine '(4 5 6) '(22 33))     '((4 22) (5 33))))


;; --- Questão 6 ----------------------------

;; Antes de trabalhar com conjuntos, é interessante ter algumas funções de apoio.

;; Além da falta de itens duplicados, outra característica dos conjuntos é a
;; ausência de ordem. As listas (1 2 3), (3 1 2), (2 3 1) etc todas representam
;; o mesmo conjunto. Por isso, não podemos usar equal? para testar igualdade de
;; conjuntos.

;; Mesmo nos testes, podemos ter diferentes implementações das operações de conjuntos,
;; ambas corretas, mas que retornam os elementos em uma ordem diferente (por
;; exemplo (uniao '(1 2 5) (2 5 3)) pode retornar (1 2 3 5) ou (3 2 1 5), ambos
;; os resultados corretos).

;; Escreva uma função conjunto=? tal que
;; (conjunto=? c1 c2) retorna #t se c1 e c2 contêm os mesmos elementos, não
;; necessariamente na mesma ordem, e #f caso exista algum elemento que pertence
;; a um mas não a outro.

(define (subconjunto? c1 c2)
    (if (empty? c1) #t
        (if (pertence? (first c1) c2) (subconjunto? (rest c1) c2) #f)))

(define (conjunto=? c1 c2)
  (if (and (subconjunto? c1 c2) (subconjunto? c2 c1)) #t #f))
  

(define-test-suite test-conjunto=?
  (test-true  "conjuntos vazios"        (conjunto=? '() '()))
  (test-false "vazio e unitário"        (conjunto=? '() '(1)))
  (test-true  "conjs. unitários"        (conjunto=? '(1) '(1)))
  (test-true  "iguais, mesma ordem"     (conjunto=? '(1 2 3) '(1 2 3)))
  (test-true  "iguais, ordem diferente" (conjunto=? '(1 2 3) '(1 3 2)))
  (test-true  "ordem diferente"         (conjunto=? '(2 1 3) '(2 3 1)))
  (test-false "(1 2 3) e (1 2 5)"       (conjunto=? '(1 2 3) '(1 2 5)))
  (test-false "(3 2 1) e (1 3 7)"       (conjunto=? '(3 2 1) '(1 3 7))))


;; --- Questão 7 ----------------------------

;; Outra função de apoio que pode ser útil é uma que, dada uma
;; lista qualquer (podendo conter itens duplicados) retorna uma lista válida como
;; conjunto, sem itens duplicados. Podemos chamar essa função remove-duplicatas.

;; Escreva a função remove-duplicatas tal que
;; (remove-duplicatas lst) retorna uma lista com os mesmos elementos de lst mas
;; sem que nenhum item ocorra mais de uma vez.
(define (remove-duplicatas lst)
  (if (empty? lst) '()
      ;;(if (empty? (rest lst)) lst
          ;;(if (pertence? (first lst) (rest lst)) (remove-duplicatas (rest lst))
              ;;(cons (first lst) (remove-duplicatas (rest lst)))))))
      (tail-remove-duplicatas lst '())))

(define (tail-remove-duplicatas lst acm)
  (cond
    [(empty? lst) (reverse acm)]
    [(pertence? (first lst) (rest lst)) (tail-remove-duplicatas (rest lst) acm)]
    [(tail-remove-duplicatas (rest lst) (cons (first lst) acm))]))

;; Um outro nome para a mesma função poderia ser lista->conjunto, enfatizando a
;; sua aplicação na criação de conjuntos a partir de listas. Nesse caso podemos
;; definir um sinônimo para a mesma função acima
(define lista->conjunto remove-duplicatas)

;; Note que usamos conjunto=? nos testes, caso contrário funções que retornassem
;; elementos em ordens diferentes não passariam
(define-test-suite test-remove-duplicatas
  (test-true "sem duplicatas"
             (conjunto=? (remove-duplicatas '(1 2 3 4 5)) '(1 2 3 4 5)))
  
  (test-true "lista vazia"
             (conjunto=? (remove-duplicatas '()) '()))
  
  (test-true "várias duplicatas"
             (conjunto=? (remove-duplicatas '(1 2 3 2 3 5)) '(1 2 3 5)))
  
  (test-true "apenas um elemento"
             (conjunto=? (lista->conjunto   '(5 5 5 5 5 5)) '(5)))
  
  (test-true "mais repetições"
             (conjunto=? (lista->conjunto '(1 2 3 1 2 3 1 2 3 1 2 3)) '(1 2 3))))

  
;; --- Questão 8 ----------------------------

;; Agora vamos implementar as operações de conjuntos implementados com listas.

;; Escreva a função uniao tal que
;; (uniao c1 c2) retorna um conjunto contendo os elementos de c1 e c2, sem duplicações.
(define (uniao c1 c2)
  (if (empty? c1) c2
      (if (empty? c2) c1
          (remove-duplicatas (append c1 c2)))))

;; Dica: com o que vimos até agora tem pelo menos duas maneiras de escrever essa função.
;; Uma forma é uma função recursiva que tem que eliminar itens duplicados a cada passo.
;; Outra forma seria combinar os dois conjuntos primeiro e remover as duplicatas só no
;; final. É interessante (mas opcional) tentar fazer das duas formas.

(define-test-suite test-uniao
  (test-true "Vazio é elemento neutro 1"
             (conjunto=? (uniao '() '(1 2 3))  '(1 2 3)))
  
  (test-true "Vazio é elemento neutro 2"
             (conjunto=? (uniao '(4 5 6) '())  '(4 5 6)))
  
  (test-true "União de vazios"
             (conjunto=? (uniao '() '())  '()))
  
  (test-true "Sem elementos em comum"
             (conjunto=? (uniao '(1 2 3) '(4 5 6))  '(1 2 3 4 5 6)))
  
  (test-true "Com elementos em comum"
             (conjunto=? (uniao '(1 4 5) '(4 5 6))  '(1 4 5 6))))


;; --- Questão 9 -----------------------

;; Escreva uma função interseccao tal que
;; (interseccao c1 c2) retorna um conjunto contendo os elementos que ocorrem
;; em ambos c1 e c2
(define (interseccao c1 c2)
  (if (empty? c1) '()
      (if (empty? c2) '()
          ;;(if (pertence? (first c1) c2) (cons (first c1) (interseccao (rest c1) c2))
              ;;(interseccao (rest c1) c2)))))
          (tail-interseccao c1 c2 '()))))

(define (tail-interseccao c1 c2 acm)
  (cond
    [(empty? c1) (reverse acm)]
    [(empty? c2) (reverse acm)]
    [(pertence? (first c1) c2) (tail-interseccao (rest c1) c2 (cons (first c1) acm))]
    [(tail-interseccao (rest c1) c2 acm)]))

(define-test-suite test-interseccao
  (test-equal? "Conjuntos vazios"        (interseccao '()      '())      '())
  (test-equal? "Intersecção com vazio 1" (interseccao '(1 2 3) '())      '())
  (test-equal? "Intersecção com vazio 2" (interseccao '()      '(11 22)) '())
  (test-equal? "Sem elementos comuns"    (interseccao '(1 2 3) '(11 22)) '())

  (test-true "Um elemento em comum"
             (conjunto=? (interseccao '(1 2 3) '(11 1 121))  '(1)))

  (test-true "Vários elementos em comum"
             (conjunto=? (interseccao '(1 3 5 7 9 11)
                                      '(11 3 1 13 17))
                         '(1 3 11)))

  (test-true "Mesmo conjunto"
             (conjunto=? (interseccao '(1 2 3 4 5) '(5 4 3 2 1))
                         '(1 2 3 4 5))))


;; --- Questão 10 -----------------------

;; Escreva uma função diferenca tal que
;; (diferenca c1 c2) retorna um conjunto que tem todos os elementos de c1 que
;; não pertencem a c2. Por exemplo, (diferenca '(1 3 5 7) '(3 7)) deve retornar
;; '(1 5) (não necessariamente nesta ordem).
(define (diferenca c1 c2)
  (cond
    [(empty? c1) c2]
    [(empty? c2) c1]
    [(tail-diferenca c1 c2 c1 '())]))

(define (tail-diferenca c1 c2 backup acm)
  (cond
    [(empty? c1)
     (if (equal? c2 backup) (remove-duplicatas (reverse acm))
         (tail-diferenca c2 backup backup acm))]
    [(pertence? (first c1) c2) (tail-diferenca (rest c1) c2 backup acm)]
    [(tail-diferenca (rest c1) c2 backup (cons (first c1) acm))]))

;; Para esta função, escreva também um conjunto de testes, e adicione a suite de 
;; testes criados à execução de todos os testes, abaixo. Você pode escrever os
;; testes antes ou depois de implementar a função.

(define-test-suite test-diferenca
  (test-equal? "conjuntos vazios" (diferenca '() '()) '())
  (test-equal? "primeiro conjunto vazio" (diferenca '() '(1 2 3)) '(1 2 3))
  (test-equal? "segundo conjunto vazio" (diferenca '(1 2 3) '()) '(1 2 3))
  (test-equal? "conjuntos distintos" (diferenca '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
  (test-equal? "conjunto de conjuntos" (diferenca (list '(1 2) '(3 4) 1 2 3) (list '(1 2) 1 3 2 5)) (list '(3 4) 5)))


;; --- Executa todos os testes ---------
(run-tests
 (test-suite "todos os testes"
             test-remove-primeiro
             test-remove-todos
             test-lista-generica
             test-pertence?
             test-combine
             test-conjunto=?
             test-remove-duplicatas
             test-uniao
             test-interseccao
             test-diferenca
             ))
