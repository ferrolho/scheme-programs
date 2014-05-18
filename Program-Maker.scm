
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                ;;
;;  READ ME !!                                                                    ;;
;;  Para começar o programa é escrever (god)                                      ;;
;;                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cenas ;;
;;;;;;;;;;;

(define cabecalho
  (lambda ()
    (begin (newline)
           (display ":::::::::::::::::::::::::::::::::::::::::::")(newline)
           (display "::                                       ::")(newline)
           (display "::        Program Maker - alpha          ::")(newline)
           (display "::                                       ::")(newline)
           (display ":::::::::::::::::::::::::::::::::::::::::::")(newline))))

(define inicio-apres
  (lambda ()
    (begin (newline)
           (display "-------------------------------------------")(newline)
           (display "               O teu programa              ")(newline)
           (display "-------------------------------------------")(newline)
           (newline))))

(define fim-faz-programa-god
  (lambda ()
    (begin (newline)(newline)
           (display "-------------------------------------------")(newline)
           (display "--                  Fim                  --")(newline)
           (display "-------------------------------------------"))))

(define continuar-god
  (lambda ()
    (let ((continuar (begin (display "Escreve algo para continuar:")
                            (newline)
                            (read))))
      (god))))

(define termina-god
  (lambda ()
    (begin (newline)
           (display "-------------------------------------------")(newline)
           (display "--          One does not simply          --")(newline)
           (display "--           terminate GOD !!!           --")(newline)
           (display "-------------------------------------------")(newline))))

(define valor-escolhido-incorrecto
  (lambda ()
    (display "A opção selecionada não existe.")(newline)
    (display "Seleciona uma opção da lista.")(newline)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programa Principal ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define god
  (lambda ()
    (cabecalho)(newline)
    (let ((que-fazer (begin (display "Seleciona uma das opções:")(newline)
                            (display "1. Faz-me já um procedimento!")
                            (newline)
                            (display "2. About God")
                            (newline)
                            (display "3. Terminar God")
                            (newline)(read))))
      (cond ((equal? que-fazer
                     1)
             (pagina-principal))
            ((equal? que-fazer
                     2)
             (detalhes-programa-god))
            ((equal? que-fazer
                     3)
             (termina-god))
            (else
             (newline)
             (valor-escolhido-incorrecto)
             (god))))))

;;;;;;;;;;;;;;;;;;;;;;;;

(define pagina-principal
  (lambda ()
    (let ((nome-procedimento (begin (newline)
                                    (display "Como se vai chamar o procedimento?")
                                    (newline)(read)))
          (programa (begin (newline)
                           (display "Seleciona o programa:")(newline)
                           (display "- Outputs")(newline)
                           (display "0. Visualizador")(newline)
                           (display "- Matemáticas:")(newline)
                           (display "1. Soma            2. Subtração")(newline)
                           (display "3. Multiplicação   4. Divisão")(newline)
                           (display "5. Função          ")(newline)
                           (display "- Listas:")(newline)
                           (display "6. Lista           7. Construtores")(newline)
                           (display "8. Selectores      ")(newline)
                           (read))))
      (cond ((equal? programa
                     0)
             (termos-faz-programa-visualizador nome-procedimento))
            ((equal? programa
                     1)
             (faz-programa-soma nome-procedimento))
            ((equal? programa
                     2)
             (faz-programa-subtracao nome-procedimento))
            ((equal? programa
                     3)
             (faz-programa-multiplicacao nome-procedimento))
            ((equal? programa
                     4)
             (faz-programa-divisao nome-procedimento))
            ((equal? programa
                     5)
             (faz-programa-funcao nome-procedimento))
            ((equal? programa
                     6)
             (faz-programa-lista nome-procedimento))
            ((equal? programa
                     7)
             (faz-programa-construtor nome-procedimento))
            ((equal? programa
                     8)
             (faz-programa-selector nome-procedimento))
            (else
             (newline)
             (valor-escolhido-incorrecto)
             (pagina-principal))))))

;;;;;;;;;;;;;;;;;;;;;;;;

(define detalhes-programa-god
  (lambda ()
    (begin (newline)
           (display "-------------------------")(newline)
           (display "--  Versão: alpha      --")(newline)
           (display "--  Henrique Ferrolho  --")(newline)
           (display "--  FEUP - MIEIC       --")(newline)
           (display "--  2012/2013          --")(newline)
           (display "-------------------------")(newline)
           (god))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programa 1 - Visualizador ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define termos-faz-programa-visualizador
  (lambda (nome-procedimento)
    (newline)
    (display "Quantos termos tem o visualizador?")(newline)
    (let* ((termos (begin (display "Insere o numero de elementos:")
                          (newline)
                          (read)))
           (lista-termos (faz-lista-termos-n-termos '() 0 termos)))
      (faz-programa-visualizador nome-procedimento '() lista-termos))))


(define faz-lista-termos-n-termos
  (lambda (lista i maximo)
    (cond ((= i 0)
           (faz-lista-termos-n-termos
            (append lista
                    (list (begin (display "Insere o primeiro termo:")
                                 (newline)
                                 (read))))
            (add1 i) maximo))
          ((= i (sub1 maximo))
           (faz-lista-termos-n-termos
            (append lista
                    (list (begin (display "Insere o último termo:")
                                 (newline)
                                 (read))))
            (add1 i) maximo))
          ((< i maximo)
           (faz-lista-termos-n-termos
            (append lista
                    (list (begin (display "Insere o próximo termo:")
                                 (newline)
                                 (read))))
            (add1 i) maximo))
          (else lista))))


(define faz-programa-visualizador
  (lambda (nome-procedimento lista lista-termos)
    (newline)
    (display "O que queres que seja visualizado?")(newline)
    (display "1. Texto fixo       2. Procedimento")(newline)
    (display "3. Termo")(newline)(newline)
    (display "4. Espaço           5. <newline>")(newline)
    (display "6. Vírgula          7. Ponto")(newline)(newline)
    (display "X. Terminar visualização")(newline)
    (let ((a (read)))
      (cond ((equal? a 1)
             (begin (newline)
                    (display "Escreve o texto:")(newline)
                    (let ((y (read)))
                      (if (number? y)
                          (faz-programa-visualizador
                           nome-procedimento
                           (append lista
                                   (list (number->string y)))
                           lista-termos)
                          (faz-programa-visualizador
                           nome-procedimento
                           (append lista
                                   (list (symbol->string y)))
                           lista-termos)))))
            ((equal? a 2)
             (begin (newline)
                    (display "Escreve o nome do procedimento:")(newline)
                    (faz-programa-visualizador
                     nome-procedimento
                     (append lista
                             (list 'procedimento-a-seguir!
                                   (read)
                                   (begin
                                     (display "Escreve os termos do procedimento:")
                                     (newline)
                                     (read))))
                     lista-termos)))
            ((equal? a 3)
             (begin (newline)
                    (display "Especifica o termo:")(newline)
                    (faz-programa-visualizador nome-procedimento
                                               (append lista
                                                       (list 'termo-a-seguir!
                                                             (read)))
                                               lista-termos)))
            ((equal? a 4)
             (begin (newline)
                    (display "Adicionar espaço: OK!")(newline)
                    (faz-programa-visualizador nome-procedimento
                                               (append lista
                                                       (list 'adicionar-espaco))
                                               lista-termos)))
            ((equal? a 5)
             (begin (newline)
                    (display "Mudança de linha: OK!")(newline)
                    (faz-programa-visualizador nome-procedimento
                                               (append lista
                                                       (list '(newline)))
                                               lista-termos)))
            ((equal? a 6)
             (begin (newline)
                    (display "Adicionar vírgula: OK!")(newline)
                    (faz-programa-visualizador nome-procedimento
                                               (append lista
                                                       (list 'adicionar-virgula))
                                               lista-termos)))
            ((equal? a 7)
             (begin (newline)
                    (display "Adicionar ponto: OK!")(newline)
                    (faz-programa-visualizador nome-procedimento
                                               (append lista
                                                       (list 'adicionar-ponto))
                                               lista-termos)))
            ((or (equal? a 'x)
                 (equal? a 'X))
             (begin (newline)
                    (display "Terminar o visualizador.")(newline)
                    (inicio-apres)
                    (display "(define ")(display nome-procedimento)(newline)
                    (display "  (lambda ")
                    (display lista-termos)
                    (interpreta-lista-visual lista))))
      (else
       (faz-programa-visualizador nome-procedimento lista lista-termos)))))


(define interpreta-lista-visual
  (lambda (lista)
    (if (null? lista)
        (begin (display "))")
               (fim-faz-programa-god)
               (newline)(continuar-god))
        (cond ((equal? (car lista)
                       '(newline))
               (begin (newline)(display "    ")
                      (display "(newline)")
                      (interpreta-lista-visual (cdr lista))))
              ((equal? (car lista)
                       'adicionar-espaco)
               (begin (display "(display \" \")")
                      (interpreta-lista-visual (cdr lista))))
              ((equal? (car lista)
                       'adicionar-virgula)
               (begin (display "(display \",\")")
                      (interpreta-lista-visual (cdr lista))))
              ((equal? (car lista)
                       'adicionar-ponto)
               (begin (display "(display \".\")")
                      (interpreta-lista-visual (cdr lista))))
              ((equal? (car lista)
                       'procedimento-a-seguir!)
               (begin (newline)(display "    ")
                      (display "(display (")(display (cadr lista))
                      (display " ")(display (caddr lista))(display "))")
                      (interpreta-lista-visual (cdddr lista))))
              ((equal? (car lista)
                       'termo-a-seguir!)
               (begin (newline)(display "    ")
                      (display "(display ")(display (cadr lista))
                      (display ")")
                      (interpreta-lista-visual (cddr lista))))
              (else
               (begin (newline)(display "    ")
                      (display "(display \"")(display (car lista))
                      (display "\")")
                      (interpreta-lista-visual (cdr lista))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programa 2 - Soma ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define faz-programa-soma
  (lambda (nome-procedimento)
    (newline)
    (display "Soma de quantas parcelas?")(newline)
    (let ((parcelas (begin (display "Insere o numero de termos:")(newline)
                           (read))))
      (inicio-apres)
      (display "(define ")(display nome-procedimento)(newline)
      (display "  (lambda ")
      (let ((lista-lambda (letrec ((aux
                                    (lambda (parcelas nome-parc)
                                      (if (equal? 0
                                                  parcelas)
                                          ()
                                          (append (termos nome-parc)
                                                  (aux (sub1 parcelas)
                                                       (add1 nome-parc)))))))
                            (aux parcelas 1))))
        (display lista-lambda)(newline)
        (display "    ")(display (append (list '+)
                                         lista-lambda))
        (display "))"))
      (fim-faz-programa-god)
      (newline)(continuar-god))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programa 3 - Subtração ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define faz-programa-subtracao
  (lambda (nome-procedimento)
    (newline)
    (display "Subtração de quantas parcelas?")(newline)
    (let ((parcelas (begin (display "Insere o numero de termos:")(newline)
                           (read))))
      (inicio-apres)
      (display "(define ")(display nome-procedimento)(newline)
      (display "  (lambda ")
      (let ((lista-lambda (letrec ((aux
                                    (lambda (parcelas nome-parc)
                                      (if (equal? 0
                                                  parcelas)
                                          ()
                                          (append (termos nome-parc)
                                                  (aux (sub1 parcelas)
                                                       (add1 nome-parc)))))))
                            (aux parcelas 1))))
        (display lista-lambda)(newline)
        (display "    ")(display (append (list '-)
                                         lista-lambda))
        (display "))"))
      (fim-faz-programa-god)
      (newline)(continuar-god))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programa 4 - Multiplicação ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define faz-programa-multiplicacao
  (lambda (nome-procedimento)
    (newline)
    (display "Multiplicação de quantas parcelas?")(newline)
    (let ((parcelas (begin (display "Insere o numero de termos:")(newline)
                           (read))))
      (inicio-apres)
      (display "(define ")(display nome-procedimento)(newline)
      (display "  (lambda ")
      (let ((lista-lambda (letrec ((aux
                                    (lambda (parcelas nome-parc)
                                      (if (equal? 0
                                                  parcelas)
                                          ()
                                          (append (termos nome-parc)
                                                  (aux (sub1 parcelas)
                                                       (add1 nome-parc)))))))
                            (aux parcelas 1))))
        (display lista-lambda)(newline)
        (display "    ")(display (append (list '*)
                                         lista-lambda))
        (display "))"))
      (fim-faz-programa-god)
      (newline)(continuar-god))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programa 4 - Divisão ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define faz-programa-divisao
  (lambda (nome-procedimento)
    (newline)
    (display "Divisão de quantas parcelas?")(newline)
    (let ((parcelas (begin (display "Insere o numero de termos:")(newline)
                           (read))))
      (inicio-apres)
      (display "(define ")(display nome-procedimento)(newline)
      (display "  (lambda ")
      (let ((lista-lambda (letrec ((aux
                                    (lambda (parcelas nome-parc)
                                      (if (equal? 0
                                                  parcelas)
                                          ()
                                          (append (termos nome-parc)
                                                  (aux (sub1 parcelas)
                                                       (add1 nome-parc)))))))
                            (aux parcelas 1))))
        (display lista-lambda)(newline)
        (display "    ")(display (append (list '/)
                                         lista-lambda))
        (display "))"))
      (fim-faz-programa-god)
      (newline)(continuar-god))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programa 5 - Função ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define faz-programa-funcao
  (lambda (nome-procedimento)
    (display "Temporariamente Indisponível.")
    (god)))

(define faz-programa-funcao-temp
  (lambda (nome-procedimento)
    (newline)
    (display "Insere a função:")(newline)
    (let* ((x1 (begin (opcoes-introducao)
                      (read))))
      ())))

;(define faz-programa-funcao
;  (lambda (nome-procedimento)
;    (newline)
;    (display "Insere a função:")(newline)
;    (let* ((x1 (begin (opcoes-introducao)
;                      (read)))
;           (if ()
;               ()
;               ()))
;      ())))

(define opcoes-introducao
  (lambda ()
    (display "1. termo         3. operador")(newline)
    (display "2. constante     4. terminar")(newline)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programa 6 - lista ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define faz-programa-lista
  (lambda (nome-procedimento)
    (newline)
    (display "Quantos elementos tem a lista?")(newline)
    (let* ((elementos (begin (display "Insere o numero de elementos:")
                             (newline)
                             (read)))
           (lista-elementos (faz-lista-n-elementos '() 0 elementos)))
      (inicio-apres)
      (display "(define ")(display nome-procedimento)(newline)
      (display "  (lambda ")(display lista-elementos)(newline)
      (display "    ")(display (append (list 'list) lista-elementos))
      (display "))")
      (fim-faz-programa-god)
      (newline)
      (continuar-god))))

(define faz-lista-n-elementos
  (lambda (lista i maximo)
    (cond ((= i 0)
           (faz-lista-n-elementos
            (append lista
                    (list (begin (display "Insere o primeiro elemento:")
                                 (newline)
                                 (read))))
            (add1 i) maximo))
          ((= i (sub1 maximo))
           (faz-lista-n-elementos
            (append lista
                    (list (begin (display "Insere o último elemento:")
                                 (newline)
                                 (read))))
            (add1 i) maximo))
          ((< i maximo)
           (faz-lista-n-elementos
            (append lista
                    (list (begin (display "Insere o próximo elemento:")
                                 (newline)
                                 (read))))
            (add1 i) maximo))
          (else lista))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programa 7 - construtores ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define faz-programa-construtor
  (lambda (nome-procedimento)
    (newline)
    (display "Selecione uma das templates:")(newline)
    (display "1. (a . b)           2. (a (b . c))")(newline)
    (display "3. ((a . b) c)       ")(newline)
    (let ((template-construtor (begin (read))))
      (cond ((equal? template-construtor
                     1)
             (template-construtor-1 nome-procedimento))
            ((equal? template-construtor
                     2)
             (template-construtor-2 nome-procedimento))
            ((equal? template-construtor
                     3)
             (template-construtor-3 nome-procedimento))
            (else
             (display "Valor inserido não listado.")(newline)
             (faz-programa-construtor nome-procedimento))))))

(define template-construtor-1
  (lambda (nome-procedimento)
    (newline)
    (display "Insira o elemento da esquerda do par:")(newline)
    (let ((a (read))
          (b (begin (display "Insira o elemento da direita do par:")
                    (newline)(read))))
      (inicio-apres)
      (display "(define ")(display nome-procedimento)(newline)
      (display "  (lambda (")(display a)
      (display " ")(display b)(display ")")(newline)
      (display "    (cons ")(display a)(display " ")
      (display b)(display ")))")
      (fim-faz-programa-god)
      (newline)
      (continuar-god))))

(define template-construtor-2
  (lambda (nome-procedimento)
    (newline)
    (display "Insira o elemento da esquerda da lista:")(newline)
    (let ((a (read))
          (b (begin (display "Insira o elemento da esquerda do par:")
                    (newline)(read)))
          (c (begin (display "Insira o elemento da direita do par:")
                    (newline)(read))))
      (inicio-apres)
      (display "(define ")(display nome-procedimento)(newline)
      (display "  (lambda (")(display a)(display " ")
      (display b)(display " ")(display c)(display ")")(newline)
      (display "    (list ")(display a)(newline)
      (display "          (cons ")(display b)(newline)
      (display "                ")(display c)(display "))))")
      (fim-faz-programa-god)
      (newline)
      (continuar-god))))

(define template-construtor-3
  (lambda (nome-procedimento)
    (newline)
    (let ((a (begin (display "Insira o elemento da esquerda do par:")
                    (newline)(read)))
          (b (begin (display "Insira o elemento da direita do par:")
                    (newline)(read)))
          (c (begin (display "Insira o último elemento da lista:")
                    (newline)(read))))
      (inicio-apres)
      (display "(define ")(display nome-procedimento)(newline)
      (display "  (lambda (")(display a)(display " ")
      (display b)(display " ")(display c)(display ")")(newline)
      (display "    (list (cons ")(display a)(newline)
      (display "                ")(display b)(display ")")(newline)
      (display "          ")(display c)(display ")))")
      (fim-faz-programa-god)
      (newline)
      (continuar-god))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programa 8 - selectores ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define faz-programa-selector
  (lambda (nome-procedimento)
    (newline)
    (display "Insira o nome do termo:")(newline)
    (let ((termo (read))
          (x (begin (newline)
                    (display "Escolhe a letra que está na posição")
                    (newline)
                    (display "pretendida: (a b c d e)")(newline)(newline)
                    (display "Nota: a lista apresentada pode não")(newline)
                    (display "ser exactamente igual à tua.")(newline)
                    (newline)(display "Exemplo: Se a tua lista for")(newline)
                    (display "((nome . apelido) viagem) e quiseres")(newline)
                    (display "um selector de 'viagem' escreve 'b',")(newline)
                    (display "porque viagem é o segundo elemento")
                    (newline)(display "da lista. Se quisesses escolher")(newline)
                    (display "'apelido' escreve 'a'.")(newline)
                    (read)))
          (y (begin (newline)
                    (display "O elemento escolhido é:")(newline)
                    (display "1. Simples        2. Um par")(newline)
                    (read))))
      (cond ((equal? y 1)
             (cond ((equal? x 'a)
                    (continua-faz-programa-selector1 nome-procedimento
                                                     termo
                                                     'car
                                                     3))
                   ((equal? x 'b)
                    (continua-faz-programa-selector1 nome-procedimento
                                                     termo
                                                     'cadr
                                                     3))
                   ((equal? x 'c)
                    (continua-faz-programa-selector1 nome-procedimento
                                                     termo
                                                     'caddr
                                                     3))
                   ((equal? x 'd)
                    (continua-faz-programa-selector1 nome-procedimento
                                                     termo
                                                     'cadddr
                                                     3))
                   ((equal? x 'e)
                    (continua-faz-programa-selector1 nome-procedimento
                                                     termo
                                                     '"cadddr (cdr"
                                                     4))
                   (else
                    (newline)
                    (valor-escolhido-incorrecto)
                    (faz-programa-selector nome-procedimento))))
            ((equal? y 2)
             (begin (newline)
                    (display "O elemento do par que queres é o da:")(newline)
                    (display "1. Esquerda          2. Direita")(newline)
                    (let ((z (read)))
                      (cond ((equal? z 1)
                             (continua-faz-programa-selector2
                              nome-procedimento
                              termo
                              (cond ((equal? x 'a)
                                     'car)
                                    ((equal? x 'b)
                                     'cadr)
                                    ((equal? x 'c)
                                     'caddr)
                                    ((equal? x 'd)
                                     'cadddr)
                                    ((equal? x 'e)
                                     '"cadddr (cdr"))
                              '"car ("
                              4))
                            ((equal? z 2)
                             (continua-faz-programa-selector2
                              nome-procedimento
                              termo
                              (cond ((equal? x 'a)
                                     'car)
                                    ((equal? x 'b)
                                     'cadr)
                                    ((equal? x 'c)
                                     'caddr)
                                    ((equal? x 'd)
                                     'cadddr)
                                    ((equal? x 'e)
                                     '"cadddr (cdr"))
                              '"cdr ("
                              (if (equal? x 'e)
                                  5
                                  4)))
                            (else
                             (newline)
                             (valor-escolhido-incorrecto)
                             (faz-programa-selector nome-procedimento))))))
            (else
             (newline)
             (valor-escolhido-incorrecto)
             (faz-programa-selector nome-procedimento))))))

(define continua-faz-programa-selector1
  (lambda (nome-procedimento termo posicao n-parentesis)
    (inicio-apres)
    (display "(define ")(display nome-procedimento)(newline)
    (display "  (lambda (")(display termo)(display ")")(newline)
    (display "    (")(display posicao)(display " ")(display termo)
    (cond ((equal? n-parentesis 3)
           (display ")))"))
          ((equal? n-parentesis 4)
           (display "))))")))
    (fim-faz-programa-god)
    (newline)
    (continuar-god)))

(define continua-faz-programa-selector2
  (lambda (nome-procedimento termo posicao pos-par n-parentesis)
    (inicio-apres)
    (display "(define ")(display nome-procedimento)(newline)
    (display "  (lambda (")(display termo)(display ")")(newline)
    (display "    (")(display pos-par)(display posicao)
    (display " ")(display termo)
    (cond ((equal? n-parentesis 3)
           (display ")))"))
          ((equal? n-parentesis 4)
           (display "))))"))
          ((equal? n-parentesis 5)
           (display ")))))")))
    (fim-faz-programa-god)
    (newline)
    (continuar-god)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outras cenas importantes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Esta parte faz a lista dos termos depois do lambda

(define termos
  (lambda (termo)
    (cond ((equal? termo 1) (list 'x1))
          ((equal? termo 2) (list 'x2))
          ((equal? termo 3) (list 'x3))
          ((equal? termo 4) (list 'x4))
          ((equal? termo 5) (list 'x5))
          ((equal? termo 6) (list 'x6))
          ((equal? termo 7) (list 'x7))
          ((equal? termo 8) (list 'x8))
          ((equal? termo 9) (list 'x9))
          ((equal? termo 10) (list 'x10))
          ((equal? termo 11) (list 'x11))
          ((equal? termo 12) (list 'x12))
          ((equal? termo 13) (list 'x13))
          ((equal? termo 14) (list 'x14))
          ((equal? termo 15) (list 'x15))
          ((equal? termo 16) (list 'x16))
          ((equal? termo 17) (list 'x17))
          ((equal? termo 18) (list 'x18))
          ((equal? termo 19) (list 'x19)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;