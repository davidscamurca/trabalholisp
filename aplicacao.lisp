;;O Seguinte programa é o tradicional Jogo da Velha, que contem respostas baseadas em árvores com possibilidades de jogadas

;;O programa deve cumprir os seguintes requisitos.

;;Ser simples e elegante
;;Ocupa poucas linhas de codigo
;;Ser minimalista em sua interfaçe gráfica
;;Utilizar recursividade

(defvar *tabuleiro*)
(defvar *arvore*)
(defvar *ganhador*)
(defvar *niveis*)
(defvar *letra-user*)
(defvar *letra-ia*)

(setq *tabuleiro* '(n n n n n n n n n))
(setq *ganhador* nil)
(setq *niveis* 8)
(setq *letra-user* 'o)
(setq *letra-ia* 'x)

(defun gera-arvore (tabuleiro letra nivel) ;função que gera uma lista de possibilidades de jogadas: coloca a letra no tabuleiro
	(let 
		((lista nil)
		  (posicao 0)
		  (copia nil)
		  (sublista nil))

	(when (> nivel 0)
	  (progn
	(decf nivel) ;decrementa 1 do nível
	(loop
	  (when (= posicao 9) (return))

	  (setq m (nth posicao tabuleiro))
	  (when (equal m 'n)
	  	(progn
	  		(setf copia (subseq tabuleiro 0 9)) ;fazendo uma cópia do tabuleiro atual
	  		(setf (nth posicao copia) letra) ;coloca a letra em seu lugar, a letra e X ou O

	  		(when (not (ganhar tabuleiro letra)) ;se não ganhou, tem que adicionar as sublistas geradas

	  		(if (equal letra 'x)
	  			(setf sublista (gera-arvore copia 'o nivel))
	  			(setf sublista (gera-arvore copia 'x nivel))))
	  	
	  		(when (not (null sublista)) (push sublista lista)) ;copia a sub-árvore gerada para colocar em outra letra;


	  		(push copia lista))) ;adiciona a lista na jogada
	  	(incf posicao))
	lista))))

;função para mostrar árvore
(defun mostrar (arvore)
	(when (not (null arvore))
		(when (listp (first arvore))
			(progn 
		(princ (first arvore))
		(mostrar (car (rest arvore)))
		(mostrar (car (rest arvore)))))))

;funcões para buscar ganhadores 
(defun ganhadores (arvore letra) ;gera uma lista que contém os tabuleiros ganhadores de um ramo segundo a letra
	(let
		((tabuleiro nil)
		 (lista nil))

		(dolist (tabuleiro arvore)
			(when (not (listp (first tabuleiro)))
			(when (ganhar tabuleiro letra)
				(push tabuleiro lista))))
		lista))

;função que devolve os NOs raiz e as jogadas ganhadoras
(defun raiz-ganhador-nivel-m (letra nivel)
	(let
		((ramos nil)
		 (ramos-m nil)
		 (arvore *arvore*)
		 (no nil)
		 (no-m nil)
		 (ganhadores-m nil)
		 (ganhadores-user nil))

	(setq *ganhador* nil)
	(loop
		(setq no (first arvore));Nó Raiz

		(when (null no)(return))
		(setq arvore (remove no arvore))

		(setq ramos (first arvore))
		(setq arvore (remove ramos arvore))

		(if (= nivel 1)
		(setq ganhadores-m (ganhadores ramos letra))
		(setq ganhadores-m (ganhadores-nivel-m ramos letra nivel)))

		(when (not (null ganhadores-m))(return)))
	(values no ganhadores-m)))

;função que verifica um nivel e retorna os ganhadores dele mesmo
(defun ganhadores-nivel-m (ramos letra nivel)
	(let 
		((no-m nil)
		 (ramos-n nil) ;;ramos de um nivel subsequente em pronfundidade 
		 (ramos-m ramos)
		 (ganhadores-user nil))

	(loop
		(setq no-m (first ramos-m))
		(when (null no-m) (return))
		(setq ramos-m (remove no-m ramos-m))
		(setq ramos-n (first ramos-m))
		(setq ramos-m (remove ramos-n ramos-m))

		(if (= nivel 2)
		(progn 
			(setq lista-ganhadores (ganhadores ramos-n letra))
			(when (not (null lista-ganhadores))
				(progn (setq *ganhador* T) (return))))


		(progn 
			(decf nivel)
			(when (not (null ramos-n))
				(when (listp (first ramos-n))
				(ganhadores-nivel-m ramos-n letra nivel)))))

		(when *ganhador* (return)))


	lista-ganhadores))

;função devolve o Nó raiz da sub-jogada, sem ganhadores humano para um unico sub-nivel
(defun raiz-no-ganhador ()
	(let
		((ramos-m *arvore*)
		 (ramos-n nil) ;ramos de um nivel subsequente
		 (no nil)
		 (ganhadores-user nil))
	(loop
		(setq no (first ramos-m));nó Raiz

		(when (null no)(return))
		(setq ramos-m (remove no ramos-m))

		(setq ramos-n (first ramos-m))
		(setq ramos-m (remove ramos-n ramos-m))

	;agora verificamos se não tem jogadas ganhadoras nos ramos-m

		(setq ganhadores-user (ganhadores ramos-n *letra-user*))
		(when (null ganhadores-user)(return))); caso contrário, continue a iteração


	no));devolve o NO raiz quando os subniveis "ramos-m" nao conter ganhadores para o usuario


;funcao verifica que tem vencedores para o usuario apos a jogada da maquina *******
(defun existe-user-ganhador ()
	(let 
		((ramos-m *arvore*)
		 (ramos-n nil);ramos de um nível subseguinte
		 (no nil)
		 (ganhadores-user nil)
		 (tem-ganhadores-user nil))
		
	(loop 
		(setq no (first ramos-m));nó raiz
		
		(when (null no)(return))
		(setq ramos-m (remove no ramos-m))

		(setq ramos-n (first ramos-m))
		(setq ramos-m (remove ramos-n ramos-m))

		(setq ganhadores-user (ganhadores ramos-n *letra-user*))

		(when (not (null ganhadores-user)) (setq tem-ganhadores-user T))) ; Se ouver usuario ganhador e verdadeiro T


	tem-ganhadores-user))

;regras
;função que devolve verdadeiro T se as posições coincidirem com a letra
(defun na-linha (tabuleiro letra posicao-1 posicao-2 posicao-3)

	(and (equal letra (nth posicao-1 tabuleiro))
		 (equal letra (nth posicao-2 tabuleiro))
		 (equal letra (nth posicao-3 tabuleiro))))

;funcão que verifica se os jogadores ganharam de acordo com a letra

(defun ganhar (tabuleiro letra)

	(or (na-linha tabuleiro letra 0 1 2);horizontais
		(na-linha tabuleiro letra 3 4 5)
		(na-linha tabuleiro letra 6 7 8)
		(na-linha tabuleiro letra 0 3 6);verticais
		(na-linha tabuleiro letra 1 4 7)
		(na-linha tabuleiro letra 2 5 8)
		(na-linha tabuleiro letra 0 4 8);diagonais
		(na-linha tabuleiro letra 2 4 6)))

;funca que realiza a jogada pelo computador
(defun ia-joga ()
	(let 
		((nivel 1)
		 (jogada nil)
		 (raizes-ganhadoras nil))

	(setq *arvore* (gera-arvore *tabuleiro* *letra-ia* *niveis*)) ;;gera as possíveis jogadas

	(setq raizes-ganhadoras (ganhadores *arvore* *letra-ia*))
	(when (not (null raizes-ganhadoras)); Posso ganhar em uma única jogada?
		(setq jogada (first raizes-ganhadoras))) ; tem que verificar quais raizes são ganhadoras

	(when (null jogada)
		(when (existe-user-ganhador) ;há usuários vencendores para o proximo jogo?
	(setq jogada (raiz-no-ganhador)))) ;se há usuários vencedores para o proximo jogo, retorna o no raiz.


	(when (null jogada) ; se a jogada é nula, procure um caminho para ganhar
		(loop
		(when (= nivel 9)(return)) ;busca jogadas com até oito níveis
		(setq jogada (multiple-value-bind (raiz ganhadores) (raiz-ganhador-nivel-m *letra-ia* nivel);buscar se ganha no nivel
				(when (not (null ganhadores)) raiz))) ; se há ganhadores no nivel, devolve o NO raiz
		(when (not (null jogada))(return));se uma jogada sair do circuito*********
		(incf nivel)))


	(when (null jogada) (setq jogada (first *arvore*))); se toda jogadar for nula, faça uma jogada boba

	(when (not (null jogada)) (setq *tabuleiro* jogada))

	(mostrar-tabuleiro)
	(when (ganhar *tabuleiro* *letra-ia*) (format t "~&Perdeu..."))))

;funcao que realiza a jogada do usuário

(defun user-joga (linha coluna)

	(let
		()
		(when (< linha 4)
			(when (< coluna 4)

		(progn 
			(decf linha)
			(decf coluna)
			(setq posicao (+ (* 3 linha) coluna));me da uma posicao entre 0 e 8;
			(setf (nth posicao *tabuleiro*) *letra-user*); coloca a letra no tabuleiro
			(mostrar-tabuleiro)
			(when (ganhar *tabuleiro* *letra-user*) (format t "~&Ganhou..")))))))


;funcao que mostra o tabuleira na tela
(defun mostrar-tabuleiro()
	(let 
		((tabuleiro *tabuleiro*))

	(setq x "x")
	(setq o "o")
	(setq n "n")
	(format t "~&| ~s ~s ~s | ~&| ~s ~s ~s | ~&| ~s ~s ~s |"
		(nth 0 tabuleiro)
		(nth 1 tabuleiro)
		(nth 2 tabuleiro)
		(nth 3 tabuleiro)
		(nth 4 tabuleiro)
		(nth 5 tabuleiro)
		(nth 6 tabuleiro)
		(nth 7 tabuleiro)
		(nth 8 tabuleiro))))


;funcao para criar novo jogo
(defun criar-novo-jogo ()
	(let 
			()
		(setq *tabuleiro* '(n n n n n n n n n))
		(mostrar-tabuleiro)))
