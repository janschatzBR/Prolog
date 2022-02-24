
/* operadores*/
:- op(35,xf,[meta,atingmeta]).
:- op(30,xfx,[a,em]).
:- op(35,xfx,[transforma, nao_produz_circulos_em]).

/* concatena duas listas */
ap([],X,X) :- !.
ap([X|Y],Z,[X|W]) :- ap(Y,Z,W).

/* verifica se um elemento está na lista */
membro(X,[X|_]) :- !.
membro(X,[_|Y]) :- membro(X,Y).

ache_todos(X,Y,Z) :- bagof(X,Y,Z), !.
ache_todos(_,_,[]).

imprima(t(FN,GN,T)) :- imprima_trajet(T).

imprima_trajet([r(raiz, Raiz)]) :- !,
	write('Estado Inicial: '),
	write(Raiz),write('.').

imprima_trajet([r(Ramo,No)|R]) :- imprima_trajet(R), nl,
	write(Ramo),
	write(' - resultando em: '), nl,
	write(No),write('.').

resolva :- estado_inicial(E), calcule_hn(E,HN),
	busca([t(HN,0,[r(raiz,E)])], Solucao),
	imprima(Solucao), nl.

busca([T|_],Solucao) :- T atingmeta, !,Solucao = T.

busca([T|Fila], Solucao):-
	ache_todos(ExtensaoAteUmFilho,
	estende_ate_filho(T,ExtensaoAteUmFilho), Extensoes),
	ap(Fila, Extensoes, FilaEstendida),
	busca(FilaEstendida, Solucao).

estende_ate_filho(t(F,G,[r(Ramo,N)|Trajetoria]),t(F1,G1,[r(Op,Filho),r(Ramo,N)|Trajetoria])) :-
	operacao(Op, N , Filho),
	\+ Filho nao_produz_circulos_em Trajetoria,
	calcule_hn(NoFilho, HNFilho),
	calcule_custo(No, NoFilho,Custo),
	G1 is G+Custo, F1 is G1+HNFilho.

Estado nao_produz_circulos_em Trajetoria:-
	membro(r(Operacao,Estado),Trajetoria).

t(_,_,[r(Ramo,M)|_]) atingmeta :- M meta.

calcule_custo(Nodulo,NoduloFilho,Custo):-
	Custo is 1.
	
calcule_hn(N, HN):- S meta,
	pecas_iguais(N,S,HN).

pecas_iguais([H1|T1], [H2|T2],0).
pecas_iguais([], [], Count) :- 
    pecas_iguais(T1, T2, TCount),
    (H1 = H2, Count is TCount+2 ; H1 \= H2, Count is TCount).

/* 8-puzzle */
operacao(esquerda,[*,_,_,_,_,_,_,_,_],[_,*,_,_,_,_,_,_,_]).
operacao(cima,[*,_,_,_,_,_,_,_,_],[_,_,_,*,_,_,_,_,_]).

operacao(direita,[_,*,_,_,_,_,_,_,_],[*,_,_,_,_,_,_,_,_]).
operacao(cima,[_,*,_,_,_,_,_,_,_],[_,_,_,_,*,_,_,_,_]).
operacao(esquerda,[_,*,_,_,_,_,_,_,_],[_,_,*,_,_,_,_,_,_]).

operacao(direita,[_,_,*,_,_,_,_,_,_],[_,*,_,_,_,_,_,_,_]).
operacao(cima,[_,_,*,_,_,_,_,_,_],[_,_,_,_,_,*,_,_,_]).

operacao(baixo,[_,_,_,*,_,_,_,_,_],[*,_,_,_,_,_,_,_,_]).
operacao(esquerda,[_,_,_,*,_,_,_,_,_],[_,_,_,_,*,_,_,_,_]).
operacao(cima,[_,_,_,*,_,_,_,_,_],[_,_,_,_,_,_,*,_,_]).

operacao(cima,[_,_,_,_,*,_,_,_,_],[_,_,_,_,_,_,_,*,_]).
operacao(baixo,[_,_,_,_,*,_,_,_,_],[_,*,_,_,_,_,_,_,_]).
operacao(direita,[_,_,_,_,*,_,_,_,_],[_,_,_,*,_,_,_,_,_]).
operacao(esquerda,[_,_,_,_,*,_,_,_,_],[_,_,_,_,_,*,_,_,_]).

operacao(cima,[_,_,_,_,_,*,_,_,_],[_,_,_,_,_,_,_,_,*]).
operacao(baixo,[_,_,_,_,_,*,_,_,_],[_,_,*,_,_,_,_,_,_]).
operacao(direita,[_,_,_,_,_,*,_,_,_],[_,_,_,_,*,_,_,_,_]).

operacao(baixo,[_,_,_,_,_,_,*,_,_],[_,_,_,*,_,_,_,_,_]).
operacao(esquerda,[_,_,_,_,_,_,*,_,_],[_,_,_,_,_,_,_,*,_]).

operacao(direita,[_,_,_,_,_,_,_,*,_],[_,_,_,_,_,_,*,_,_]).
operacao(esquerda,[_,_,_,_,_,_,_,*,_],[_,_,_,_,_,_,_,_,*]).
operacao(baixo,[_,_,_,_,_,_,_,*,_],[_,_,_,_,*,_,_,_,_]).

operacao(direita,[_,_,_,_,_,_,_,_,*],[_,_,_,_,_,_,_,*,_]).
operacao(baixo,[_,_,_,_,_,_,_,_,*],[_,_,_,_,_,*,_,_,_]).

/* Definição do estado inicial do jogo */
estado_inicial([2,8,3,1,6,4,7,*,5]).

/* Definição do nó objetivo */
[1,2,3,8,*,4,7,6,5] meta.