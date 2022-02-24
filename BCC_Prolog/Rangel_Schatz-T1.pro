velha('O') :- inicio(Matriz), escreve(Matriz), nl, write('Voce joga com O.'), nl, write('O computador joga com X.'), nl, nl, write('O computador comeca.'), nl, partida(Matriz,10,'X'),!.
velha('X') :- inicio(Matriz), escreve(Matriz), nl, write('O computador joga com O.'), nl, write('Voce joga com X.'), nl, nl, write('Voce comeca.'), nl, partida(Matriz,9,'X'),!.

inicio([*,*,*,*,*,*,*,*,*]).

escreve([]).
escreve([A,B,C|Restante]) :- write(A),write(' | '), write(B), write(' | '), write(C), nl, escreve(Restante).

partida(Matriz,Rodada,_) :- verifica(Matriz,Q,Celula), Q==1, write(Celula), write(' - venceu o jogo!'), nl, write('-- FIM --'), nl, !.
partida(Matriz,Rodada,'X') :- Rodada==0, write('Deu velha...'), nl, write('-- FIM --'), nl, !.
partida(Matriz,Rodada,'O') :- Rodada==1, write('Deu velha...'), nl, write('-- FIM --'), nl, !.
partida(Matriz,Rodada,'O') :- Posicao is Rodada mod 2, Posicao\=0, jogauser(Matriz,MatrizRes,'O'), J is Rodada-1, partida(MatrizRes,J,'X').
partida(Matriz,Rodada,'X') :- Posicao is Rodada mod 2, Posicao\=0, jogauser(Matriz,MatrizRes,'X'), J is Rodada-1, partida(MatrizRes,J,'O').
partida(Matriz,Rodada,'O') :- Posicao is Rodada mod 2, Posicao==0, jogacomp(Matriz,MatrizRes,'O'), J is Rodada-1, partida(MatrizRes,J,'X').
partida(Matriz,Rodada,'X') :- Posicao is Rodada mod 2, Posicao==0, jogacomp(Matriz,MatrizRes,'X'), J is Rodada-1, partida(MatrizRes,J,'O').

/* Verifica se existe ganhador */
verifica([A,B,C|T],1,A) :- A==B, B==C, A\='*'.             /* Linha 1 */
verifica([A,B,C,D,E,F|T],1,D) :- D==E, E==F, D\='*'.       /* Linha 2 */
verifica([A,B,C,D,E,F,G,H,I|T],1,G) :- G==H, H==I, G\='*'. /* Linha 3 */

verifica([A,B,C,D,E,F,G,H,I|T],Posicao,A) :- A==D, D==G, A\='*', Posicao is 1. /* Coluna 1 */
verifica([A,B,C,D,E,F,G,H,I|T],Posicao,B) :- B==E, E==H, B\='*', Posicao is 1. /* Coluna 2 */
verifica([A,B,C,D,E,F,G,H,I|T],Posicao,C) :- C==F, F==I, C\='*', Posicao is 1. /* Coluna 3 */

verifica([A,B,C,D,E,F,G,H,I|T],Posicao,A) :- A==E, E==I, A\='*', Posicao is 1. /* Diagonal Principal */
verifica([A,B,C,D,E,F,G,H,I|T],Posicao,C) :- C==E, E==G, C\='*', Posicao is 1. /* Diagonal Secundaria */

verifica(Matriz,Posicao,Celula) :- Posicao is 0.

/*"jogacomp" => Trata da jogada do computador*/
jogacomp(Matriz,MatrizRes,Caractere) :- onde(Matriz,Posicao,Caractere), replace(Matriz,Posicao,Caractere,MatrizAux), write('Computador:'), nl, escreve(MatrizAux), copia(MatrizAux,MatrizRes), nl. /*Ataque*/
jogacomp(Matriz,MatrizRes,'O') :- onde(Matriz,Posicao,'X'), replace(Matriz,Posicao,'O',MatrizAux), write('Computador:'), nl, escreve(MatrizAux), copia(MatrizAux,MatrizRes), nl. /*Defesa com O*/
jogacomp(Matriz,MatrizRes,'X') :- onde(Matriz,Posicao,'O'), replace(Matriz,Posicao,'X',MatrizAux), write('Computador:'), nl, escreve(MatrizAux), copia(MatrizAux,MatrizRes), nl. /*Defesa com X*/
jogacomp(Matriz,MatrizRes,Caractere) :- livre(Matriz,9,Posicao), replace(Matriz,Posicao,Caractere,MatrizAux), write('Computador:'), nl, escreve(MatrizAux), copia(MatrizAux,MatrizRes), nl. /*Livre*/

/*Verifica em que casa o computador deve jogar para ganhar ou defender*/
onde(Matriz,Posicao,Caractere) :- lin(Matriz,Posicao,Caractere,'*').
onde(Matriz,Posicao,Caractere) :- col(Matriz,Posicao,Caractere,'*').
onde(Matriz,Posicao,Caractere) :- diagp(Matriz,Posicao,Caractere,'*').
onde(Matriz,Posicao,Caractere) :- diags(Matriz,Posicao,Caractere,'*').

/*Busca um espaco livre para jogar visando o ataque*/
livre(Matriz,X,Posicao) :- lin(Matriz,PosAux,'*','O'), PosAux==3, Posicao is PosAux-1.
livre(Matriz,X,Posicao) :- lin(Matriz,PosAux,'*','O'), PosAux==6, Posicao is PosAux-1.
livre(Matriz,X,Posicao) :- lin(Matriz,PosAux,'*','O'), PosAux==9, Posicao is PosAux-1.
livre(Matriz,X,Posicao) :- lin(Matriz,PosAux,'*','O'), Posicao is PosAux+1.
livre(Matriz,X,Posicao) :- col(Matriz,PosAux,'*','O'), PosAux<7, Posicao is PosAux+3.
livre(Matriz,X,Posicao) :- col(Matriz,PosAux,'*','O'), PosAux>6, Posicao is PosAux-3.
livre(Matriz,X,Posicao) :- diags(Matriz,PosAux,'*','O'), PosAux==3, Posicao is PosAux+2.
livre(Matriz,X,Posicao) :- diags(Matriz,PosAux,'*','O'), PosAux==5, Posicao is PosAux+2.
livre(Matriz,X,Posicao) :- diags(Matriz,PosAux,'*','O'), PosAux==7, Posicao is PosAux-2.
livre(Matriz,X,Posicao) :- diagp(Matriz,PosAux,'*','O'), PosAux==1, Posicao is PosAux+4.
livre(Matriz,X,Posicao) :- diagp(Matriz,PosAux,'*','O'), PosAux==5, Posicao is PosAux+4.
livre(Matriz,X,Posicao) :- diagp(Matriz,PosAux,'*','O'), PosAux==9, Posicao is PosAux-4.

/*Busca um espaco livre qualquer*/
livre([Cab|Cauda],X,Posicao) :- Cab == '*', Posicao is 10-X.
livre([Cab|Cauda],X,Posicao) :- Cab \= '*', Aux is X-1, Aux>0, livre(Cauda,Aux,Posicao).

/*"jogauser" => Trata da jogada do usuario*/
jogauser(Matriz,MatrizRes,Caractere) :- joga(Linha,Coluna), jogar(Matriz,Linha,Coluna,MatrizRes,Caractere).

joga(L,Coluna) :- nl, write('Digite a Linha e a Coluna: '), nl, write('Linha:'), read(L), write('Coluna:'), read(Coluna).

jogar(Matriz,Linha,Coluna,MatrizRes,Caractere) :- ocupado(Matriz,Linha,Coluna,'X'), nl, write('*** Jogada invalida, jogue novamente ***'), nl, jogauser(Matriz,MatrizRes,Caractere).
jogar(Matriz,Linha,Coluna,MatrizRes,Caractere) :- ocupado(Matriz,Linha,Coluna,'O'), nl, write('*** Jogada invalida, jogue novamente ***'), nl, jogauser(Matriz,MatrizRes,Caractere).
jogar(Matriz,Linha,Coluna,MatrizRes,Caractere) :- altera(Matriz,Linha,Coluna,Caractere,MatrizAux), nl, write('Voce:'), nl, escreve(MatrizAux), copia(MatrizAux,MatrizRes), nl.

/* Verifica se determinada posicao ou linha/coluna esta ocupada*/
ocupado(Matriz,Posicao,Caractere) :- Posicao > 0, Posicao < 4, ocupado(Matriz,1,Posicao,Caractere).
ocupado(Matriz,Posicao,Caractere) :- Posicao > 3, Posicao < 7, X is Posicao-3, ocupado(Matriz,2,X,Caractere).
ocupado(Matriz,Posicao,Caractere) :- Posicao > 6, Posicao < 10, X is Posicao-6, ocupado(Matriz,3,X,Caractere).

ocupado([X|Y],Linha,Coluna,Caractere) :- Linha==1, Coluna==1, X==Caractere.
ocupado([X|Y],Linha,Coluna,Caractere) :- Linha==1, Aux is Coluna-1, ocupado(Y,Linha,Aux,Caractere).
ocupado([X|Y],Linha,Coluna,Caractere) :- Linha==2, Aux is Coluna+2, ocupado(Y,1,Aux,Caractere).
ocupado([X|Y],Linha,Coluna,Caractere) :- Linha==3, Aux is Coluna+5, ocupado(Y,1,Aux,Caractere).

/* Troca "*" por "X" ou "O" */
altera(Matriz,1,Coluna,Caractere,MatrizRes) :- replace(Matriz,Coluna,Caractere,MatrizRes).
altera(Matriz,2,Coluna,Caractere,MatrizRes) :- X is Coluna+3, replace(Matriz,X,Caractere,MatrizRes).
altera(Matriz,3,Coluna,Caractere,MatrizRes) :- X is Coluna+6, replace(Matriz,X,Caractere,MatrizRes).

replace([X|Y],1,Caractere,[Caractere|Y]).
replace([X|Y],Posicao,Caractere,[X|Restante]) :- Aux is Posicao-1, replace(Y,Aux,Caractere,Restante).

/*Copia uma matriz*/
copia([Cab|Cauda],[Cab|Cauda]).

/*Conjunto de regras de jogadas para linha, coluna e diagonais*/
lin(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,1,C1), ocupado(Matriz,1,2,C1), ocupado(Matriz,3,C2), Posicao is 3.
lin(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,2,C1), ocupado(Matriz,1,3,C1), ocupado(Matriz,1,C2), Posicao is 1.
lin(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,1,C1), ocupado(Matriz,1,3,C1), ocupado(Matriz,2,C2), Posicao is 2.
lin(Matriz,Posicao,C1,C2) :- ocupado(Matriz,2,1,C1), ocupado(Matriz,2,2,C1), ocupado(Matriz,6,C2), Posicao is 6.
lin(Matriz,Posicao,C1,C2) :- ocupado(Matriz,2,2,C1), ocupado(Matriz,2,3,C1), ocupado(Matriz,4,C2), Posicao is 4.
lin(Matriz,Posicao,C1,C2) :- ocupado(Matriz,2,1,C1), ocupado(Matriz,2,3,C1), ocupado(Matriz,5,C2), Posicao is 5.
lin(Matriz,Posicao,C1,C2) :- ocupado(Matriz,3,1,C1), ocupado(Matriz,3,2,C1), ocupado(Matriz,9,C2), Posicao is 9.
lin(Matriz,Posicao,C1,C2) :- ocupado(Matriz,3,2,C1), ocupado(Matriz,3,3,C1), ocupado(Matriz,7,C2), Posicao is 7.
lin(Matriz,Posicao,C1,C2) :- ocupado(Matriz,3,1,C1), ocupado(Matriz,3,3,C1), ocupado(Matriz,8,C2), Posicao is 8.

col(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,1,C1), ocupado(Matriz,2,1,C1), ocupado(Matriz,7,C2), Posicao is 7.
col(Matriz,Posicao,C1,C2) :- ocupado(Matriz,2,1,C1), ocupado(Matriz,3,1,C1), ocupado(Matriz,1,C2), Posicao is 1.
col(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,1,C1), ocupado(Matriz,3,1,C1), ocupado(Matriz,4,C2), Posicao is 4.
col(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,2,C1), ocupado(Matriz,2,2,C1), ocupado(Matriz,8,C2), Posicao is 8.
col(Matriz,Posicao,C1,C2) :- ocupado(Matriz,2,2,C1), ocupado(Matriz,3,2,C1), ocupado(Matriz,2,C2), Posicao is 2.
col(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,2,C1), ocupado(Matriz,3,2,C1), ocupado(Matriz,5,C2), Posicao is 5.
col(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,3,C1), ocupado(Matriz,2,3,C1), ocupado(Matriz,9,C2), Posicao is 9.
col(Matriz,Posicao,C1,C2) :- ocupado(Matriz,2,3,C1), ocupado(Matriz,3,3,C1), ocupado(Matriz,3,C2), Posicao is 3.
col(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,3,C1), ocupado(Matriz,3,3,C1), ocupado(Matriz,6,C2), Posicao is 6.

diagp(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,1,C1), ocupado(Matriz,2,2,C1), ocupado(Matriz,9,C2), Posicao is 9.
diagp(Matriz,Posicao,C1,C2) :- ocupado(Matriz,2,2,C1), ocupado(Matriz,3,3,C1), ocupado(Matriz,1,C2), Posicao is 1.
diagp(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,1,C1), ocupado(Matriz,3,3,C1), ocupado(Matriz,5,C2), Posicao is 5.

diags(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,3,C1), ocupado(Matriz,2,2,C1), ocupado(Matriz,7,C2), Posicao is 7.
diags(Matriz,Posicao,C1,C2) :- ocupado(Matriz,2,2,C1), ocupado(Matriz,3,1,C1), ocupado(Matriz,3,C2), Posicao is 3.
diags(Matriz,Posicao,C1,C2) :- ocupado(Matriz,1,3,C1), ocupado(Matriz,3,1,C1), ocupado(Matriz,5,C2), Posicao is 5.