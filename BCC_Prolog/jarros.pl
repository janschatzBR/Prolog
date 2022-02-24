
:- nl,nl,
   write('###############################################################'),nl,nl,
   write('1º Trabalho de IA'),nl,
   write('Profª. Lúcia Rino'),nl,nl,
   write('Alunos: Jan Schatz - 252751'),nl,
   write('        Rafael Marsolla - 270393'),nl,nl,
   write('O Problema dos Jarros (Busca Cega)'),nl,nl,
   write('###############################################################'),nl,nl,
   write('Caso Teste'),nl,
   write('  dist(Jarro4_inicial,Jarro3_inicial,Jarro4_final,Jarro3_final).'),nl,
   write('Exemplo Testado:'),nl,
   write('   dist(0,0,2,0).'),nl,nl,
   write('###############################################################'),nl,nl.

dist(X1, Y1, X2, Y2) :- buscal([ [ (X1,Y1,'Inicio') ] ], (X2,Y2,_),Solucao),
   write('###############################################################'),nl,
   calcula(Solucao),nl.

calcula([]).
calcula([(X,Y,O)|R]) :- calcula(R), write(O), write(':          ('),write(X), write(','), write(Y), write(')'),nl.

oper( (X1,Y1,_), (4,Y1,'EncherJ4')) :-  X1 < 4.

oper( (X1,Y1,_), (X1,3,'EncherJ3')) :-  Y1 < 3.

oper( (X1,Y1,_), (0,Y1,'EsvaziarJ4')) :-  X1 > 0.

oper( (X1,Y1,_), (X1,0,'EsvaziarJ3')) :-  Y1 > 0.

oper( (X1,Y1,_), (4,Y2,'TransbordaJ3J4')) :-  T is X1 + Y1, T >= 4, X1 < 4, Y1 > 0, Y2 is Y1 - (4 - X1).

oper( (X1,Y1,_), (X2,3,'TransbordaJ4J3')) :-  T is X1 + Y1, T >= 3, X1 > 0, Y1 < 3, X2 is X1 - (3 - Y1).

oper( (X1,Y1,_), (X2,0,'TransfereJ3J4')) :-  T is X1 + Y1, T =< 4, Y1 > 0, X2 is X1 + Y1.

oper( (X1,Y1,_), (0,Y2,'TransfereJ4J3')) :-  T is X1 + Y1, T =< 3, X1 > 0, Y2 is X1 + Y1.

buscal( [ [Meta|Solucao] | _], Meta, [Meta|Solucao]).

buscal([ Caminho|RAbertos], Meta, Solucao) :- filhos( Caminho, Filhos), %write('Filhos '), write(Filhos),nl,nl,
   % largura:
   conc( RAbertos, Filhos, NovoAbertos),
   % profundidade: 
   %conc( Filhos, RAbertos, NovoAbertos),	 
   buscal( NovoAbertos, Meta, Solucao).

filhos([Nodo | Caminho], NovosCaminhos) :- findall([Filho, Nodo | Caminho], 
      (oper( Nodo, Filho), \+ member(Filho, [Nodo|Caminho])), NovosCaminhos).

member(X,[X|_]).
member(X,[_|R]) :- member(X,R).
conc([],L,L).
conc([X|L1],L2,[X|L3]) :- conc(L1,L2,L3).

:- dist(0,0,2,0).
