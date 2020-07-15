fill(-1):-!.
fill(X):- asserta(d(X)), X1 is X-1, fill(X1).
  
%1
%Скільки існує щасливих чотиризначних білетів, таких, що сума перших двох цифр рівна сумі двох останніх. П'ятизначна система числення.
%Скільки доданків суми 1+2+3+... треба взяти, щоб отримати тризначне число, яке складається з однакових цифр?


tickets:-
d(X),X<5,
d(Y), Y<5,
d(Z), Z<5,
d(A),A<5, 
S1 is X+Y,S2 is Z+A, S1=S2.
tickets(X):- findall(_,tickets,Z), length(Z,X).

error_cr(Www,Down,Error):- 
d(W), W\=0, 
d(D), D\=W, D\=0,
d(O), O\=W, O\=D,
d(N), N\=W,N\=D,  N\=O,
d(E), E\=W,E\=D,  E\=O, E\=N, E\=0,
d(R), R\=W,R\=D,  R\=O, R\=N, R\=E,
Www is W*100+W*10+W,
Down is D*1000+O*100+W*10+N,
Error is E*10000+R*1000+R*100+O*10+R,
Error is Www+Down.

%2 
%Iga*Byla=Smahna
crossword1(Iga,Byla,Smanha):- 
d(I), I\=0, 
d(G), G\=I,
d(A), A\=I, A\=G,
d(B), B\=0, B\=I,B\=G,  B\=A,
d(Y), Y\=I,Y\=G,  Y\=A, Y\=B,
d(L), L\=I,L\=G,  L\=A, L\=B, L\=Y,
d(S), S\=0, S\=I,S\=G,  S\=A, S\=B, S\=Y, S\=L,
d(M), M\=I,M\=G,  M\=A, M\=B, M\=Y, M\=L, M\=S,
d(N), N\=I,N\=G,  N\=A, N\=B, N\=Y, N\=L, N\=S, N\=M,
d(H), H\=I,H\=G,  H\=A, H\=B, H\=Y, H\=L, H\=S, H\=M, H\=N,

Iga is I*100+G*10+A,
Byla is B*1000+Y*100+L*10+A,
Smanha is S*100000+M*10000+A*1000+N*100+H*10+A,
Smanha is Iga*Byla.

%Ataka+Ydar+Ydar=Nokayt

crossword2(Ataka,Ydar,Nokayt):- 
d(A), A\=0,
d(T), T\=A,
d(K), K\=A, K\=T,
d(Y), Y\=0, Y\=A,Y\=T,  Y\=K,
d(D), D\=A,D\=T,  D\=K, D\=Y,
d(R), R\=A,R\=T,  R\=K, R\=Y, R\=D,
d(N), N\=0,N\=A,N\=T,  N\=K, N\=Y, N\=D, N\=R,
d(O), O\=A,O\=T,  O\=K, O\=Y, O\=D, O\=R, O\=N,

Ataka is A*10000+T*1000+A*100+K*10+A,
Ydar is Y*1000+D*100+A*10+R,
Nokayt is N*100000+O*10000+K*1000+A*100+Y*10+T,
Nokayt is Ataka+Ydar+Ydar.



%3
female(agnes).
female(idit).
female(margo).
female(lussi).
female(sally).
female(holly).
female(gulia).
female(dori).
female(dayni).
female(matulda).
female(hortenzia).
female(depla).

%married(чоловік, дружина).
    married(mcqueen, sally).
    married(gry, lussi).
    married(mater, holly).
    married(ramone, flo).
    married(mayk, selia).
 
%parents(дитина, батьки).
    parents(margo, gry).
    parents(idit,gry).
    parents(agnes,lussi).
    parents(gry,gulia).
    parents(dru, gulia).
    parents(gulia, ghost).
    parents(nafario, ghost).
    parents(lussi, baltazar).
    parents(rom, baltazar).
    parents(kevin,rom ).
    parents(idit,rom ).
    parents(sally, ramon ).
    parents(ramon, doluvay).
    parents(luidgy, doluvay).
    parents(gvido,luidgy).
    parents(mcqueen, doc).
    parents(sally, flo).
    parents(carera, flo).
    parents(holly,carera).
    parents(dori, sally).
    parents(mac, sally).
    parents(donald, dak).
    parents(dayni, dak).
    parents(matulda, dayni).
    parents(donald, hortenzia).
    parents(depla,hortenzia).
    parents(huey, matulda).
    parents(dewey, matulda).
    parents(louie, matulda).
    parents(mayk, wazowski).
    parents(selia, vantuz).
    parents(wazowski, chak).
    parents(duck, chak).

%male(X), (чоловіки)
%father(X), (батько)
%uncle(X), (дядько, брат - мами, батька)
%motherinLaw(X,Y)(X – теща, Y – зять), 
%match(X,Y)свати(X – батько(мати) дружини, Y – батько(мати) чоловіка), 
%nephew(X)(племінник(ця), дитина брата або сестри)

male(X):- (married(X,_);married(_,X);parents(X,_);parents(_,X)),\+female(X).
father(X):- parents(_,X),male(X).
brother(X):-(
    (parents(X,Y),parents(Q,Y),X \=Q);
    ((married(E,R);married(R,E)),parents(X,E),parents(T,R))
),male(X).
sister(X):-(
    (parents(X,Y),parents(Q,Y),X \=Q);
    ((married(E,R);married(R,E)),parents(X,E),parents(T,R))
),female(X).
uncle(X):- parents(X,Y), parents(Z,Y),X\=Z,parents(_,Z),male(X).
motherinLaw(X,Y):-married(Y,C), parents(C,X),female(X).
match(X,Y):-parents(Q,X),parents(W,Y),married(Q,W),X\=Y, Q\=W,male(X),male(Y).
nephew(X):-parents(X,Y),(sister(Y);brother(Y)).


%4 
pow(_, 0, 1) :- !.
pow(X, 1, X) :- !.
pow(X,Y,Z):-  Y1 is Y-1, pow(X,Y1,Z1), Z is Z1*X.

run :- retractall(d(_)),fill(9),write('1st:'),
       forall(tickets(X), (write('tickets'), write(' - '), write(X),write('\n'))),
       retractall(d(_)),fill(9),
       write('\n2nd:'),
       forall(crossword1(X, Y,Z), (write('crossword1 - '),write(X), write(' '),write(Y),write(' '),write(Z),write('\n'))),
       forall(crossword2(X, Y,Z), (write('crossword2 - '),write(X), write(' '),write(Y),write(' '),write(Z),write('\n'))),
     write('\n3rd:'),
     forall(male(X), (write('male - '),write(X),write('\n'))),
     forall(father(X), (write('father - '),write(X),write('\n'))),
     forall(uncle(X), (write('uncle - '),write(X),write('\n'))),
     forall(motherinLaw(X,Y), (write('motherinLaw - '),write(X),write(' '),write(Y),write('\n'))),
     forall(match(X,Y), (write('match - '),write(X),write(' '),write(Y),write('\n'))),
     forall(nephew(X), (write('nephew - '),write(X),write('\n'))),
     write('\n4rd:'),
     pow(10, 2,Z), write('10^2 = '),write(Z),write('\n'),
     pow(9,4,Z), write('9^4 = '),write(Z),write('\n').
     
%:- initialization(run).