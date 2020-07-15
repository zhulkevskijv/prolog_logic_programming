% Zhulkevskyi Vladyslav list 4 lab

% first task
% samples 
% 1. minusList([-1,0,-56,91,100],R).
% 2. minusList([-100,56,7,4,3,-456,-100],R).

helper1([],Y,_,Y):-!.
helper1([X|L],Y,Z,Res):-(X<0,append([Z],Y,Y1), Z1 is Z+1,helper1(L,Y1,Z1,Res)) ; (Z1 is Z+1,helper1(L,Y,Z1,Res)). 

minusList(X,Y):- helper1(X,[],0,Res), append([],Res,Y1),reverse(Y1,Y),!.

% second task
% samples 
% 1. replaceElem([1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1], 7,R).
% 2. replaceElem(['a','a','b','c'], 'a',R).
% 3. replaceElem([[1,2,3],[4,5,6],[-1,-2,-3]], [1,2,3],R).
helper2([],_,Temp,Res):-append(Temp,[],Res1),reverse(Res1,Res).
helper2([X|L],Y,Temp,Res):- (X=Y,append(['change_log'],Temp,Temp1), helper2(L,Y,Temp1,Res)); (append([X],Temp,Temp1) ,helper2(L,Y,Temp1,Res)).

replaceElem(X,Y,Res):-  helper2(X,Y,[],Res),!.

%third task
%samples
% 1. convertRomanian([1,29,50,91,52,121],R).
% 2. convertRomanian([-40],R).               - fail
% 3. convertRomanian([24,204,88],R).
convertNum(X, _):- X<0, !, fail.
convertNum(0, []).
convertNum(X, ['I'|Roman]) :- X < 4, Y is X - 1, convertNum(Y, Roman).
convertNum(4, ['IV']).
convertNum(5, ['V']).
convertNum(X, ['V'|Roman]) :- X < 9, Y is X - 5, convertNum(Y, Roman).
convertNum(9, ['IX']).
convertNum(X, ['X'|Roman]) :- X < 40, Y is X - 10, convertNum(Y, Roman).
convertNum(X, ['XL'|Roman]) :- X < 50, Y is X - 40, convertNum(Y, Roman).
convertNum(X, ['L'|Roman]) :- X<90,Y is X - 50, convertNum(Y, Roman).
convertNum(X, ['XC'|Roman]) :- X < 100, Y is X - 90, convertNum(Y, Roman).
convertNum(X, ['C'|Roman]) :- X < 400, Y is X - 100, convertNum(Y, Roman).
convertNum(X, ['CD'|Roman]) :- X < 500, Y is X - 400, convertNum(Y, Roman).
convertNum(X, ['D'|Roman]) :- X < 900, Y is X - 500, convertNum(Y, Roman).
convertNum(X, ['CM'|Roman]) :- X < 1000, Y is X - 900, convertNum(Y, Roman).
convertNum(X, ['M'|Roman]) :- Y is X - 1000, convertNum(Y, Roman).

concatList([],Y,Y):-!. 
concatList([X|L],Y,Res):- atom_concat(X, Y, Y1),concatList(L,Y1,Res).


helper3([],Y,Y):-!.
helper3([X|L],Y,Res):- convertNum(X,Q),reverse(Q,R),concatList(R,'',R1), append([R1],Y,Y1),helper3(L,Y1,Res).

convertRomanian(X,Y):- helper3(X,[],Res),reverse(Res,R),append(R,[],Y),!.

% forth task
% 1. cycleShiftR([1,2,3],R).
% 2. cycleShiftR([90,100,110,120,130],R).
% 3. cycleShiftR(['a','b',1],R).
helper4([X|[]],Y,Res):- append(Y,[X],Res1),reverse(Res1,Res).
helper4([X|L],Y,Res):-append([X],Y,Y1),helper4(L,Y1,Res).

cycleShiftR([],[]):-!.
cycleShiftR(X,Res):- helper4(X,[],Res),!.

% fifth task
% samples
% 1. multMatrix([[2,-3],[4,7]] , [2,3],R).
% 2. multMatrix([[1,1,1],[2,2,2],[3,3,3],[4,4,4]], [1,2,3],R).
% 3. multMatrix([[1,1,1],[2,2,2],[3,3,3],[4,4,4]], [1,2],R).     - fail
% 4. multMatrix([[1,0,1,0,1],[0,1,0,1,0],[2,0,2,0,2],[0,2,0,2,0]],  [10,20,30,40,50],R).
multiply([],[],Res,Res):- !.
multiply([X|L],[Y|V],Temp,Res) :- El is X*Y,Temp1 is Temp+El, multiply(L,V,Temp1,Res).

multElems([],_,Res,Res):- !.
multElems([X|M],Y,Temp,Res):- multiply(X,Y,0,Z),append(Temp,[Z],Temp1), multElems(M,Y,Temp1,Res). 

checkMatrix([],_):- !.
checkMatrix([X|L],Len):- length(X,Len1), (Len1=Len, checkMatrix(L,Len)); fail.

multMatrix(M,V,Res):-length(V,Len),checkMatrix(M,Len),multElems(M,V,[],Res),!.


