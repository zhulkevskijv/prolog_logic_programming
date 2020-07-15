%Made by Zhulkevskyi Vladyslav
%Реалізувати обчислювані предикати:

%Ділення з остачею (лише для додатніх цілих чисел)- ціла частина та остача від ділення (не використувати вбудовані функції!). Hint: Використовувати оператори віднімання
%Піднесення до степеню (лінійне та логарифмічне) Hint: Використовувати оператори (*, +, -, mod)
%Числа Фібоначі (класична рекурсія, ітеративна) Hint: (+)
%Розклад числа на прості множники (виведення всії простих множників числа)
%Обрахувати сумму 1/1! + 1/2! + 1/3! + ... 1/n! за допомогою рекурентних співвідношень

%Ділення з остачею
modDiv(Divd,Divs,Res,Rem):- modDiv(Divd,Divs,Res,Rem,0).
modDiv(Divd,Divs,TempRes,Divd,TempRes):- Divd<Divs,!.
modDiv(Divd,Divs,Res,Rem,TempRes):- newDivd is Divd-Divs, newTRes is TempRes+1, modDiv(newDivd,Divs,Res,Rem,TempRes).

% Піднесення до степеня логарифмічне.
powlog(_, 0, 1) :- !.
powlog(X, 1, X) :- !.
powlog(X, Y, Z) :- Y mod 2 =:= 0,
                   X1 is X*X, Y1 is Y/2,
                   powlog(X1, Y1, Z).
powlog(X, Y, Z) :- Y1 is Y-1,
                   powlog(X, Y1, Z1),
                   Z is Z1 * X.

% Піднесення до степеня лінійне.
pow(_, 0, 1) :- !.
pow(X, 1, X) :- !.
pow(X,Y,Z):-  Y1 is Y-1, pow(X,Y1,Z1), Z is Z1*X.

% Числа Фібоначі
%fibon(0,X) X=0
%fibon(1,X) X=1 ...


fibon(X,Res):-fibon(0,1,X,Res,1).
fibon(_,_,0,Res,_):- Res is 0,!.
fibon(_,_,1,Res,_):- Res is 1,!.
fibon(_,B,X,Res,X):-Res is B,!.
fibon(A,B,X,Res,K):- NewA is B,NewB is A+B,NewK is K+1, fibon(NewA,NewB,X,Res,NewK).
fibonRec(0, 0) :- !.
fibonRec(1, 1) :- !.
fibonRec(N, R) :- P1 is N-1,P2 is N-2,
               fibon(P1,R1), fibon(P2,R2),
               R is R1 + R2.

%Розклад числа на прості множники
simple(X) :- X < 2, !.
simple(X) :- simpleHelper(X,R,2), write(R), write(' '), R1 is div(X,R), simple(R1).
simpleHelper(X,_,B) :- X < B,!.
simpleHelper(X,A,B) :- X mod B =:= 0, A is B,!.
simpleHelper(X,A,B) :- B1 is B + 1, simpleHelper(X,A,B1).

%Cума 1/1! + 1/2! + 1/3! + ... 1/n!
rowSum(X,Res) :- rowSumHelper(X,0,1,0,Res).
rowSumHelper(X,Y, _,S,Res) :- X =< Y, Res is S,!.
rowSumHelper(X,Y,T,S,Res) :- Y1 is Y+1, T1 is T/Y1, 
                           S1 is S+T1,rowSumHelper(X,Y1,T1,S1,Res).

                                            