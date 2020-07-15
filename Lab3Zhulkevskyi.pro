%Made by Zhulkevskyi Vladyslav

%1. У тризначному числi, всi цифри якого непаpнi, закpеслили середню цифру. Виявилось, що отpимане двозначне число є дiльником вихiдного числа. Знайдiть всi такi тризначнi числа.
%2. Знайдiть чотиризначне число, яке є точним квадратом, у якого двi першi цифри однаковi та двi останнi також однаковi.
%3. Скiльки iснує цiлих чисел вiд 1 до 1998, якi не дiляться на жодне з чисел 6, 10, 15?
%4. Знайти найменше натуральне число M, яке має наступну властивiсть: сума квадратiв одинадцяти послiдовних натуральних чисел, починаючи з M, є точним квадратом?
%5. В послiдовностi 1998737... кождна цифра, починаючи з п'ятої, дорiвнює останнiй цифрi суми чотирьох попеpеднiх цифр. Через скiльки цифр знову зустрiнитья початкова комбiнацiя 1998 (тобто скiльки цифр в перiодi)?

fill(-1) :- !.
fill(X) :- asserta(d(X)), X1 is X-1, fill(X1).

% with fill(9)
func1(X) :- d(A), A > 0, d(B), d(C),
            A mod 2 =\= 0,
            B mod 2 =\= 0,
            C mod 2 =\= 0,
            X is A*100+B*10+C,
            X1 is A*10+C,
            (X mod X1) =:= 0.
            
% with fill(9)
func2(X) :- d(A), A > 0, d(A), d(B), d(B),
            X is A*1000+A*100+B*10+B,
            X1 is sqrt(X),
            round(X1)**2 =:=X,!.


func3(X):- count(1,X,0),!.

count(X,Res,TempRes):- (X>1998,Res is TempRes);
((X mod 6) =\=0, 
(X mod 10) =\=0, 
(X mod 15) =\=0,
NewRes is TempRes+1,
X1 is X+1, 
count(X1,Res,NewRes)
);
(X1 is X+1, count(X1,Res,TempRes)).

func4(X):- findM(1,X),!.

findM(X,Res):-
findSqSum(X,Temp,0,0),
(round(sqrt(Temp))**2 =:=Temp,
Res is X);(X1 is X+1, findM(X1,Res)).

findSqSum(_,Res,11,Res):-!.
findSqSum(M,Res,X,TempRes):-
NewRes is (TempRes+(M+X)**2), 
X1 is X+1,
findSqSum(M,Res,X1,NewRes).

func5(X):- helper(1,9,9,8,X,0).

helper(1,9,9,8,Res,TempRes):- TempRes>0,Res is TempRes, !.
helper(A,B,C,D,Res,TempRes):- 
S is A+B+C+D,
Num is S mod 10,
NewRes is TempRes+1,
helper(B,C,D,Num,Res,NewRes).



