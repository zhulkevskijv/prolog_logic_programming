% Developed by Zhulkevskyi Vladyslav

% 1 task
% Написати програму, яка здійснює обхід бінарного дерева зліва-направо.
% leftBypass(tree(tree(tree(nil,a,nil),b,tree(nil,c,nil)),d,tree(nil,e,tree(tree(nil,f,nil),g,nil))),R).       R = [a,b,c,d,e,f,g]
% leftBypass(tree(tree(tree(tree(nil,a,nil),b,tree(nil,c,nil)),d,tree(nil,e,nil)),f,tree(tree(nil,g,nil),h, tree(tree(nil,i,nil),j,nil))),R).   R = [a,b,c,d,e,f,g,h,i,j]
% leftBypass(tree(nil,a,nil),R).  R = [a]
% leftBypass(nil,R).              R = []
leftBypass(nil,[]):-!.
leftBypass(tree(L,X,R),Res):-leftBypass(L,Left),leftBypass(R,Right),append(Left,[X|Right],Res),!.

% 2 task
% Написати програму, яка б визначала кількість вершин-листків в бінарному дереві.
% leafQuantity(tree(tree(tree(nil,a,nil),b,tree(nil,c,nil)),d,tree(nil,e,tree(tree(nil,f,nil),g,nil))),R).             Res = 3
% leafQuantity(tree(tree(tree(tree(nil,a,nil),b,tree(nil,c,nil)),d,tree(nil,e,nil)),f,tree(tree(nil,g,nil),h, tree(tree(nil,i,nil),j,nil))),R).   Res = 5
% leafQuantity(tree(nil,a,nil),R). Res = 1
% leafQuantity(nil,R). Res = 0
leafQuantity(nil,0):-!.
leafQuantity(tree(nil,_,nil),1):-!.
leafQuantity(tree(L,_,R),Res):-leafQuantity(L,Left),leafQuantity(R,Right), Res is Right+Left,!.

% 3 task
% Написати програму, яка знаходила б висоту бінарного дерева.
% treeHeight(tree(tree(tree(nil,a,nil),b,tree(nil,c,nil)),d,tree(nil,e,tree(tree(nil,f,nil),g,nil))),R).       Res = 4
% treeHeight(tree(tree(tree(tree(nil,a,nil),b,tree(nil,c,nil)),d,tree(nil,e,nil)),f,tree(tree(nil,g,nil),h, tree(tree(nil,i,nil),j,nil))),R).    Res = 4
% treeHeight(tree(nil,a,nil),R).     Res = 1
% treeHeight(nil,R).  Res = 0
treeHeight(nil,0):-!.
treeHeight(tree(L,_,R),Res):-treeHeight(L,Left),treeHeight(R,Right),Res is 1+max(Left, Right),!.

% 4 task
% Написати програму, яка визначає кількість вузлів у бінарному дереві.
% nodesQuantity(tree(tree(tree(nil,a,nil),b,tree(nil,c,nil)),d,tree(nil,e,tree(tree(nil,f,nil),g,nil))),R).       Res = 7
% nodesQuantity(tree(tree(tree(tree(nil,a,nil),b,tree(nil,c,nil)),d,tree(nil,e,nil)),f,tree(tree(nil,g,nil),h, tree(tree(nil,i,nil),j,nil))),R).    Res = 10
% nodesQuantity(tree(nil,a,nil),R).   Res = 1
% nodesQuantity(nil,R). Res = 0
nodesQuantity(T,Res):-leftBypass(T,R),length(R,Res),!.

% 5 task
% Написати програму обходу 2-3-дерева(1).
% bypass23(v3(l(1),2,l(3),4,v3(nil,5,nil,6,nil)),R).              [1,2,3,4,5,6]
% bypass23(v2(v3(nil,a,nil,b,nil),c,l(d)),R).     [a,b,c,d]
bypass23(nil,[]):-!.
bypass23(l(R),[R]):-!.
bypass23(v2(L,X,R),Res):-bypass23(L,Left), bypass23(R,Right),append(Left,[X|Right],Res),!.
bypass23(v3(L,First,M,Second,R),Res):-bypass23(L,Left), bypass23(M,Middle), bypass23(R,Right),
  append(Left,[First|Middle],Temp1),append(Temp1,[Second|Right],Res),!.

% 6 task
% Написати програму пошуку заданого елемента в 2-3-дереві(1)
% findElem(v3(l(1),2,l(3),4,v3(nil,5,nil,6,nil)),3).   yes
% findElem(v2(v3(nil,a,nil,b,nil),c,l(d)),d).   yes
% findElem(v2(v3(nil,a,nil,b,nil),c,l(d)),z).   no
findElem(nil,_):-fail.
findElem(l(R),Y):-Y=R,!.
findElem(v2(L,X,R),Y):-(X=Y; findElem(L,Y);findElem(R,Y)),!.
findElem(v3(L,First,M,Second,R),Y):-(First=Y; Second=Y; findElem(L,Y); findElem(M,Y); findElem(R,Y)),!.

% 7 task
% Написати програму, яка перевірить чи є заданий об'єкт бінарним деревом чи 2-3 деревом.
% Зробив два варіанти: для відсортованих та несортованих дерев.

% isBinary(tree(tree(nil, "a",nil),"b",nil)).  yes
% isBinary(tree(tree(nil, tree(nil,nil,nil),nil),"b",nil)). no
% isBinary(tree(tree(tree(nil,"a",nil),"b",tree(nil,"c",nil)),"d",tree(nil,"e",tree(tree(nil,"f",nil),"g",nil)))).  yes
% is23(v3(l(1),2,l(3),4,v3(nil,5,nil,6,nil))).     yes
% is23(v3(l(1),2,l(3),l(5),v3(nil,5,nil,6,nil))). no
% is23Sort(v2(v3(nil,"a",nil,"b",nil),"c",l("d"))). yes


% isBinarySort(tree(tree(nil, "a",nil),"b",nil)).   yes
% isBinarySort(tree(tree(tree(nil,"a",nil),"b",tree(nil,"c",nil)),"d",tree(nil,"e",tree(tree(nil,"f",nil),"g",nil)))).   yes
% isBinarySort(tree(tree(tree(nil,"a",nil),"b",tree(nil,"a",nil)),"d",tree(nil,"e",tree(tree(nil,"f",nil),"g",nil)))).    no
% isBinarySort(nil).    yes
% isBinarySort(tree(nil,nil,nil)).   no
% is23Sort(nil).   yes
% is23Sort(l(1)).   yes
% is23Sort(v3(l(1),2,l(3),4,v3(nil,5,nil,6,nil))).  yes
% is23Sort(v3(l(7),2,l(3),4,v3(nil,5,nil,6,nil))).   no
% is23Sort(v2(v3(l("a"),"b",nil,"c",l("d")),"e",nil)).   yes
% is23Sort(v2(v3(nil,"a",nil,"b",nil),"c",l("d"))).  yes


isBinary(nil):-!.
isBinary(tree(L,X,R)):-X\=nil,X\=tree(_,_,_),isBinary(L),isBinary(R),!.

is23(nil):-!.
is23(l(X)):-X\=nil,X\=v2(_,_,_),X\=v3(_,_,_,_,_),X\=l(_),!.
is23(v2(L,X,R)):-X\=nil,X\=l(_),X\=v3(_,_,_,_,_),X\=v2(_,_,_),is23(L),is23(R),!.
is23(v3(L,First,M,Second,R)):-First\=nil,  First\=v3(_,_,_,_,_),First\=v2(_,_,_),First\=l(_),Second\=nil,Second\=v3(_,_,_,_,_),Second\=v2(_,_,_),Second\=l(_), is23(L),is23(R),is23(M),!.

checkLess(_,[]):-!.
checkLess(X,[X1|List]):- X=<X1,checkLess(X,List),!.
checkGreater(_,[]):-!.
checkGreater(X,[X1|List]):- X>=X1,checkGreater(X,List),!.

isBinarySort(nil):-!.
isBinarySort(tree(L,X,R)):-X\=nil,X\=tree(_,_,_),isBinarySort(L),isBinarySort(R),leftBypass(L,Res1),leftBypass(R,Res2),checkGreater(X,Res1),checkLess(X,Res2),!.


is23Sort(nil):-!.
is23Sort(l(X)):-X\=nil,X\=v2(_,_,_),X\=v3(_,_,_,_,_),X\=l(_),!.
is23Sort(v2(L,X,R)):- X\=nil,X\=l(_),X\=v3(_,_,_,_,_),X\=v2(_,_,_),bypass23(L,Res1),bypass23(R,Res2),checkGreater(X,Res1),checkLess(X,Res2), is23Sort(L), is23Sort(R), !.
is23Sort(v3(L,First,M,Second,R)):- First\=nil,  First\=v3(_,_,_,_,_),First\=v2(_,_,_),First\=l(_),Second\=nil,Second\=v3(_,_,_,_,_),Second\=v2(_,_,_),Second\=l(_), First<Second,
   bypass23(L,Res1),bypass23(M,Res2),bypass23(R,Res3),checkGreater(First,Res1),checkGreater(Second,Res2),checkLess(First,Res2),checkLess(Second,Res3), is23Sort(L),is23Sort(M),is23Sort(R),!.
