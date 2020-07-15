% 1. Написати програму обходу AVL-дерева.
%  bypassAVL(tree(tree(nil/0,1,nil/0)/1,3,nil/0)/2,R).   [1,3]
%  bypassAVL(tree(tree(tree(tree(nil/0,1,nil/0)/1,2,nil/0)/2,3,tree(nil/0,4,nil/0)/1)/3,5,tree(nil/0,6,tree(nil/0,7,nil/0)/1)/2)/4,R). [1,2,3,4,5,6,7]
bypassAVL(nil/0,[]):-!.
bypassAVL(tree(L,X,R)/_,Res):- bypassAVL(L,Left),bypassAVL(R,Right),append(Left,[X|Right],Res),!.

% 2. Написати програму пошуку заданого елемента в AVL-дереві
% findElem(tree(tree(nil/0,1,nil/0)/1,3,nil/0)/2,3).   yes
% findElem(tree(tree(nil/0,1,nil/0)/1,3,tree(nil/0,5,nil/0)/1)/2,5). yes
findElem(nil/0,_):-fail.
findElem(tree(_,X,_)/_,X):-!.
findElem(tree(L,_,R)/_,S):-(findElem(L,S);findElem(R,S);fail),!.

% 3. Написати програму, яка перевірить чи є заданий об'єкт AVL-деревом

% checkAVL(tree(tree(nil/0,1,nil/0)/2,3,tree(nil/0,5,nil/0)/2)/1). no     because of wrong height
% checkAVL(tree(tree(nil/0,1,nil/0)/1,3,nil/0)/2).   yes
% checkAVL(tree(tree(nil/0,1,nil/0)/1,3,tree(nil/0,5,nil/0)/1)/2).   yes
% checkAVL(tree(tree(tree(tree(nil/0,1,nil/0)/1,2,nil/0)/2,3,tree(nil/0,4,nil/0)/1)/3,5,tree(nil/0,6,tree(nil/0,7,nil/0)/1)/2)/4). yes
% checkAVL(tree(tree(tree(tree(nil/0,7,nil/0)/1,2,nil/0)/2,3,tree(nil/0,4,nil/0)/1)/3,1,tree(nil/0,6,tree(nil/0,7,nil/0)/1)/2)/4).   no because not sorted
% checkAVL(tree(tree(tree(tree(nil/0,1,nil/0)/1,2,nil/0)/2,3,tree(nil/0,4,nil/0)/1)/3,5,tree(nil/0,6,nil/0)/1)/4).   no because difference in height not in -1 <= difference<=1
heightAVL(nil/0,0):-!.
heightAVL(tree(L,_,R)/_,Height):-heightAVL(L,Left),heightAVL(R,Right),Height is 1+max(Left, Right),!.

checkLess(_,[]):-!.
checkLess(X,[X1|List]):- X=<X1,checkLess(X,List),!.
checkGreater(_,[]):-!.
checkGreater(X,[X1|List]):- X>=X1,checkGreater(X,List),!.

checkAVL(nil/0):-!.
checkAVL(tree(L,X,R)/D):- X\=nil/0,X\=tree(_,_,_)/_,checkAVL(L),checkAVL(R),
 % check height of node
 heightAVL(tree(L,X,R)/D,Height), D=Height, 
 % check if sorted
 bypassAVL(L,Left),bypassAVL(R,Right),checkGreater(X,Left),checkLess(X,Right),
 % check differnece in height 
 heightAVL(L,LH),heightAVL(R,RH), Diff is RH-LH, (-1)=<Diff, Diff=<(1),
 !. 


% 4. Написати програму видалення заданого вузла з дерева, не порушуючи його структуру: бінарне сортуюче дерево .
% deleteElemBST(tree(tree(tree(nil,1,nil),2,tree(nil,3,nil)),4,tree(nil,5,tree(tree(nil,6,nil),7,nil))),4,Res).   Res = tree(tree(tree(nil,1,nil),2,tree(nil,3,nil)),5,tree(tree(nil,6,nil),7,nil)) 
% deleteElemBST(tree(tree(tree(nil,1,nil),2,tree(nil,3,nil)),4,tree(nil,5,tree(tree(nil,6,nil),7,nil))),8,Res).   Res = tree(tree(tree(nil,1,nil),2,tree(nil,3,nil)),4,tree(nil,5,tree(tree(nil,6,nil),7,nil)))
% deleteElemBST(tree(nil,1,nil),1,Res).  Res=nil
% deleteElemBST(tree(tree(tree(tree(nil,4,nil),10,tree(nil,11,nil)),13,tree(nil,15,tree(nil,16,nil))),17,tree(nil,21,tree(tree(nil,23,nil),24,tree(tree(nil,25,tree(nil,26,nil)),27,nil)))),1,Res).  
% Res = tree(tree(tree(tree(nil,4,nil),10,tree(nil,11,nil)),13,tree(nil,15,tree(nil,16,nil))),17,tree(nil,21,tree(tree(nil,23,nil),24,tree(tree(nil,25,tree(nil,26,nil)),27,nil))))
% deleteElemBST(tree(tree(tree(tree(nil,4,nil),10,tree(nil,11,nil)),13,tree(nil,15,tree(nil,16,nil))),17,tree(nil,21,tree(tree(nil,23,nil),24,tree(tree(nil,25,tree(nil,26,nil)),27,nil)))),4,Res).
% Res = tree(tree(tree(nil,10,tree(nil,11,nil)),13,tree(nil,15,tree(nil,16,nil))),17,tree(nil,21,tree(tree(nil,23,nil),24,tree(tree(nil,25,tree(nil,26,nil)),27,nil))))
leftBypass(nil,[]):-!.
leftBypass(tree(L,X,R),Res):-leftBypass(L,Left),leftBypass(R,Right),append(Left,[X|Right],Res),!.

isBinarySort(nil):-!.
isBinarySort(tree(L,X,R)):-X\=nil,X\=tree(_,_,_),isBinarySort(L),isBinarySort(R),leftBypass(L,Res1),leftBypass(R,Res2),checkGreater(X,Res1),checkLess(X,Res2),!.

findElemBST(nil,_):-fail.
findElemBST(tree(L,X,R),Y):-(X=Y; findElemBST(L,Y);findElemBST(R,Y)),!.

minElem(nil, _) :- fail.
minElem(tree(nil, Min, _), Min) :- !.
minElem(tree(L, _, _), Min) :- minElem(L, Min).

removeElem(tree(nil,X,nil),X,nil).
removeElem(tree(nil, X, R), X, R).
removeElem(tree(L, X, nil), X, L).
removeElem(tree(L, X, R),X,New):-
    minElem(R,MinR),
    removeElem(R,MinR,NewRT),
    New = tree(L,MinR,NewRT).
removeElem(tree(L,X,R), Rem, Res):-
    (Rem < X, removeElem(L, Rem, NewLT), Res = tree(NewLT,X,R));(removeElem(R,Rem,NewRT),Res = tree(L,X,NewRT)).

deleteElemBST(Tree, X, ResTree):-
    isBinarySort(Tree),
    (findElemBST(Tree,X), removeElem(Tree,X,ResTree),!);
    % if elem not in AVLtree
    (ResTree = Tree,!).


% Delete in AVL tree
% Let it be the bonus
% Algorythm from geeks for geeks
% deleteInAVL(tree(tree(tree(tree(nil/0,1,nil/0)/1,2,nil/0)/2,3,tree(nil/0,4,nil/0)/1)/3,5,tree(nil/0,6,tree(nil/0,7,nil/0)/1)/2)/4,7,R).    deletion with LL rotation
% Res = R = tree(tree(tree(nil/0,1,nil/0)/1,2,nil/0)/2,3,tree(tree(nil/0,4,nil/0)/1,5,tree(nil/0,6,nil/0)/1)/2)/3

minElemAVL(nil/0, _) :- fail.
minElemAVL(tree(nil/0, E, _)/_, E) :- !.
minElemAVL(tree(L, _, _)/_, E) :- minAvl(L, E).

removeElemAVL(tree(nil/0,X,nil/0)/_, X, nil/0).
removeElemAVL(tree(L, X, nil/0)/_, X, L).
removeElemAVL(tree(nil/0,X,R)/_,X, R).
removeElemAVL(tree(L, X, R)/H, X, Res):- minElemAVL(R, MinRight), removeElemAVL(R, MinRight, NewR), Res=tree(L, MinRight, NewR)/H.
removeElemAVL(tree(L, X, R)/H, Y, Res):-  (X>Y, removeElemAVL(L, Y, NewL), Res = tree(NewL, X, R)/H); (removeElemAVL(R, Y, NewR),Res = tree(L, X, NewR)/H).

% LL left left rotation
rotateLeft(tree(tree(tree(T1, X, T2)/_, Y, T3)/_, Z, T4)/_, Res):- L=tree(T1, X, T2)/0, R=tree(T3, Z, T4)/0,
  heightAVL(L, NewLH),
  heightAVL(R, NewRH),
  NewHeight is 1+max(NewLH, NewRH),
  Res=tree(tree(T1, X, T2)/NewLH, Y, tree(T3, Z, T4)/NewRH)/NewHeight.
% LR left right rotation
rotateLeft(tree(tree(T1, Y, tree(T2, X, T3)/_)/_, Z, T4)/_, Res):- L=tree(T1, Y, T2)/0, R=tree(T3, Z, T4)/0,
  heightAVL(L, NewLH),
  heightAVL(R, NewRH),
  NewHeight is 1+max(NewLH, NewRH),
  Res=tree(tree(T1, Y, T2)/NewLH, X, tree(T3, Z, T4)/NewRH)/NewHeight.
% RR right right rotation
rotateRight(tree(T1, Z, tree(T2, Y, tree(T3, X, T4)/_)/_)/_, Res):- L=tree(T1, Z, T2)/0, R=tree(T3, X, T4)/0,
  heightAVL(L, NewLH),
  heightAVL(R, NewRH),
  NewHeight is 1+max(NewLH, NewRH),
  Res=tree(tree(T1, Z, T2)/NewLH, Y, tree(T3, X, T4)/NewRH)/NewHeight.
% RL right left rotation
rotateRight(tree(T1, Z, tree(tree(T2, X, T3)/_, Y, T4)/_)/_, Res):- L=tree(T1, Z, T2)/0, R=tree(T3, Y, T4)/0,
  heightAVL(L, NewLH),
  heightAVL(R, NewRH),
  NewHeight is 1+max(NewLH, NewRH),
  Res=tree(tree(T1, Z, T2)/NewLH, X, tree(T3, Y, T4)/NewRH)/NewHeight.

makeNewHeight(T/_, H, T/H).

rebalance(nil/0, nil/0).
rebalance(tree(L, X, R)/_, Res):- rebalance(L, NewL), rebalance(R, NewR),
    heightAVL(NewL, NewLHeight), heightAVL(NewR, NewRHeight),
    Height is 1+max(NewLHeight,NewRHeight),
    makeNewHeight(NewL, NewLHeight, ActualL),
    makeNewHeight(NewR, NewRHeight, ActualR),
    BeforeBalanceTree=tree(ActualL, X, ActualR)/Height,
    Balance is NewLHeight-NewRHeight,
    % next lines doesn't work with or ; so there is arrow notation
    ( ((-1) =< Balance, Balance =< 1) -> Res=BeforeBalanceTree;
        ((Balance =:= 2) ->
            rotateLeft(BeforeBalanceTree, Res);
            rotateRight(BeforeBalanceTree, Res)
        )).
deleteInAVL(T, X, Res) :- checkAVL(T),
    (findElem(T, X),
    removeElemAVL(T, X, Temp), rebalance(Temp, Res),!);
    (Res is T, !).