%перелік ігор (id,назва,автор, хвилини, зі скількох, до скільки)
game(1,arkhamhorror, launius,120, gap(1,8)).
game(2,stayaway, ferrara,20, gap(3,12)).
game(3,munchkin, jackson,40, gap(2,8)).
game(4,smashup,bxerk, 40, gap(2,4)).
game(5,codenames,chvatil,30,gap(2,12)).
game(6,flickemup, beaujannot,60,gap(1,10)).


%табличка магазинів та ігор у них (id ,id, ціна, кількість)
selled(1,store(hobbyworld,poshtovaplosha,67346),1600,10).
selled(2,store(hobbyworld,oceanplaza,34567),300,20).
selled(2,store(gearup,mezhyhirska,67582),270,30).
selled(3,store(igromag,bohdanakmelnytskogo,17854),420,23).
selled(3,store(hobbyworld,poshtovaplosha,67346),450,6).
selled(4,store(hobbyworld,gulliver,78909),650,15).
selled(5,store(igromag,bohdanakmelnytskogo,17854),600,0).
selled(6,store(gearup,mezhyhirska,67582),1800,5).


% правила
%скільки коштує гра
cost(X,Y) :- game(T,X,_,_,_),selled(T,_,Y,_).
%Знайти номери телефонів магазинів, де продається вказана гра
phones(X,Y) :- game(T,X,_,_,_), selled(T,store(_,_,Y),_,_).
%отримати магазини та їхні адреси яким потрібно зробити дозакупку ігор(дозакупка ігор відбувається коли їхня кількість менше 10)
topurchase(X,Y,Z) :- selled(I,store(X,Y,_),_,Q),game(I,Z,_,_,_),Q<10.
%вказати магазини(та їхні адреси) де продаються ігри на вказане число гравців
players(X,Y,Z,V) :- selled(T,store(X,Y,_),_,_), game(T,Z,_,_,gap(L,B)),between(L,B,V).
%загальна сума грошей, які можна отримати за продаж гри у магазині
sum(X,Y,Z) :- game(T,X,_,_,_),selled(T,store(Y,_,_),O,P), Z is O*P.


run :- write('1st:'),
       forall(cost(X,Y), (write(X), write(' '), write(Y),write('\n'))),
       write('\n2nd:'),
       forall(phones(X, Y), (write(X), write(' '),write(Y),write('\n'))),
     write('\n3rd:'),
     forall(topurchase(X,Y,Z), (write(X), write(' '),write(Y),write(' '),write(Z),write('\n'))),
     write('\n4rd:'),
     forall(players(X,Y,Z,V), (write(X), write(' '),write(Y), write(' '),write(Z), write(' '),write(V),write('\n'))),
     write('\n5th:'),
     forall(sum(X, Y,Z), (write(X), write(' '),write(Y), write(' '),write(Z),write('\n'))).
     
:- initialization(run).