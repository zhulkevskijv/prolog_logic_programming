% Developed by Zhulkevskyi Vladyslav

% Задача Ейнштейна

% А. Ейнштейн придумав цю задачу в минулому сторіччі та вважав, що 98% жителів Землі не в змозі її розв'язати подумки. Чи належите ви до 2% найрозумніших людей планети? Тут немає фокуса лише чиста логіка.
% Задача 
% 1. Є 5 будинків, кожен різного кольору.
% 2. В кожному будинку живе по одній людині відмінної від іншого національності.
% 3. Кожен пожилець п'є лише один визначений напій, палить певну марку цигарок та має певну тварину.
% 4. Жоден з 5 людей не п'є однакові напої, не палить однакові цигарки та не тримає однаковиу тварину.

% Питання: кому належить риба?

% Підказки:

% Англієць живе в червоному будинку
% Швед тримає собаку
% Датчанин п'є чай
% Зелений будинок стоїть зліва від білого (вважайте, що ці два будинки стоять поруч - інакше в задачі виходять два розв'язки)
% Мешканець зеленого дома п'є каву
% Людина, яка палить Pall Mall, тримає пташку
% Мешканець з середнього будинку п'є молоко
% Мешканець з жовтого будинку палить Dunhill
% Норвежець живе в першому будинку
% Курець Marlboro живе біля того, хто тримає кицьку
% Людина, яка тримає коня, живе біля того, хто курить Dunhill
% Курець цигарок Winfield п'є пиво
% Норвежець живе біля блакитного будинку
% Німець палить Rothmans
% Курець Marlboro - сусіда людини, яка п'є воду
% Відповідь: Власник риби - німець.

% Завдання. Написати програму, яка за файлом з умовами задачі Ейнштейна ти питаннями сформує відовідні предикати та видасть розв'язок.

right(X, Y) :- X is Y+1.
left(X, Y) :- Y is X+1.

neighbor(X, Y) :- right(X, Y).
neighbor(X, Y) :- left(X, Y).

makeDeduction(List,Owner) :-
    % h( Номер будинку, Колір, Національність, напій, цигарки, тварина)
    List = [
        h(1, _,_,_,_,_),
        h(2, _,_,_,_,_),
        h(3, _,_,_,_,_),
        h(4, _,_,_,_,_),
        h(5, _,_,_,_,_)],
    % Англієць живе в червоному будинку
    member(h(_, red,englishman,_,_,_), List),
    % Швед тримає собаку
    member(h(_, _,swede,_,_,dog), List),
    % Датчанин п'є чай
    member(h(_, _,dane,tea,_,_), List),
    % Зелений будинок стоїть зліва від білого
    member(h(A, green,_,_,_,_), List),
    member(h(B, white,_,_,_,_), List),
    left(A, B),
    % Мешканець зеленого дома п'є каву
    member(h(_, green,_,coffee,_,_), List),
    % Людина, яка палить Pall Mall, тримає пташку
    member(h(_, _,_,_,pall_mall,bird), List),
    % Мешканець з середнього будинку п'є молоко
    member(h(3, _,_,milk,_,_), List),
    % Мешканець з жовтого будинку палить Dunhill
    member(h(_, yellow,_,_,dunhill,_), List),
    % Норвежець живе в першому будинку
    member(h(1, _,norway,_,_,_), List),
    % Курець Marlboro живе біля того, хто тримає кицьку
    member(h(C, _,_,_,marlboro,_), List),
    member(h(D, _,_,_,_,cat), List),
    neighbor(C, D),
    % Людина, яка тримає коня, живе біля того, хто курить Dunhill
    member(h(E, _,_,_,_,horse), List),
    member(h(F, _,_,_,dunhill,_), List),
    neighbor(E, F),
    % Курець цигарок Winfield п'є пиво
    member(h(_, _,_,beer,winfield,_), List),
    % Норвежець живе біля блакитного будинку
    member(h(G, _,norway,_,_,_), List),
    member(h(H, blue,_,_,_,_), List),
    neighbor(G, H),
    % Німець палить Rothmans
    member(h(_, _,german,_,rothmans,_), List),
    % Курець Marlboro - сусіда людини, яка п'є воду
    member(h(I, _,_,_,marlboro,_), List),
    member(h(J, _,_,water,_,_), List),
    neighbor(I, J),
    member(h(_, _,Owner,_,_,fish), List).