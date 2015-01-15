%person(Inventory, Name)
%state(Person1, Person2):-
%  person(Person1),
%  person(Person2).

action(state(person(I1, N1), person(I2, N2)), [give, I], state(person(IR1, N1), person(IR2, N2))):-
  dif(N1, N2),
  member(I, I1),
  append(I2, [I], IR2),
  select(I, I1, IR1).

action(state(person(I1, N1), person(I2, N2)), [take, I], state(person(IR1, N1), person(IR2, N2))):-
  dif(N1, N2),
  member(I, I2),
  append(I1, [I], IR1),
  select(I, I2, IR2).

has-item(person(IN, _), I):-
  member(I, IN).

player-can-have(state(person(I, player), _), []):-
  member(macguffin, I).

player-can-have(State1, Plan):-
  action(State1, Action, State2),
  player-can-have(State2, PartialPlan),
  add(Action, PartialPlan, Plan).

add(X,L,[X|L]).

%?- player-can-have(state(person([], player), person([macguffin], npc)), Plan).                                   % Plan = [[take, macguffin]] .

