%person(Inventory, Name, Knowledge)
%state(Person1, Person2):-
%  person(Person1),
%  person(Person2).

add(X,L,[X|L]).

action(state(person(I1, N1, _), person(I2, N2, _)), [give, I], state(person(IR1, N1, _), person(IR2, N2, _))):-
  dif(N1, N2),
  member(I, I1),
  append(I2, [I], IR2),
  select(I, I1, IR1).

action(state(person(I1, N1, _), person(I2, N2, _)), [take, I], state(person(IR1, N1, _), person(IR2, N2, _))):-
  dif(N1, N2),
  member(I, I2),
  add(I, I1, IR1),
  select(I, I2, IR2).

action(state(person(I1, N1, K1), person(I2, N2, K2)), [say, T], state(person(IR1, N1, KR1), person(IR2, N2, KR2))):-
  dif(N1, N2),
  member(T, K1),
  add(T, K2, KR2).

has-item(person(IN, _), I):-
  member(I, IN).

has-knowledge(person(_, _, K), T):-
  member(T, K).

player-can-have(state(person(I, player), _), []):-
  member(macguffin, I).

player-can-have(State1, Plan):-
  action(State1, Action, State2),
  player-can-have(State2, PartialPlan),
  add(Action, PartialPlan, Plan).


%?- player-can-have(state(person([], player), person([macguffin], npc)), Plan).                                   % Plan = [[take, macguffin]] .

