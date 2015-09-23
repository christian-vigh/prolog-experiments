% Auxiliary Predicates
% =========================================================

% no_doubles(+Xs,-Ys)
% ----------------------------------------------------------
% Ys is Xs with all duplicate elements removed
% keeps earlier occurrences wrt order, as in
%   no_doubles([a,b,a],[a,b])
% could be written more efficiently with ! 
% ----------------------------------------------------------
no_doubles([],[]).
no_doubles([X|Xs],[X|Ys]):-
  remove(Xs,X,Xs2),
  no_doubles(Xs2,Ys).

% remove(+Xs,+X,?Ys)
% ----------------------------------------------------------
% Ys is Xs with all occurrences of X removed
% ----------------------------------------------------------
remove([],_,[]).
remove([Y|Xs],X,[Y|Zs]):-
  \+ X==Y,
  remove(Xs,X,Zs).
remove([Y|Xs],X,Zs):-
  X == Y,
  remove(Xs,X,Zs).

% append(?Xs,?Ys,?Zs)
% ----------------------------------------------------------
% Zs is Xs concatenated to Ys
% ----------------------------------------------------------
append([],Xs,Xs).
append([X|Xs],Ys,[X|Zs]):-
  append(Xs,Ys,Zs).


member(X,[X|_]).
member(X,[_|Xs]):-
  member(X,Xs).


reverse(Xs,Ys):-
  reverse(Xs,[],Ys).

reverse([],Xs,Xs).
reverse([X|Xs],Ys,Zs):-
  reverse(Xs,[X|Ys],Zs).


esetof(_,G,[]):-
  \+ G.
esetof(X,G,Xs):-
  setof(X,G,Xs).

% cat_atoms/3
% -----------
cat_atoms(A1,A2,A3):-
  name(A1,L1),
  name(A2,L2),
  append(L1,L2,L3),
  name(A3,L3).


printgoal([],G):-
  numbervars(G,0,_),
  write(G), write('.'), nl.
printgoal([G1|Gs],G):-
  numbervars(temp(G,G1,Gs),0,_),
  write(G), write(':-'), nl,
  printgoals(Gs,G1).

printgoals([],G):-
  tab(2), write(G), write('.'), nl.
printgoals([G|Gs],G1):-
  tab(2), write(G1), write(','), nl,
  printgoals(Gs,G).
