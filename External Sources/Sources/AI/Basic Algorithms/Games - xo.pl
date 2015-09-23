% Tic-tac-toe game tree

moves(A,B) :- % MAX (x) moves
   max(A),
   findall(A,(member(V,A),var(V),V=x),B), B\=[].
moves(A,B) :- % MIN (o) moves
   min(A),
   findall(A,(member(V,A),var(V),V=o),B),B\=[].

utility([A,B,C,_,_,_,_,_,_],V) :- xo(A,B,C,V), !.
utility([_,_,_,A,B,C,_,_,_],V) :- xo(A,B,C,V), !.
utility([_,_,_,_,_,_,A,B,C],V) :- xo(A,B,C,V), !.

utility([A,_,_,B,_,_,C,_,_],V) :- xo(A,B,C,V), !.
utility([_,A,_,_,B,_,_,C,_],V) :- xo(A,B,C,V), !.
utility([_,_,A,_,_,B,_,_,C],V) :- xo(A,B,C,V), !.

utility([A,_,_,_,B,_,_,_,C],V) :- xo(A,B,C,V), !.
utility([_,_,A,_,B,_,C,_,_],V) :- xo(A,B,C,V), !.

utility(A,0) :- ground(A).


xo(A,B,C,1) :- A==x,B==x,C==x.
xo(A,B,C,-1) :- A==o,B==o,C==o.

max(A) :-
   findall(V,(member(V,A),var(V)),L),
   length(L,N), 1 is N mod 2, !.

min(A) :-
   findall(V,(member(V,A),var(V)),L),
   length(L,N), 0 is N mod 2, !.
