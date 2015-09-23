% contains( BigList, SmallList ).
% BigLists contains as a whole the smallList.
% if succeeds it only does so once.
contains( [H|T], [H|R] ) :-
     contains_1( R, T ),
     !.
contains( [_H|T], List ) :-
     contains( T, List ).

contains_1( [], _Any ) :- !.
contains_1( [H|T], [H|R] ) :-
     contains_1( T, R ).
