:- ensure_loaded( library(defines) ).		%.
:- defines( [yap(_),sicstus(_)], flatten/2 ).

%%%%%%%%
% flatten( +NestedLists, ?FlatList ) :-
% FlatList is the list of all not list elements of 
% the lists in NestedLists, in depth first, left to right
% order, from o'keefe's book p. 97-98
%%%%%%%
%
flatten( Nlist, Flist ) :-
	flatten( Nlist, Flist, [] ).
flatten( [], L0, L ) :-
	!,
	L0 = L.
flatten( [H|T], L0, L ) :-
	!,
	flatten( H, L0, L1 ),
	flatten( T, L1, L ).
flatten( Oth, [Oth|List], List ).
