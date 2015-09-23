%%%%%%%%
% break_list_on( +List, +Element, ?LeftPartition, ?RightPartition ).
% Element does nto appear in either the end of LeftPartition,
% or as first element of RightPartition.
% Only finds first partiotion so Element should be ground 
% | ?- break_list_on( L, El, [a], [c,b,d,b,e] ).
%  = [a,El,c,b,d,b,e] ? ; no 
%
break_list_on( [X|Xs], X, [], Xs ) :-
	!.
break_list_on( [X|Xs], Xa, [X|XLa], XRa ) :-
	break_list_on( Xs, Xa, XLa, XRa ).
