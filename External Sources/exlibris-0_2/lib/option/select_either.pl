:- ensure_loaded( library(requires) ).
:- requires( memberchk/2 ).

% select_either( Term, Preferred, FallBack ) :-
% Term is unified with an element of Preferred, if 
% this is possible, or with an element in FallBack list.
% If Term unifies with neither, then predicate fails,
% unlike, select_preferred/3.
%
select_either( Element, List1, List2 ) :-
	( memberchk(Element,List1) -> 
		true
		;
		memberchk(Element,List2)
	).
