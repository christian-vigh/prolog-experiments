:- ensure_loaded( library( 'compat/if_pl' ) ).
:- if_pl( not(swi(_)), ensure_loaded( library(lists) ) ).
	% memberchk/2.

% dont use require on a system pred since this is used by mkindex.
% :- ensure_loaded( library(requires) ).  
% :- requires( memberchk/2 ).

% select_preferred( +Term, +Preferred, +FallBack ) :-
% Term is unified with an element of Preferred, if 
% this is possible, or with an element in FallBack list.
% If this is not possible a warning message is printed,
% but computation doesn't fail.
%
select_preferred( Element, List1, List2 ) :-
	( memberchk(Element,List1) ->
		true
		;
		( memberchk(Element,List2) -> 
			true
			;
			write( user_error, ['Unrecognised option ',Element,' in options'] ),
			nl( user_error ),
			write( user_error, List1 ), nl( user_error ),
			write( user_error, ' or defaults ' ), nl( user_error ),
			write( user_error, List2 ), nl( user_error )
		)
	).
