:- ensure_loaded( library(defines) ).
:- defines( [sicstus(_),yap(_)], delete_unif/3 ).

delete_unif( [], _El, [] ).
delete_unif( [H|T], Elem, Rest ) :-
	( \+ \+ (H = Elem) -> 
		Rest = More
		;
		Rest = [H|More]
		),
	delete_unif( T, Elem, More ).
