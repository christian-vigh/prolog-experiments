delete_leading( List, Elem, Clean ) :-
	( List = [H|T] ->
		( H \= Elem -> 
			Clean = List
			;
			delete_leading( T, Elem, Clean )
		)
		;
		Clean = List
	).
