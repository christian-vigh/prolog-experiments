tuple_to_list( Var, TheVar ) :-
	var( Var ),
	!,
	TheVar = Var.
tuple_to_list( (A,B), [A|BList] ) :-
	!,
	tuple_to_list( B, BList ).
tuple_to_list( Catch, [Catch] ).
