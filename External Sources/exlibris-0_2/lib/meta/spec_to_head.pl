:- ensure_loaded( library('list/is_list_of_n_vars') ).

spec_to_head( Name/Arity, Head ) :-
	is_list_of_n_vars( Arity, Args ),
	Head =.. [Name|Args].
