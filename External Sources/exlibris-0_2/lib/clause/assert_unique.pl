:- ensure_loaded( library('meta/head_to_spec') ).		%.

assert_unique( Clause ) :-
	( Clause = (Head:-_Body) ->
		true
		;
		Head = Clause
	),
	head_to_spec( Head, Spec ),
	( current_predicate(Spec) ->
		abolish( Spec )
		;
		true
	),
	assert( Clause ).
