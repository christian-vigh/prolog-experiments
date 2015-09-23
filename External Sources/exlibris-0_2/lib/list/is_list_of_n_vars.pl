is_list_of_n_vars( 0, [] ) :- !.

is_list_of_n_vars( N, [H|T] ) :-
	N > 0,
	var( H ),
	N1 is N - 1,
	is_list_of_n_vars( N1, T ).
