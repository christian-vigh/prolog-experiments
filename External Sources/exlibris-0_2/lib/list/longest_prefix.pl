longest_prefix( [H|T1], [H|T2], [H|TPfx] ) :-
	!,
	longest_prefix( T1, T2, TPfx ).
longest_prefix( _L1, _L2, [] ).

