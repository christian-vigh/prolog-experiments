:- ensure_loaded( library(requires) ).
:- requires( shell/2 ).
:- requires( if_pl/3 ).

sys_info_on( System, Flag, Stream ) :-
	if_pl( yap(_), system( System, Status ), shell( System, Status ) ), 
	( Status = 0 -> StatusTerm = ok(System)
			   ;	 StatusTerm = exit(Status,System)
	),
	( Flag == true ->
		write( Stream, StatusTerm ),
		nl( Stream )
		;
		true
	).
