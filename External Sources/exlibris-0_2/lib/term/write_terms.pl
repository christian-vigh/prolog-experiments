write_terms( File, Terms ) :-
	write_terms( File, write, Terms, [] ).

write_terms( File, Mode, Terms, Opts ) :-
        open( File, Mode, Stream ),
        write_terms_stream( Stream, Terms, Opts ),
        close( Stream ).

write_terms_stream( _Stream, [], _Opts ).
write_terms_stream( Stream, [H|T], Opts ) :-
	( H == [] ->
		nl( Stream )
		;
		write_term( Stream, H, Opts ),
		write( Stream, '.' ),
		nl( Stream )
	),
	write_terms_stream( Stream, T, Opts ).
