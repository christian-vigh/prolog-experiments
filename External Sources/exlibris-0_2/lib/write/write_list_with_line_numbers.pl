:- ensure_loaded( library('compat/if_pl') ).		%.
:- if_pl(swi(_),true,ensure_loaded(library('list/flatten')) ).%/2.

write_list_with_line_numbers( [], _Number, _Spaces ).
write_list_with_line_numbers( [H|T], Number, Spaces ) :-
	flatten( ["~t~d~",Spaces,"| : "], FormatThis ),
	format( FormatThis, [Number] ),
	NxN is Number + 1,
	write( H ), nl,
	write_list_with_line_numbers( T, NxN, Spaces ).
