:- ensure_loaded( library(defines) ).		%.
:- for( swi(_) ).

portray_clause( Stream, Term ) :-
	current_output( Out ),
	set_output( Stream ),
	portray_clause( Term ),
	set_output( Out ).
