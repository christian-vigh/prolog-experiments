

parse_open(File, ID) :-
	open(File, read, ID).
	
parse_close(ID) :-
	close(ID).
	
	
parse_next(ID, String) :-
	do_parse_next(ID, 