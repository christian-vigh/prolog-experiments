:- ensure_loaded( library(requires) ).
:- requires( [directory_files/2,delete/3] ).

directory_files_nodots( Dir, Entries ) :-
	directory_files( Dir, AllEntries ),
	delete( AllEntries, '.', NoSingleEntries ),
	delete( NoSingleEntries, '..', Entries ).
