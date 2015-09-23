 define	ERR_STORAGE_ALREADY_DEFINED		21000
# define	ERR_STORAGE_UNKNOW_STORAGE		21001

storage(Name) :-
	storage$root(Name),
	storage_error(ERR_STORAGE_ALREADY_DEFINED,
		['storage area ''', Name, ''' is already defined.']).
storage(Name) :-
	assert( storage$root(Name) ).

default(Name) :-
	var(Name),
	storage$default( Name ).
default(Name) :-
	storage$root(Name),
	retractall( storage$default(Name) ),
	assert(( storage$default(Name) )).
default(Name) :-
	storage_error(ERR_STORAGE_UNKNOWN_STORAGE, 
		['Storage area ''', Name, ''' does not exist.']).
	
	
		
		
storage_error(Id, List) :-
	atomlist_concat(List, Str),
	throw( storage_error(Id, Str) ).
	