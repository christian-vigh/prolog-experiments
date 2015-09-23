# define	ERR_CLASS_ALREADY_EXISTS		20000

initialize :-
	nl, nl, write('********  Class package Version 0.1  ********'), nl, nl,
	class( generic, 
		[
			category(domains, keyword:domain,
			 [
			 	[parameter:Name, classification:atom      , get:default, set:default],
			 	[parameter:Type, classification:check_type, get:default, set:default]
			  ],
			  
		 ] ).
		  
class(Name, Defs) :-
	define_class(null, Name, Defs).
	
class(Parent, Name, Defs) :-
	define_class(Parent, Name, Defs).
	
	

% Class definition
define_class(Parent, Name, Defs) :-
	class$class(Parent, Name),
	class_error(ERR_CLASS_ALREADY_EXISTS, 
		['The specified class ''', Name, ''' (from parent ''', Parent,
		 ''') is already defined.']).
define_class(Parent, Name, Defs) :-
	assert( class$class(Parent, Name) ).
		 
	 
class_error(ErrNumber, List) :- 
	atomlist_concat(List, Msg),
	throw( class_error(ErrNumber, Msg) ).
		
			
:- initialize.	