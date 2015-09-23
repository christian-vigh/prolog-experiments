:- ensure_loaded( library('compat/pl') ).
:- ensure_loaded( library('compat/pl_match') ). 	% /2.
:- ensure_loaded( library('compat/if_pl') ). 	% /2.
:- if_pl( swi(_), ensure_loaded(library('compat/swi/file_exists')), ensure_loaded(library(system)) ).
% :- ensure_loaded( library(system) ). 	% file_exists/2.
:- ensure_loaded( library('file/append_to_dir') ). % /3.
:- ensure_loaded( library('meta/spec_to_head') ). 	%
:- ensure_loaded( library('term/to_list') ). 		% /2.

:- multifile index/5.
:- dynamic index/5.

requires( Spec ) :-
	to_list( Spec, Specs ),
	ensure_lib_dirs_loaded,
	ensure_requires_loaded( Specs ).

clean_requires_indices :-
	retractall( index/5 ).

ensure_requires_loaded( [] ).
ensure_requires_loaded( [Nm/Ar|T] ) :-
	spec_to_head( Nm/Ar, Head ),
	( predicate_property(Head,built_in) ->
		true
		;
		findall( (Mod,File), matching_index(Nm,Ar,Mod,File), ModFiles ),
		( ModFiles = [(HMod,HFile)|_T] ->
			( HMod == built_in ->  
				true
				;
				ensure_loaded( library(HFile) )
			)
			;
			write( user_error, 'Cannot locate source file for predicate ' ),
			write( user_error, Nm/Ar ), nl( user_error ),
			write( user_error, 'in requires/1.' ), nl( user_error )
		)
	),
	ensure_requires_loaded( T ).

matching_index( Nm, Ar, Mod, File ) :-
	index( Nm, Ar, IndPls, Mod, File ),
	pl( Sys ),
	pl_match( Sys, IndPls ).

ensure_lib_dirs_loaded :-
	library_directory( LibDir ),
	absolute_file_name( LibDir, AbsLDir ),
	append_to_dir( AbsLDir, 'Index.pl', AbsIndex ),
	file_exists( AbsIndex ),
	ensure_loaded( AbsIndex ),
	fail.
ensure_lib_dirs_loaded.
