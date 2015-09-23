:- ensure_loaded( '../mock_home/add_library_directory' ).
:- add_library_directory( abs_ald('../mock_home') ).


:- ensure_loaded( library('compat/if_pl') ).		%
:- ensure_loaded( library(requires) ).			%
:- if_pl( swi(_), true, ( ensure_loaded(library(lists)), requires((flatten/2)) ) ).
% :- ensure_loaded( library('ensure/may_load') ).	% /1.
% :- ensure_loaded( library(requires) ).			% /1.
:- if_pl( yap(_), ensure_loaded(library('ensure/may_load')), require([may_load/1]) ).
:- may_load( library('file/basedir') ).   		% /2.

one_file( List ) :-
	one( List, '', All ),
	atom_codes( All, AllCs ),
	flatten( [AllCs,AllCs], FlatCs ),
	reverse( FlatCs, RvFlatCs ),
	atom_codes( RvFlat, RvFlatCs ),
	write( flat(RvFlat) ), nl,
	ensure_loaded( library('file/basedir') ),
	basedir( '/abc/xyz/tomorrow', Base ),
	write( basedir(Base) ), nl.

one( [], All, All ).
one( [H|T], Acc, All ) :-
	atom_concat( H, Acc, NxAcc ),
	one( T, NxAcc, All ).
