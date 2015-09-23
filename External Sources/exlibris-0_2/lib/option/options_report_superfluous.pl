:- ensure_loaded( library('list/delete_all_unif') ).	% /3.
:- ensure_loaded( library(requires) ).				% /3.
:- requires( delete_unif/3 ).						% /3.
:- ensure_loaded( library('meta/head_to_spec') ). 	% /2.
:- ensure_loaded( library('meta/spec_to_head') ). 	% /2.

options_report_superfluous( Opts, Known, ErrStr, ExistUnknowns ) :-
	delete_all_unif( Known, Opts, Unknown ),
	( Unknown == [] ->
		ExistUnknowns = false
		;
		ExistUnknowns = true
	),
	ErrStr = (PrSpec,PrArg),
	options_report_unknown( Unknown, PrSpec, PrArg ).

options_report_unknown( [], _PrSpec, _PrArg  ).
options_report_unknown( [H|T], PrSpec, PrArg ) :-
	H =.. [HNm|HArgs],
	length( HArgs, HArgsLg ),
	write( user_error, 'Unknown option ' ),
	write( user_error, HNm/HArgsLg ),
	nl( user_error ),
	write( user_error, 'in argument ' ),
	write( user_error, PrArg ),
	write( user_error, ' of predicate ' ),
	write( user_error, PrSpec ),
	head_to_spec( H, HSpec ),
	spec_to_head( HSpec, EmptyH ),
	delete_unif( EmptyH, T, NxT ),
	options_report_unknown( NxT, PrSpec, PrArg ).
