:- ensure_loaded( library(lists) ).	% memberchk/2, reverse/2, is_list/1.
:- ensure_loaded( library('term/to_list') ). %

option_match( OptNms, OptArg, Values, ErrStr, RevOpts, Defs ) :-
	reverse( RevOpts, Opts ),
	( is_list(OptNms) ->
		OptNms = [OptNm|ToptNms]
		;
		OptNm = OptNms, ToptNms = []
	),
	to_list( OptArg, OptArgs ),
	Opt =..  [OptNm|OptArgs],
	( memberchk(Opt,Opts) ->
		option_values_check( OptArg, 'Given ', Values, ErrStr, OptNm )
		;
		( ToptNms = [] ->
			( is_list(Defs) ->
				( memberchk(Opt,Defs) ->
					option_values_check( OptArg, 'Default ', Values, ErrStr, OptNm )
					;
					option_error_missing( ErrStr, OptNm, OptArgs )
				)
				;
				call( Defs )
			)
			;
			option_match( ToptNms, OptArg, Values, ErrStr, Opts, Defs )
		)
	).

option_values_check( OptArg, Wh, Values, ErrStr, OptNm ) :-
	( \+ \+ memberchk(OptArg,Values) ->
		true
		;
		option_match_domain_error( ErrStr, Wh, OptNm, OptArg, Values )
	).

option_match_domain_error( ErrStr, Wh, OptNm, OptArg, Values ) :-
	ErrStr = (PrSpec,PrArg),
	write( user_error, Wh ),
	write( user_error, ' value not in domain for argument, ' ),
	write( user_error, PrArg ),
	write( user_error, 'of predicate ' ),
	write( user_error, PrSpec ),
	write( user_error, '.' ), nl( user_error ),
	write( user_error, 'For option ' ),
	write( user_error, OptNm ),
	write( user_error, ' found value ' ),
	write( user_error, OptArg ),
	write( user_error, ' whilst expecting one of ' ),
	write( user_error, Values ), nl( user_error ),
	abort.

option_error_missing( none, _OptNm, _OptArgs ) :-
	!.
option_error_missing( ErrStr, OptNm, OptArgs ) :-
	length( OptArgs, OptArt ),
	ErrStr = (PrSpec,PrArg),
	write( user_error, 'Could not locate option ' ),
	write( user_error, OptNm/OptArt ),
	nl( user_error ),
	write( user_error, ' for argument number ' ),
	write( user_error, PrArg ), 
	write( user_error, ' of predidate ' ),
	write( user_error, PrSpec ),
	write( user_error, ' .' ),
	nl( user_error ), abort.
