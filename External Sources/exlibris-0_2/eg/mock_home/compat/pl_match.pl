:- ensure_loaded( pl ).
:- ( pl(swi(_)) -> true ; ensure_loaded(library(lists)) ).	% member/2.

pls_match( [], _Ifs, [] ).
pls_match( [H|T], Ifs, Match ) :-
	( pl_match( H, Ifs ) ->
		Match = [H|TMatch]
		;
		TMatch = Match
	),
	pls_match( T, Ifs, TMatch ).
	
pl_match( Sys, Ifs ) :-
	( Ifs == any ->
		true
		;
		( Ifs = not(If) ->
			\+ pl_match( Sys, If )
			;
			pl_match_to_list( Ifs, IfList ),
			member( OneIf , IfList ),
			pl_match_one( Sys, OneIf )
		)
	),
	!.

pl_match_one( Sys, OneIf ) :-
	( OneIf = not( OthIf ) ->
		\+ pl_match_one( Sys, OthIf )
		;
		( OneIf = (OneLf,OneRt) ->
			( atomic(OneLf) ->
				pl_match_operators( Sys, OneLf, OneRt )
				;
				pl_match_term_operators( Sys, OneLf, OneRt )
			)
			;
			\+ \+ Sys = OneIf
		)
	).

pl_match_operators( Sys, OneNm, VersOps ) :-
	( VersOps = not( InVersOps ) ->
		\+ pl_match_operators( Sys, OneNm, InVersOps )
		;
		Sys =.. [OneNm,SysVer],
		pl_match_to_list( VersOps, VersOpsList ),
		findall( Op, (
				member((Ver,Op),VersOpsList),
				OpCall =.. [Op,Ver,SysVer],
				\+ call( OpCall )
				) , NotSat ),
		NotSat == []
	).

pl_match_term_operators( Sys, ToSys, ToOps ) :-
	Sys =.. [SysNm,SysVer],
	ToSys =.. [SysNm,ToSysVer],
	( ToOps = not(InOps) ->
		\+ pl_match_term_operators( Sys, ToSys, InOps )
		;
		pl_match_to_list( ToOps, OpsList ),
		findall( Op, (
				member(Op,OpsList),
				OpCall =.. [Op,ToSysVer,SysVer],
				\+ call( OpCall )
				) , NotSat ),
		NotSat == []
	).

pl_match_to_list( This, List ) :-
	( (var(This);(This\=[_|_],This\==[])) ->
		List = [This]
		;
		List = This
	).
