:- ensure_loaded( pl ).				% /1.
:- ensure_loaded( pl_match ).			% /2.
:- ensure_loaded( '../term/to_list' ).	% /2.

:- ( pl(swi(_)) -> true ; ensure_loaded( library(lists) )).

if_pl( IfPls, Call ) :-
	pl( PlSys ),
	if_pl_sys( IfPls, PlSys, Call ).

if_pl( IfPls, Then, Else ) :-
	pl( PlSys ),
	if_pl_sys( IfPls, PlSys, Then, Else ).
	
if_pl_sys( IfPls, PlSys, Call ) :-
	if_pl_sys( IfPls, PlSys, Call, true ).

if_pl_sys( IfPls, PlSys, Then, Else ) :-
	( pl_match( PlSys, IfPls ) ->
		call( Then )
		;
		call( Else )
	).
