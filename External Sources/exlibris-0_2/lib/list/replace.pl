replace( [], _This, _That, [] ).
replace( [H|T], This, That, [AtH|AtT] ) :-
	( \+ \+ H = This ->
		AtH = That
		;
		AtH = H
	),
	replace( T, This, That, AtT ).
