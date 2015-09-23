head_to_spec( H, Name/Largs ) :-
	H =.. [Name|Args],
	length( Args, Largs ).
