% append_on_dir( +Left, +Right, -Full ) :-
% Append Right onto the end of directory Left producing Full.
%
append_to_dir( Left, Right, Full ) :-
	( Left == '' ->
		Full = Right
		;
		( Right = '' ->
			Full = Left
			;
			atom_concat( Left, '/', Pfx ),
			atom_concat( Pfx, Right, Full )
		)
	).

