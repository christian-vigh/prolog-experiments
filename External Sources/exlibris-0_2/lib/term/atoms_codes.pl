atoms_codes( [], [] ).
atoms_codes( [H|T], [HCs|TCs] ) :-
	atom_codes( H, HCs ),
	atoms_codes( T, TCs ).
