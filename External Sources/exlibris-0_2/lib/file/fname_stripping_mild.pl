:- ensure_loaded( library('compat/if_pl') ).				% /2.
:- if_pl( not(swi(_)), ensure_loaded(library(lists)) ).  	% append/3

% :- [fname_stripping_mild/4]
% a stronger version is due, which will be more 
% energetic about stripping off the last extension of
% Fname...
%
fname_stripping_mild( Fname, ExtCs, Stem, SurelyExtFname ) :-
	atom_codes( Fname, FnameCs ),
	( append( StemCs, ExtCs, FnameCs ) ->
		atom_codes( Stem, StemCs ),
		SurelyExtFname = Fname
		;
		Stem = Fname, % this makes it mild 
		append( FnameCs, ExtCs, SurelyExtFnameCs ),
		atom_codes( SurelyExtFname, SurelyExtFnameCs )
	).
