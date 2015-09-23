:- if_pl( not(swi(_)), ensure_loaded( [library(system),library(lists)]) ).				% file_exists/1,
											% file_property/2.
:- ensure_loaded( library('compat/if_pl') ).			% /2.
:- if_pl( swi(_), ensure_loaded([library('file/is_a_directory')]) ). % /1.
:- ensure_loaded( library('list/delete_leading') ).	% /3.
:- ensure_loaded( library('list/break_list_on') ).	% /4.

basedir( Fname, BaseDir ) :-
	atom_codes( Fname, FnameCs ),
	reverse( FnameCs, RevCs ),
	( (file_exists(Fname),file_property(Fname,type(directory))) ->
		delete_leading( RevCs, 0'/, ClRevCs )
		;
		break_list_on( RevCs, 0'/, _Left, ClRevCs )
	),
	( break_list_on(ClRevCs,0'/,RevBaseDirCs,_Right) ->
		reverse( RevBaseDirCs, BaseDirCs )
		;
		reverse( ClRevCs, BaseDirCs ),
		BaseDirCs = ClRevCs
	),
	atom_codes( BaseDir, BaseDirCs ).
