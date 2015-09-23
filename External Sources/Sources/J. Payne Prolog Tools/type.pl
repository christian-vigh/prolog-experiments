%   File   : TYPE.PL
%   Author : R.A.O'Keefe
%   Updated: 21 June 1983
%   Purpose: Command to display files.

%   "type [a,b,c]" will see a, b, c and will display them
%   on the terminal.  Output redirection will have no effect on it, a
%   more general command can be made by editing this one.

%  By consulting the file TYPE_FILE_OPS you get the former
%  Dec10 Util commands: ty +File and type +File.
%  (Of course, you have to cunsult this file as well.)
%  This version does not try to imitate "try_hard_to_see".
%  You quote the full file name.

:- op(940, fx, [ty,type]).

ty File :-
	type File.

type Var  :-
	var(Var),
	!,
	write('! variable given as file name'), nl,
	fail.

type [Head|Tail]  :- !,
	 ype(Head),
	!,
	type(Tail).

type File  :-
	seeing(Old),
	see(File),
	nl,
	repeat,
		get0(Ch),
		(
			Ch =:= -1	% NIP End of File
		;
			put0(Ch),
			fail
		),
	!,
	seen,
	see(Old).



