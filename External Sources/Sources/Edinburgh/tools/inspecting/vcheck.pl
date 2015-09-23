%   File   : VCHECK.PL
%   Author : Richard A. O'Keefe
%   Updated: 5 August 1984
%   Purpose: Check for misspelled variables.
/*
    This file defines a predicate vcheck(File) which may be used as a
    debugging tool.  All it does is to read the clauses of the file,
    one at a time, and report which clauses contain unique variables.
    It uses the tokeniser read_tokens; either the RdTok.Pl version or
    the RdTok.Gen version will do.

Needs:
	try_hard_to_see		from Util:TrySee.Pl
	give_help		from Util:Helper.Pl
	get_file		from Util:GetFil.Pl
	read_tokens		from Util:RdTok.Pl
*/

:- public
	vcheck/0,
	vcheck/1.

:- mode
	check_tokens(+, +),
	scan_tokens(+),
	scan_variables(+,-),
	vcheck,
	vcheck(+),
	vCheck(+),		%   post hoc patch
	write_tokens(+),
	write_token(+).


vcheck :-
	getfile(File),  !,
	vCheck(File).



vCheck('') :- !.
vCheck('?') :-
	give_help(count, files), !,
	vcheck.
vCheck(File) :-
	vcheck(File), !,
	vcheck.


vcheck(File) :-
	seeing(OldSee),
	try_hard_to_see(File, [], [pl,cpl]),
	telling(OldTell),
	tell(user),
	repeat,
	    read_tokens(Tokens, Variables),
	    check_tokens(Tokens, Variables),
	!,
	seen,
	see(OldSee),
	tell(OldTell).


check_tokens([atom(end_of_file)], []) :- !.
check_tokens(Tokens, Variables) :-
	scan_tokens(Tokens),
	scan_variables(Variables, Culprits),
	Culprits \== [],
	write('** '),
	write_tokens(Culprits),
	write_tokens(Tokens), nl,
	!, fail.


scan_tokens([var(Var,_)|Rest]) :-
	var(Var), Var = once(_), !,
	scan_tokens(Rest).
scan_tokens([var(once(twice),_)|Rest]) :- !,
	scan_tokens(Rest).
scan_tokens([_|Rest]) :- !,
	scan_tokens(Rest).
scan_tokens([]).


scan_variables(['_'=_|Rest], More) :- !,
	scan_variables(Rest, More).
scan_variables([Name=once(only)|Rest], [Name|More]) :- !,
	scan_variables(Rest, More).
scan_variables([_|Rest], More) :- !,
	scan_variables(Rest, More).
scan_variables([], []).


write_tokens([Head|Tail]) :-
	put(32),
	write_token(Head), !,
	write_tokens(Tail).
write_tokens([]) :-
	write('.'), nl.

write_token(atom(X)) :- !,	write(X).
write_token(var(V,X)) :- !,	write(X).
write_token(integer(X)) :- !,	write(X).
write_token(string(X)) :- !,	write(X).
write_token(X) :- 		write(X).


