%   Author : R.A.O'Keefe
%   Updated: 28 May 1984
%   Purpose: Interactive Cross-Reference module for PP.

%   Note: this program is completely parasitic on PP.Pl.  It only provides
%   a way of building the database and a few methods for accessing it; the
%   general pattern-matching stuff comes from PP, and the access method is
%   called from it.  It also depends on Helper for try_hard_to_see.

:- public
	ixref/1,	%  inspect some files
	ixref_path/3,	%  for 'setof'
	(ct)/0, (ct)/1,	%  calling tree
	(sf)/0, (sf)/1, (sf)/2,
	(sp)/1, (sp)/2.	%  show paths

:- mode
    ixref(+),		%  read a list of files and update the database
	get_from(+,+),
	    start_ixref(+,+),
	    ixref_process(+,+,+),
		ixref_command(+),
		    ixref_declaration(+),
		ixref_head(+,+,+,-,-),
		ixref_goal(+,-,-),
	ixref_Current(+, -, -),
	ixref_Pattern(+),
    cassert(+),		%  put something in DataBase if not already there
    ct, ct(+),
	ct(+,+,+),
	    ct(+,+,+,+,-,+,-),
		ct_prefix(+,-,+,+,+),
		ct(+,+,+,-,+,-),
    sf, sf(+), sf(+,-),
    sp(+), sp(+,-),
	ixref_path(+, +, -),
	    ixref_path(+, ?, -, +),
		memberchk(+, +).
    
:-	op(900,  fx, [ct,sf,sp]).


ixref(Files) :-
	nofileerrors,
	(   call('$seen'('util:ixref.def'))
	;   get_from('util:ixref.def', +)
	),  !,
	get_from(Files, +),
	fileerrors.


get_from([Head|Tail], Flag) :-
	!, get_from(Head, Flag),
	!, get_from(Tail, Flag).
get_from([],	      Flag) :- !.
get_from(erase(File), Flag) :- !,
	start_ixref(File, erase).
get_from(-File,       Flag) :- !,
	get_from(File, -).
get_from(File, Flag) :-
	seeing(OldFile),
	try_hard_to_see(File, [press,extras,mec,util,pll], [pl,def]),
	seeing(NewFile),
	start_ixref(NewFile, Flag),
	repeat,
	    read(Term),
	    expand_term(Term, Form),
	    ixref_process(Form, NewFile, Flag),
	    Form = end_of_file,
	!,
	seen,
	see(OldFile).


	start_ixref(File, Flag) :-
		retract('$seen'(File)),			%   File has been seen before
		retract('$defn'(Fn,Ar, File)),		%   Fn/Ar is defined in File
		retract('$call'(Fn,Ar, _,_)),		%   forget all its calls
		retract('$call'(Fn,Ar, _)),		%   forget what it applies
		fail.					%   failure-driven LOOP
	start_ixref(File, erase) :- !.
	start_ixref(File, Flag) :-
		assertz('$seen'(File)).


	ixref_process(end_of_file,    File, Flag) :- !.
	ixref_process((Head :- Body), File, Flag) :- !,
		ixref_head(Head, File, Flag, HeadFn,HeadAr),
		ixref_goal(Body, GoalFn,GoalAr),
		cassert('$seen'(GoalFn,GoalAr)),
		cassert('$call'(HeadFn,HeadAr, GoalFn,GoalAr)).
	ixref_process((:- Commands),  File, Flag) :- !,
		ixref_command(Commands).
	ixref_process((?- Question),  File, Flag) :- !.
	ixref_process(system(Head),	File, Flag) :- !,
		ixref_head(Head, utility, Flag, HeadFn,HeadAr).
	ixref_process(known(Head, F), File, Flag) :- !,
		ixref_head(Head, F, Flag, HeadFn,HeadAr).
	ixref_process(op(P, T, O),	File, Flag) :- !,
		op(P, T, O).
	ixref_process(applies(G, A),	File, Flag) :-
		var(A), !,
		cassert('$call'(G, A, 0)).
	ixref_process(applies(G, A+N),File, Flag) :- !.
		cassert('$call'(G, A, N)).
	ixref_process(Fact,	 	File, Flag) :- !,
		ixref_head(Fact, File, Flag, HeadFn,HeadAr).


	ixref_command((A,B)) :-
		ixref_command(A), !,
		ixref_command(B).
	ixref_command(op(P, T, O)) :- !,
		op(P, T, O).
	ixref_command([X|Y]) :- !,
		get_from([X|Y], +).
	ixref_command(consult(Files)) :- !,
		get_from(Files, +).
	ixref_command(reconsult(Files)) :- !,
		get_from(Files, -).
	ixref_command(compile(Files)) :- !,
		get_from(Files, -).
	ixref_command((public Public)) :- !,
		ixref_declaration(Public).
	ixref_command((mode Mode)) :- !,
		ixref_declaration(Mode).
	ixref_command(_).

	%   handle :- public and :- mode declarations.  The information
	%   should be stored somewhere for the sake of MEDIC, but until
	%   all these tools are properly fitted together it doesn't matter.

		ixref_declaration((A,B)) :-
			ixref_declaration(A), !,
			ixref_declaration(B).
		ixref_declaration(Functor/Arity) :- !,
			cassert('$seen'(Functor, Arity)).
		ixref_declaration(Term) :-
			functor(Term, Functor, Arity),
			cassert('$seen'(Functor, Arity)).


	ixref_head(Head, File, Flag, Functor, Arity) :-
		functor(Head, Functor, Arity),
		call('$defn'(Functor, Arity, File)), !.
	ixref_head(Head, File, Flag, Functor, Arity) :-
		functor(Head, Functor, Arity),
		(   call('$defn'(Functor, Arity, OtherFile)),
			OtherFile \== File,
			(Flag == - ; OtherFile = utility),
			display('** '), display(File),
			display(' redefines '), display(Functor),
			display(/), display(Arity),
			display(' which belongs to '),
			display(OtherFile), ttynl
		;   true
		),  !,
		cassert('$seen'(Functor, Arity)),
		cassert('$defn'(Functor, Arity, File)).


	ixref_goal(Goal,    Fn,Ar) :-
		var(Goal), !,
		fail.
	ixref_goal((G1,G2), Fn,Ar) :-
		ixref_goal(G1, Fn,Ar).
	ixref_goal((G1,G2), Fn,Ar) :- !,
		ixref_goal(G2, Fn,Ar).
	ixref_goal((G1;G2), Fn,Ar) :-
		ixref_goal(G1, Fn,Ar).
	ixref_goal((G1;G2), Fn,Ar) :- !,
		ixref_goal(G2, Fn,Ar).
	ixref_goal(Goal,    Fn,Ar) :-
		call('$call'(Goal, Argument, Extra)),
		nonvar(Argument),
		functor(Argument, Fn, Small),
		Ar is Small+Extra.
	ixref_goal(Goal,    Fn,Ar) :-
		functor(Goal, Fn,Ar),
		call('$defn'(Fn,Ar, utility)),
		!, fail.
	ixref_goal(Goal,    Fn,Ar) :-
		functor(Goal, Fn,Ar).


cassert(Fact) :-
	call(Fact), !.
cassert(Fact) :-
	assertz(Fact).


%   The following predicate accesses the IXREF data-base.
%	from(-)		-- called but not defined
%	from(F)		-- defined in file F
%	tops(F)		-- defined in file F, not called in file F
%	>(-)		-- defined but calling nothing
%	>(Pattern)	-- calling something matching Pattern
%	<(-)		-- defined but not called
%	<(Pattern)	-- called by something matching Pattern
%	@>(Pattern)	-- calling Pattern = closure of >
%	@<(Pattern)	-- called by Pattern = closure of <

ixref_Pattern(from(_)).
ixref_Pattern(tops(_)).
ixref_Pattern(>(_)).
ixref_Pattern(<(_)).
ixref_Pattern(@>(_)).
ixref_Pattern(@<(_)).


ixref_Current(from(-), Functor, Arity) :- !,
	call('$seen'(Functor, Arity)),
	\+ call('$defn'(Functor, Arity, File)).
ixref_Current(from(File), Functor, Arity) :- !,
	call('$defn'(Functor, Arity, File)).
ixref_Current(tops(File), Functor, Arity) :- !,
	call('$defn'(Functor, Arity, File)),
	File \== utility,
	\+ ( '$call'(F, N, Functor, Arity), '$defn'(F, N, File) ).
ixref_Current(>(-), Functor, Arity) :- !,
	call('$defn'(Functor, Arity, _)),
	\+ call('$call'(Functor, Arity, _, _)).
ixref_Current(>(Pattern), Functor, Arity) :- !,
	isCurrent(Pattern, sp, G/B),
	call('$call'(Functor, Arity, G, B)).
ixref_Current(<(-), Functor, Arity) :- !,
	call('$defn'(Functor, Arity, File)),
	File \== utility,
	\+ call('$call'(_, _, Functor, Arity)).
ixref_Current(<(Pattern), Functor, Arity) :- !,
	isCurrent(Pattern, sp, G/B),
	call('$call'(G, B, Functor, Arity)).
ixref_Current(@>(Pattern), Functor, Arity) :- !,
	ixref_path(Functor/Arity, Pattern, _).
ixref_Current(@<(Pattern), Functor, Arity) :- !,
	ixref_path(Pattern, Functor/Arity, _).


/*----------------------------------------------------------------------+
|									|
|			   Seen File ?					|
|									|
|   The predicates provided to the user are				|
|	sf(Pattern, Files)		-- return selected filenames	|
|	sf(Pattern)			-- display selected filenames	|
|	sf				-- display all file names	|
|									|
|   Once again, there are two sorts of patterns, and keeping them apart	|
|   is confusing.  If the pattern is a string, the user is told which	|
|   files have been seen whose names match the pattern.  Otherwise, he	|
|   is told which files have been seen that defined predicates matching	|
|   the pattern.  E.g. sf "fre*" might locate a file 'fred.pl', while	|
|   sf ["fre*"] will locate files defining predicates fred...		|
|									|
+----------------------------------------------------------------------*/

sf :-
	sf("*").

sf(Pattern) :-
	sf(Pattern, Files),
	answer_List(Files, 32).

sf([Head|Tail], Files) :-
	integer(Head), !,
	setof(File, ('$seen'(File), isCurrent([Head|Tail], File)), Files).
sf(Pattern, Files) :-
	setof(File, ('$defn'(F,A,File), isCurrent(Pattern, cf, F/A)), Files).


/*----------------------------------------------------------------------+	
|									|
|			      Show Paths				|
|									|
|   The predicates provided for the user are				|
|	sp(Limits, Paths)		-- return paths			|
|	sp(Limits)			-- display paths		|
|   Note that there is no sp/0, as the complete list of paths is as	|
|   long as it is boring.						|
|	A path is a list [F0/N0, ..., Fk/Nk] where each entry names	|
|   a predicate, and Fi/Ni calls Fi+1/Ni+1, and no entry appears more	|
|   than once.  It describes in detail how F0/N0 may call Fk/Nk.  For	|
|   my convenience, this is the scheme used to implement @> and @<.	|
|   The Limits are							|
|	FirstCaller - LastCalled					|
|	- LastCalled							|
|	FirstCaller							|
|   where FirstCaller, LastCalled are TermPatterns.			|
|									|
+----------------------------------------------------------------------*/

sp(Limits) :-
	sp(Limits, Paths),
	answer_List(Paths, 31).

sp(FirstCaller-LastCalled, Paths) :- !,
	setof(Path, ixref_path(FirstCaller, LastCalled, Path), Paths).
sp(-LastCalled, Paths) :- !,
	sp("*"-LastCalled, Paths).
sp(FirstCaller, Paths) :-
	sp(FirstCaller-"*", Paths).


ixref_path(First, Last, [FirstSpec|Path]) :-
	isCurrent(First, sp, FirstSpec),
	ixref_path(FirstSpec, LastSpec, Path, [FirstSpec]),
	isCurrent(Last, sp, LastSpec).

		ixref_path(F/A, G/B, [H/C|Path], Forbidden) :-
			call('$call'(F, A, H, C)),
			\+ memberchk(H/C, Forbidden),
			ixref_path(H/C, G/B, Path, [H/C|Forbidden]).
		ixref_path(F/A, F/A, [], _).

			memberchk(H, [H|_]) :- !.
			memberchk(X, [_|T]) :- memberchk(X, T).


/*----------------------------------------------------------------------+	
|									|
|			      Call Tree					|
|									|
|   The predicates provided for the user are				|
|	ct				-- call tree for all top preds	|
|	ct(Pattern)			-- call tree for each match	|
|   A call tree is a way of displaying who calls whom in a compact and	|
|   readable table.  DOCUMENT THIS FURTHER.				|
|									|
+----------------------------------------------------------------------*/


ct :-
	ct(tops(_)).


ct(Pattern) :-
	cf(Pattern, Predicates),
	ct(Predicates, 0, []).


ct([], _, _) :- !.
ct([Functor/Arity|Predicates], LinesSoFar, UsedSoFar) :-
	call('$defn'(Functor, Arity, File)),
	File \== utility,
	!,
	ct(Functor, Arity, 0, LinesSoFar, Lines, UsedSoFar, Used),
	nl,
	ct(Predicates, Lines, Used).
ct([_|Predicates], Lines, Used) :-
	ct(Predicates, Lines, Used).


ct_prefix(Line0, Line, Depth, Functor, Arity) :-
	Line is Line0+1,
	(   Line < 10,  put(32), put(32)
	;   Line < 100, put(32)
	;   true
	),  !,
	write(Line), put(32),
	tab(Depth),
	write(Functor), put(47), write(Arity).


ct(Functor, Arity, Depth, L0, L, Used, Used) :-
	memberchk(f(Functor,Arity,Line), Used),
	!,
	ct_prefix(L0, L, Depth, Functor, Arity),
	write('  % see '), write(Line), nl.
ct(Functor, Arity, Depth, L0, L, U0, Used) :-
	'$defn'(Functor, Arity, File),
	!,
	ct_prefix(L0, L1, Depth, Functor, Arity),
	write('  % from '), write(File), nl,
	NewDepth is Depth+3,
	findall(F/A, '$call'(Functor,Arity,F,A), Children),
	ct(Children, NewDepth, L1, L, [f(Functor,Arity,L1)|U0], Used).
ct(Functor, Arity, Depth, L0, L, Used, Used) :-
	ct_prefix(L0, L, Depth, Functor, Arity),
	write('  % UNDEFINED'), nl.


ct([], _, L, L, U, U) :- !.
ct([F/A|Ch], D, L0, L, U0, U) :-
	ct(F, A, D, L0, L1, U0, U1),
	ct(Ch, D, L1, L, U1, U).




