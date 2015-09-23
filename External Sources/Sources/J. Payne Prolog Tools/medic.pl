% Needs: append

/*----------------------------------------------------------------------------

	Mode Error Diagnosis in Interpreted Code
	----------------------------------------

		A Prolog Debugging Aid.


    This little package is supposed to help a Prolog programmer find mode
errors in his program.  It provides a new consulting routine called "mode_chk"
which reads an unmodified file just like compile, consult, or reconsult.
The new evaluable predicate "expand_term" provided in version 3 Prolog permits
the use of MEDIC with DCGs and any future extensions to Prolog made in that
way.  A procedure whose mode is being checked may be spied and traced like
an ordinary Prolog procedure, because it IS an ordinary Prolog procedure.

    Just write "mode_chk(File)" instead of "compile(File)".  The effect of
this is similar to the effect of "reconsult(File)", NOT "consult(File)".

    When a mode violation is detected, an error message will be printed and
you will be put in a break.  If you simply exit from the break with ^Z the
program will continue just as if nothing was wrong.  This will happen EACH
time the error is detected;  you can disable/enable it, however.  First, an
example.  Suppose you said   mode_chk(tom)  where
tom:
	:- mode dick(+, -).
	dick(f(X, Y), Z) :-
		... .
and then called  dick(U, 1+1).  The message will be
	! Mode error: dick(+,-) called by
	dick(_1763,1+1)
	--- break ---

    If you have worked out why some mode is wrong, but would like to keep on
debugging, you can disable the checking by calling
	well(Functor, Arity).
There is still a certain amount of overhead associated with the procedure,
but at least you won't get the error messages any more.  If you change your
mind and want to see error messages again for that procedure, call
	sick(Functor, Arity).

    NB: mode_chk doesn't understand about spy-points.  It will not preserve them,
and neither will "sick" and "well".  Nor is "mode_chkation" transparent to spying.
If you want a procedure to be checked and spied, you will have to spy on it
again every time mode_chk does something to it.

----------------------------------------------------------------------------*/

:- public
	mode_chk/1,
	well/2,
	sick/2,
	'med$check'/2.

:- mode
	mode_chk(?),	%  playing safe, should be +
	    rest(+, +),
		handle(+, +, -),
		    modes(+),
			compare(+, +),
			genname(+, -),
		    define(+, +, +, -),
			change(+, +, -),
			passed(+, +, +, -),
			    passed(+, +, +),
		    others(+),
	sick(?, ?), well(?, ?),    %  playing safe again
	    genterms(+, +, +, +, -, -),
	'med$check'(+, +),
	    check_args(+, +).


mode_chk(File) :-
	atom(File),
	seeing(OldFile),
	see(File), !,
	read(Term),
	rest(Term, none),
	seen,
	seeing(OldFile),
	write('mode_chk consulted '), write(File), write('.'), nl.

%   rest is given two things: the next term to be processed, and a table
%   of the procedures seen so far in this file.  The table is
%	none				- none read yet
%	read(Functor, Arity, Rest)	- Functor/Arity and the Rest

rest(end_of_file, _) :- !.
rest(Other, Read) :-
	expand_term(Other, Term),
	handle(Term, Read, Seen),
	read(Next), !,
	rest(Next, Seen).

%   handle must cope with six cases:
%	:- public -,.. , - .				{ignore}
%	:- mode   -,.. , - .				{translate & store}
%	:- op     -,.. , - .				{obey}
%	:- reconsult(File).				{recur}
%	:- question.  or ?- question.			{ignore? obey?}
%	assertion.					{translate & assert}

handle(':-'(public(_)),   Read, Read) :- !.
handle(':-'(mode(Modes)), Read, Read) :- !,
	modes(Modes).
handle(':-'(Others),	  Read, Read) :- !,
	others(Others).
handle(':-'(Head, Body),  Read, Seen) :- !,
	define(Head, Tete,Read, Seen),
	assertz(( Tete :- Body )).
handle(Head,       	  Read, Seen) :- !,
	define(Head, Tete, Read, Seen),
	assertz(( Tete :- true )).

%   define(Head, Tete, Read, Seen)  checks whether the goal Head defines
%   some procedure whose mode is to be checked.  If it is, then a stub
%   has already been generated, and the functor is to be renamed, producing
%   a new goal Tete.  If the new goal is the first of its sort in this file
%   then any existing definitions of it should be abolished.

define(Head, Tete, Read, Seen) :-
	functor(Head, OldFunc, Arity),
	change(OldFunc, Arity, NewFunc),
	passed(Read, NewFunc, Arity, Seen),
	Head =.. [OldFunc|Args],
	Tete =.. [NewFunc|Args].

change(OldFunc, Arity, NewFunc) :-
		'med$mode'(OldFunc, Arity, NewFunc, _), !.
change(OldFunc, _, OldFunc).

%   passed(Read, Functor, Arity, Seen) checks whether Functor/Arity is in
%   Read, in which case Seen=Read {nothing new}, or whether it is not, in
%   which case it is added to Read to form Seen, and any previous version
%   of the procedure is abolished.

passed(Read, Functor, Arity, Read) :-
	passed(Read, Functor, Arity), !.
passed(Read, Functor, Arity, read(Functor, Arity, Read)) :-
	abolish(Functor, Arity).

passed(read(Functor, Arity, _), Functor, Arity) :- !.
passed(read(_,       _,     Read), Functor, Arity) :- !,
		passed(Read, Functor, Arity).

'med$check'(Template, Call) :-
	Template =.. [Functor|ArgModes],
	Call	 =.. [_|Actuals],
	(   check_args(ArgModes, Actuals)
	;   write('! Mode error: '), write(Template),
	    write(' called by'), nl,
	    Term =.. [Functor|Actuals], write(Term), nl,
	    break
	),  !,
	call(Call).

	check_args([+|Rest], [A|More]) :- !,
		nonvar(A), !,
		check_args(Rest, More).
	check_args([-|Rest], [A|More]) :- !,
		var(A),    !,
		check_args(Rest, More).
	check_args([?|Rest], [_|More]) :- !,
		check_args(Rest, More).
	check_args([],	     []      ).

%   sick(Functor, Arity) asserts that Functor/Arity is to be checked.
%   It generates a stub, e.g.  :- mode dick(+,-)
%   =>	dick(A,B) :- med$check(dick(+,-), med$dick(A,B)).

sick(Functor, Arity) :-
	atom(Functor), integer(Arity), Arity >= 0,
	'med$mode'(Functor, Arity, NewFunc, Template), !,
	abolish(Functor, Arity),  %  remove old stub or code
	genterms(Functor, NewFunc, Arity, [], Head, Call),
	assert((  Head :- 'med$check'(Template, Call)  )),
	!.
sick(Functor, Arity) :-
	write('! MEDIC hasn''t been consulted about '),
	write(Functor/Arity), nl,
	!.

%   well(Functor, Arity)  asserts that Functor/Arity is no longer to be
%   checked.  So it changes the stub to a direct call, e.g.
%	dick(A,B) :- med$dick(A,B).
%   This is easily changed back by "sick".

well(Functor, Arity) :-
	atom(Functor), integer(Arity), Arity >= 0,
	'med$mode'(Functor, Arity, NewFunc, _), !,
	abolish(Functor, Arity),  %  remove old stub
	genterms(Functor, NewFunc, Arity, [], Head, Call),
	assert((  Head :- Call  )),
	!.
well(Functor, Arity) :-
	write('! MEDIC hasn''t been consulted about '),
	write(Functor/Arity), nl,
	!.

%   genterms(F1, F2, N, [], T1, T2)
%   binds T1 to F1(A,...,Z) and T2 to F2(A,...,Z).

genterms(F1, F2, 0, Args, T1, T2) :- !,
	T1 =.. [F1|Args],
	T2 =.. [F2|Args].
genterms(F1, F2, N, Args, T1, T2) :-
	M is N-1, !,
	genterms(F1, F2, M, [_|Args], T1, T2).

%   modes(Modes) is given a comma-list of mode-declarations, which I call
%   "templates" here.  For each template, it checks that the new template
%   doesn't conflict with a previous template for the same procedure.  In
%   any case it creates an entry in the table med$mode and then says that
%   the procedure is "sick".  E.g. given :- mode dick(+,-) it stores
%	med$mode(dick, 2, med$dick, dick(+,-)).
%   and creates the stub
%	dick(A, B) :- med$check(dick(+,-), med$dick(A,B)).
%   MEDIC is free to create any new name in place of med$dick; only this
%   section of the package knows what that name is.  And only "sick/well"
%   know how the run-time checking is done.

modes(','(A,B)) :- !,
	modes(A),
	modes(B).
modes(Template) :-
	functor(Template, Functor, Arity),
	(   retract('med$mode'(Functor, Arity, NewFunc, OldTemp)),
		compare(OldTemp, Template)
	;   genname(Functor, NewFunc)
	),
	assert('med$mode'(Functor, Arity, NewFunc, Template)), !,
	sick(Functor, Arity).

%   compare(Old_template, New_template) checks that the new description
%   doesn't conflict with the old.  At the moment this is a simple = test,
%   but some more complex test might be justifiable.  Might.

compare(Same, Same) :- !.
compare(Old,  New ) :-
	write('! New mode declaration '), write(New),
	write(' conflicts with '), write(Old), nl,
	write('  New declaration accepted.'), nl.

genname(OldAtom, NewAtom) :-
	name(OldAtom, OldName),
	append("med$", OldName, NewName),
	name(NewAtom, NewName).

%   others handles miscellaneous things like "op", "reconsult".
%   perhaps other commands should be obeyed too?

others(','(A,B)) :- !,
	others(A),  !,
	others(B).
others(op(A,B,C)) :- !,
	op(A,B,C).
others(reconsult(A)) :- !,
	mode_chk(A).
others([-A]) :- !,
	mode_chk(A).
others(_).			%   ignore them.

