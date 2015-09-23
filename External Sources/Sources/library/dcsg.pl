%   File   : DCSG.PL
%   Author : R.A.O'Keefe
%   Updated: 3 December 1983
%   Purpose: Preprocessor for Definite Clause Slash Grammars
%   Needs  : append/3, member/2, memberchk/2.

/*  Definite Clause Slash Gammars are a special kind of DCG which
    allows slash categories, e.g. s/np.
    A nonterminal with N arguments is translated into a predicate
    with N+4 arguments, 
	nt(X1,...,Xn) => nt(X1,...,Xn,T0,T,S0,S)
    where T0 is the "trace" before and T that after parsing an nt,
    and S0 is the string before and S that after parsing an nt.
    The translator produces a clause

	non_terminal(nt(X1,...,Xn), T0, T, S0, S) :-
		nt(X1,...,Xn, T0, T, S0, S).

    for each non-terminal.  This was meant for variables standing
    in non-terminal positions.  You can't write

	s --> Alpha, s/Alpha.

    but you can write

	s -->
		Alpha^[np,pp],
		s/Alpha.

    which turns into

	s(T0, T, S0, S) :-
		member(Alpha, [np, pp]),
		non_terminal(Alpha, T0, T, S0, S1),
		s(Alpha, 0, S1, S).

    Most of the complexity in this translator comes from the decision
    to allow schematic rules, where a nonterminal or an extraposed item
    can be a variable, followed by ^[list of possibilities].  I am not
    sure that it was worth while, as I don't actually have any use for
    this case.

    Note: you can block extraposition out of a particular place by using
    /0.  That is, if nonterm/0 appears in a rule, nothing can be
    extraposed out of nonterm.  You can block a particular kind of movement
    by writing e.g.
	vp_obj/Alpha --> vp/Alpha, {Alpha\=np(subj,_,_)}.
    which will let anything out that vp will, except a subject np, and
    then you can call vp_obj instead of vp in appropriate places.

    If you haven't heard of Gazdar, you probably won't know what
    slash categories are for.  Generalised Phrase Structure Grammar
    augments context-free grammars with
	features	(function-free DCG arguments)
	slash categories(as here)
	rule schemas	(as done badly here)
	meta-rules	(sort of like transformations, but not really)
    in such a way that only the notational convenience is increased,
    the grammars still have only context-free power.  As I understand
    it, Gazdar doesn't claim that English *is* a context-free language,
    only that the arguments to date that it is *not* are about as sound
    as a 3-dollar note.  I have to draw a distinction between DCSGs
    (which are just logic programs, and are at least as powerful as
    GPSGs) and the particular parser obtained by using Prolog as the
    parser.  When we use Prolog to parse DCSGs, we run into problems
    with left-recursion and so on that have nothing to do with the
    formalism.  Henry Thompson, DAI Edinburgh, has a full implementation
    of GPSGs in Lisp, including meta-rules, which is a chart parser.
    This file is intended as a cheap way of exploring some of the ideas
    of GPSG, no more.  A serious tool for people who can't hack Prolog
    would be very much bigger.
*/


:- public
	dcsg/1,
	dcsg_phrase/2.

:- mode
	dcsg(+),			%   Files ->
	dcsg_load(+),			%   Files ->
	dcsg_hack(+),			%   Term ->
	dcsg_head(+, +, -),		%   NT x Rhs -> Clause
	dcsg_note(+, +),		%   {clause|dcsg} x Goal ->
	dcsg_args(+, ?, ?, ?, ?, -),
	dcsg_choices(?, +, -),		%   Var x NT-list -> Goal-list
	dcsg_head(+, -, +, -, ?, ?),
	dcsg_body(+, ?, ?, ?, ?, -),
	dcsg_and(+, +, -),
	dcsg_or(+, ?, ?, ?, ?, -),
	dcsg_list(+, ?, ?, -),
	dcsg_choices(+, -),
	dcsg_choice(+, -),
	non_terminal(+, ?, ?, ?, ?, +),
	dcsg_phrase(+, +).


%   dscg(Files)
%   consults a list of Files which are expected to contain grammar
%   rules.  It first of all wipes out all the existing grammar rules,
%   so acts as a sort of reconsult.  I haven't really made up my
%   mind about that.  In any case, I've only the one file to load.

dcsg(_) :-
	clause(non_terminal(_,_,_,_,_), NT),
	functor(NT, NonTerm, Arity),
	abolish(NonTerm, Arity),
	fail.
dcsg(_) :-
	non_dcsg(Functor, Arity),
	abolish(Functor, Arity),
	fail.
dcsg(Files) :-
	abolish(non_dcsg, 2),
	abolish(non_terminal, 5),
	dcsg_load(Files).


dcsg_load([]) :- !.
dcsg_load([File|Files]) :- !,
	dcsg_load(File),
	dcsg_load(Files).
dcsg_load(File) :-
	atom(File),
	nofileerrors,
	see(File),
	fileerrors,
	repeat,
	    read(Term),
	    dcsg_hack(Term),
	    Term = end_of_file,
	!,
	seeing(FullName),
	seen,
	write(FullName), write(' loaded.'), nl.
dcsg_load(File) :-
	write('! can''t load '), write(File), nl.



%   dcsg_hack(Term)
%   processes each term as it is read.  Grammar rules are checked a bit,
%   but ordinary clauses (basically dictionary information) are not, though
%   it does check that a predicate doesn't have grammar rules and ordinary
%   clauses both.

dcsg_hack(end_of_file) :- !.
dcsg_hack((:-Command)) :- !,
	call(Command).			%  operator declarations
dcsg_hack((Head-->Body)) :- !,
	(   dcsg_head(Head, Body, Clause),
	    assertz(Clause)
	;   X=(Head-->Body), numbervars(X,0,_),
	    print(X), nl
	), !.
dcsg_hack((Head:-Body)) :- !,
	dcsg_note(clause, Head),
	assertz((Head :- Body)).
dcsg_hack(Head) :-
	dcsg_note(clause, Head),
	assertz(Head).



%   dcsg_note(Type, Head) 
%   is responsible for checking that a predicate doesn't have rules
%   and clauses both.  The main point of the dcsg_note predicate is
%   really to keep track of nonterminals so that variable nonterminals
%   and dcsg_phrase can work easily.

dcsg_note(clause, Head) :- !,
	nonvar(Head),
	functor(Head, Functor, Arity),
	functor(Term, Functor, Arity),
	(   non_dcsg(Functor, Arity)
	;   assert(non_dcsg(Functor, Arity)),
	    (   clause(non_terminal(Term, _, _, _, _), _),
		write('! Rules and clauses for '),
		write(Functor/Arity), nl
	    ;   true
	    )
	), !.
dcsg_note(dcsg, Head/Missing) :- !,
	dcsg_note(dcsg, Head).
dcsg_note(dcsg, Head) :-
	functor(Head, NonTerm, Arity),
	functor(Term, NonTerm, Arity),
	dcsg_args(Term, T0, T, S0, S, Goal),
	(   clause(non_terminal(Term, T0, T, S0, S), Goal)
	;   assertz(( non_terminal(Term,T0,T,S0,S) :- Goal )),
	    (   non_dcsg(NonTerm, Arity),
		write('! Rules and clauses for '),
		write(NonTerm/Arity), nl
	    ;   true
	    ),
	    T0 = Term, T = 0, S0 = S,
	    assertz(( Goal ))
	), !.



%   dcsg_args(NonTerm, T0, T, S0, S, Goal)
%   adds the extra arguments to the nonterminal to form a proper
%   Prolog goal.  Variables and slash categories are handled elsewhere.

dcsg_args(NonT, T0, T, S0, S, Goal) :-
	NonT =.. [NT|Args],
	append(Args, [T0, T, S0, S], Full),
	Goal =.. [NT|Full], !.


%   dcsg_head(NonTerm, Rhs, Clause),
%   processes the head of a grammar rule, which takes the form
%   <nonterminal> [/ <extraposition> [^ <restriction>]] .
%   The non-terminal itself may not be a variable, but the other
%   bit may be.  It then hands the Rhs on to dcsg_body to process.

dcsg_head(Dud, _, _) :-
	(   var(Dud)
	;   Dud = Var/_, var(Var)
	;   Dud = Var^_
	;   Dud = Var^_/_
	),
	!,
	write('! Rule head may not be a variable.'), nl, fail.
dcsg_head(NonT/Missing^Restriction, Rhs,
		(Head:-member(Missing,Choices),Body)) :-
	nonvar(Restriction), !,
	dcsg_choices(Missing, Restriction, Choices),
	dcsg_head(NonT, Head, Rhs, Body, Missing, 0).
dcsg_head(NonT/Missing, Rhs, (Head:-Body)) :- !,
	dcsg_head(NonT, Head, Rhs, Body, Missing, 0).
dcsg_head(NonT, Rhs, (Head:-Body)) :-
	dcsg_head(NonT, Head, Rhs, Body, _, _).


dcsg_head(NonT, Head, Rhs, Body, T0, T) :-
	dcsg_note(dcsg, NonT),
	dcsg_args(NonT, T0, T, S0, S, Head),
	dcsg_body(Rhs,  T0, T, S0, S, Body).



%   dcsg_body(RHS, <Context>, Translation)
%   translates the RHS of a DCSG grammar rule into Prolog.
%   The method is all but identical to that for DCGs or XGs.

dcsg_body(Var, T0, T, S0, S, non_terminal(Var,T0,T,S0,S,0)) :-
	var(Var),
	!.
dcsg_body(Var^Poss, T0, T, S0, S, non_terminal(Var,T0,T,S0,S,Choices)) :- !,
	dcsg_choices(Var, Poss, Choices).
dcsg_body((A,B), T0, T, S0, S, Trans) :- !,
	dcsg_body(A, T0, T1, S0, S1, A1),
	dcsg_body(B, T1, T,  S1, S,  B1),
	dcsg_and(A1, B1, Trans).
dcsg_body((A;B), T0, T, S0, S, (A1;B1)) :- !,
	dcsg_or(A, T0, T, S0, S, A1),
	dcsg_or(B, T0, T, S0, S, B1).
dcsg_body(!, T, T, S, S, !) :- !.
dcsg_body({Goals}, T, T, S, S, Goals) :- !.
dcsg_body([], T, T, S, S, true) :- !.
dcsg_body([Term|Terms], T, T, S0, S, Trans) :-
	dcsg_list([Term|Terms], S0, S, Trans).
dcsg_body(NonTerm/Missing^Restriction, T, T, S0, S,
		(Goal,member(Restriction,Choices))) :-
	nonvar(Missing), !,
	dcsg_choices(Missing, Restriction, Choices),
	dcsg_args(NonTerm, Missing, 0, S0, S, Goal).
dcsg_body(NonTerm/Missing, T, T, S0, S, Goal) :- !,
	dcsg_args(NonTerm, Missing, 0, S0, S, Goal).
dcsg_body(NonTerm, T0, T, S0, S, Goal) :-
	dcsg_args(NonTerm, T0, T, S0, S, Goal).


dcsg_list([Term], S0, S, 'C'(S0, Term, S)) :- !.
dcsg_list([Term|Terms], S0, S, ('C'(S0,Term,S1),Trans)) :-
	dcsg_list(Terms, S1, S, Trans).


dcsg_or(A, T0, T, S0, S, A3) :-
	dcsg_body(A, T1, T, S1, S, A1),
	(   S \== S1, !, S0 = S1, A2 = A1 ; dcsg_and(S0=S1, A1, A2)   ),
	(   T \== T1, !, T0 = T1, A3 = A2 ; dcsg_and(T0=T1, A2, A3)   ).


dcsg_and(true, X, X) :- !.
dcsg_and(X, true, X) :- !.
dcsg_and((X,Y), Z, (X,W)) :- !,
	dcsg_and(Y, Z, W).
dcsg_and(X, Y, (X,Y)).



%   dcsg_choices([list of nt or nt/arity or goal], [list of goal])
%   turns e.g. [a, b/3, c(X,Y), d(X), e/1] into e.g.
%   [a, b(_,_,_), c(X,Y), d(X), e(_)].  These lists are used to
%   restrict the range of nonterminal variables.
%   dcsg_choices(Var, Restriction, Choices)
%   does the translation, after first checking that the restricted thing
%   is in fact a Prolog variable.  Note that the "existential quantifier"
%   meaning of "^" is still available inside {escape-to-Prolog} braces.


dcsg_choices(Var, Restriction, Choices) :-
	var(Var), !,
	dcsg_choices(Restriction, Choices).
dcsg_choices(_, _, _) :-
	write('! Restriction on non-variable.'), nl, fail.


dcsg_choices([], []) :- !.
dcsg_choices([Head|Tail], [This|Rest]) :-
	dcsg_choice(Head, This),
	dcsg_choices(Tail, Rest).

dcsg_choice(Atom, Atom) :-
	atom(Atom), !.
dcsg_choice(Functor/Arity, Term) :- !,
	atom(Functor), integer(Arity), Arity >= 0,
	functor(Term, Functor, Arity).
dcsg_choice(Term, Term) :-
	nonvar(Term),
	functor(Term, _, Arity),
	Arity > 0.		%  rule out numbers



%   Variables appearing at the top level of a grammar rule are
%   turned into calls on nonterminal/6.
%	Var	=> non_terminal(Var, T0,T, S0,S, 0).
%	Var/Poss=> non_terminal(Var, T0,T, S0,S, Choices).
%   non_terminal/6 checks or ensures that the variable is suitably
%   bound, and then dispatches through non_terminal/5.  Note that
%   Var/Poss may backtrack through different possibilities.

non_terminal(Var, T0, T, S0, S, 0) :-
	var(Var), !,
	write('! variable non-terminal at run-time'), nl,
	fail.		%  we want a better error signal
non_terminal(Var, T0, T, S0, S, 0) :-
	non_terminal(Var, T0, T, S0, S).
non_terminal(Var, T0, T, S0, S, Choices) :-
	nonvar(Var), !,
	memberchk(Var, Choices),
	non_terminal(Var, T0, T, S0, S).
non_terminal(Var, T0, T, S0, S, Choices) :-
	member(Var, Choices),
	non_terminal(Var, T0, T, S0, S).



%   dcsg_phrase(Nonterminal, Sentence)
%   is the dcsg analogue of the standard predicate phrase/2,
%   can the Sentence be completely parsed as a Nonterminal
%   with nothing missing?

dcsg_phrase(NonTerm, Sentence) :-
	non_terminal(NonTerm, 0, 0, Sentence, []).


/*	For debugging DCSG.PL in bare Prolog:
append([],L,L).
append([H|T],L,[H|R]):-append(T,L,R).
member(X,[X|_]).		memberchk(X,[X|_]):-!.
member(X,[_|T]):-member(X,T).	memberchk(X,[_|T]):-memberchk(X,T).
/**/
/*	For debugging DCSG.PL in ToolKit:	*/
ppg :-
	non_dcsg(F,N),
	pp(F/N),
	fail
    ;	clause(non_terminal(_,_,_,_,_), Goal),
	functor(Goal, F, N),
	pp(F/N),
	fail
    ;   true.






