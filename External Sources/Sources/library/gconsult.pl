/*  File   : gconsult
    Author : R.A.O'Keefe
    Updated: 28 Feb 83
    Purpose: grammar rule pre-processor
*/

/*  COPYRIGHT (C) 1983
    This module is the original work of R.A.O'Keefe done between
    Wednesday February 23 and Friday February 26 1983.  During this
    time I was employed (but not paid) by Systems Designers Ltd,
    but this work was done after 9pm in the evenings and outside
    their premises.  This particular copy was typed on their Systime
    5000, but that was to provide them with a copy for their own use,
    not because I used the machine at any point during the development
    of the code.  I didn't.  Any resemblance between this code and
    other grammar rule preprocessors written in Prolog is due to the
    nature of the problem and the directness of the solution, not to
    my having copied anyone else's code, which I have not done.
*/

/*  This module defines five predicates, one of which the user may extend.
    It contains a several predicates whose names begin with "expand$".
    They are not intended for general use.  I assume that 'append'/3 is
    available as a utility; if not it should be defined as
	append([H|T], L, [H|R]) :- !, append(T, L, R).
	append([], L, L).


    abolish(Functor, Arity)
	removes all the clauses for F/A.  This is built in to the Dec-10
	and C Prolog systems, which use it in place of PDP-11 Prolog's
	'retractall'.  This is used by greconsult, and is given this name
	to made -11 Prolog more like DEC-10/C Prolog.
	NB: the method of preserving spypoints only works in NU7 Prolog.

    gconsult(Files)
	reads one or more Prolog source files, translates the terms read
	using expand_term, and stores the results.  It is analogous to
	consult/1.  The argument may be a single file, a single file
	preceded by a $ sign (use - in DEC-10 Prolog, C Prolog, ZIP, or
	any other Prolog system permitting bivalent operators), or a
	list of such file specifications.

    greconsult(Files)
	reads one or more Prolog source files as gconsult(Files), but is
	like reconsult rather than consult.  That is, consult and gconsult
	just add new clauses after existing ones, while reconsult and this
	greconsult will wipe out any existing clauses for a predicate that
	is defined in the Files it reads.  Use gconsult to load a file in
	the first time, and greconsult to reload it after it has been
	edited.

    expand_term(SourceTerm,Expansion)
	takes a SourceTerm as returned by read/1 and translates it into
	some suitable internal form, the Expansion.  Two cases are
	predefined: the default is to return the SourceTerm unchanged,
	and grammar rules are expanded as described in "Programming in
	Prolog".  If you want to add your own rules, perhaps doing macro
	expansion or unfolding data structure access predicates, you do
	so by 'asserta'ing your clauses.  Your cases must go at the front.

	The expansion may be a list, in which case all the elements of the
	list are added.  Also, the expansion may be a command even when the
	input is not a command.  It will still be executed.

	Grammar rules (for Definite Clause Grammars) take the form
		NT,[Terminals] --> Rhs.

    phrase(GrammarRuleBody, TokenList)
	succeeds when TokenList matches the syntax defined by the
	GrammarRuleBody.  Normally the grammar rule body is a single
	nonterminal (possibly with arguments), so it has the effect
	of GrammarRuleBody(TokenList, []).  This is a convenient way
	of debugging the definitions of nonterminals other than the
	start symbol of your grammar (which you normally have a nice
	specially written interface to).
*/

/*  BEWARE:
    through some oversight, the curly braces are not treated specially
    by the PDP-11 Prolog system.  It should refuse to combine them with
    other characters as it already refuses to combine ()[] with other
    characters.  The DEC-10 and C Prolog systems read {X} as '{}'(X).
    This has nothing to do with grammar rules; several other things use
    the convention that {X} is an excape to Prolog.  A consequence of
    this omission from PDP-11 Prolog is that "}." is read as ONE token;
    always leave a space between the brace and the period.  "}," is safe.
    A modified version of the PDP-11 Prolog "read" module is available
    from me which is more compatible with Dec-10 Prolog.
*/

/*  Operator declarations for grammar rules:
*/
?-
	op(251, fx, ('{')),	/* not needed in DEC-10/C Prolog */
	op(250, xf, ('}')),	/* not needed in DEC-10/C Prolog */
	op(255,xfx, ('-->')).   /* should be same as :- */


/*  Remove all the clauses for the predicate named Functor which
    has Arity arguments.  This is a primitive in other Prologs.
    If the predicate has a spypoint, the spypoint is put back.
    The way this is done only works in PDP-11 Prolog.  Even there
    we can't do pattern matching on the '$', because the atom which
    is actually used has been taken out of the dictionary.
*/
abolish(Functor, Arity) :-
	functor(MostGeneralTerm, Functor, Arity),
	(   clause(MostGeneralTerm, (Foo,!,FooBaz)),
		atom(Foo), FooBaz =.. [Baz,MostGeneralTerm], !,
		retractall(MostGeneralTerm),
		assert((MostGeneralTerm :- (Foo,!,FooBaz) ))
	;   retractall(MostGeneralTerm)
	),  !.


/*  consult or reconsult a file, applying any user- or system-
    defined transformations to the sentences in it.
*/

gconsult(Files) :-		/* generalised 'consult' */
	gconsult(Files, gconsult),
	abolish('expand$done', 2).

greconsult(Files) :-		/* generalised 'reconsult' */
	gconsult(Files, greconsult),
	abolish('expand$done', 2).



gconsult(Var, _) :-
	var(Var), !,
	write('** variable given to gconsult'), nl.
gconsult([File|Files], Flag) :- !,
	gconsult(File,  Flag),
	gconsult(Files, Flag).
gconsult($File, _) :- !,	/* reconsult; use - instead of $ if possible */
	gconsult(File, greconsult).
gconsult([], _) :- !.
gconsult(File, Flag) :-
	seeing(OldFile),
	see(File), !,
	repeat,
		read(Term),
		expand_term(Term, Expansion),
		'expand$assert'(Expansion, Flag),
	!,
	seen,
	see(OldFile),
	write(File), put(32),	/* 32 is " " */
	write(Flag), write('ed'), nl.
gconsult(File, Flag) :-
	write('** can''t '), write(Flag),
	put(32), write(File), nl.


/*  expand$assert is used in a failure-driven loop.
    It is supposed to succeed when it is given the end-of-file token,
    and to fail otherwise.  But before it fails it is supposed to
    perform an action.  This action is to obey a command or to store
    a clause.  When greconsulting, the first time we see any clause
    for a particular predicate, we wipe out any existing clauses.
*/

'expand$assert'((?-end), _) :- !.
'expand$assert'((?-Command), _) :-
	call(Command), !,
	fail.
'expand$assert'((?-Command), _) :-
	write('** can''t obey '),
	write(Command), nl, !,
	fail.
'expand$assert'([Head|Tail], Flag) :- !,
	(   'expand$assert'(Head, Flag)
	;   'expand$assert'(Tail, Flag)
	).
'expand$assert'([], _) :- !,
	fail.					/* failure-driven wretch */
'expand$assert'(Clause, greconsult) :-
	'expand$assert'(Clause).		/* which always fails */
'expand$assert'(Clause, _) :-
	assertz(Clause),
	fail.


/*  expand$assert(Clause) wipes out any clauses for this predicate
    unless that has already been done.  Whatever happens, it fails.
*/
'expand$assert'((Head:-Body)) :- !,
	'expand$assert'(Head).
'expand$assert'(Head) :-
	functor(Head, Functor, Arity),
	not('expand$done'(Functor,Arity)),
	abolish(Functor, Arity),
	assertz('expand$done'(Functor,Arity)),
	fail.


/*  expand_term(Given, Transformed) has two cases.
    The first handles grammar rules.
    The second is the identity mapping.
    Feel free to add new rules before these two cases.
    The cut in the second clause is there in case you add
    rewrite rules of your own.  You aren't supposed to add
    them after the second clause, and if you do, the cut
    means that they will be ignored.
*/
expand_term((Head-->Body), (Head1:-Body1)) :- !,
	'expand$head'(Head, S0, S, Head1),
	'expand$body'(Body, S0, S, Body1).
expand_term(Term, Term) :- !.


/*  expand the head of a grammar rule.  This is almost always a
    single goal (non-terminal with arguments) like an ordinary
    Prolog rule, and we have only to add the difference list
    arguments.  But DCGs also let you push new terminals back on
    the output list.
*/
'expand$head'((NonTerm,PushBack), S0, S, Head1) :- !,
	nonvar(PushBack),
	append(PushBack, S, S1),
	'expand$goal'(NonTerm, S0, S1, Head1).
'expand$head'(NonTerm, S0, S, Head1) :-
	'expand$goal'(NonTerm, S0, S,  Head1).


/*  expand a single goal so that it includes the extra arguments.
    The extra arguments form a difference list, which is the portion of
    the input list to be matched by this goal.
*/
'expand$goal'(Goal, Before, After, NewGoal) :-
	Goal =.. GoalList,
	append(GoalList, [Before,After], NewGoalList), !,
	NewGoal =.. NewGoalList.


/*  expand the body of a grammar rule.  The things we have to watch
    out for are the difference lists given to branches of an 'or',
    and trying to keep the 'ands's flat.  We assume that the reader
    returned nice flat 'and's where it could.
    An innovation in this grammar rule processor is that variable
    goals are allowed in grammar rules as well as in orinary rules.
    The effect is to call the given goal with the extra arguments.
    The P arguments are input Phrases, the G arguments are output Goals,
    and the S arguments are list Segments.

    NB:  DEC-10 and C Prolog use a different expansion for terminals;
	expand$body(Terminals, S0, S, Goal) :-
		expand$term(Terminals, S0, S, Goal).
	expand$term([T], S0, S, c(T, S0, S)) :- !.
	expand$term([H|T], S0, S, (c(H,S0,S1),C)) :- !,
		expand$term(T,S1,S,C).
	c(H, [H|T], T).
    That expansion gets rid of a problem with cuts.  In this version,
    terminals appearing directly after a cut are effectively promoted to
    before the cut.  This version has the advantage that terminals at the
    beginning of a rule are promoted into the head.  The version using
    'C' doesn't promote terminals into the head, but neither does it move
    them past a cut.  I am quite happy with this version, as I never have
    terminals after a cut (except in a disjunction where the problem goes
    away anyway).  If you have a grammar where the cuts don't seem to be
    working, check for this problem, and use the alternative expansion.
*/
'expand$body'(NonTerm, S0, S, ('expand$body'(NonTerm,S0,S,Goal),call(Goal))) :-
	var(NonTerm), !.
'expand$body'((P1,P2), S0, S, G) :- !,
	'expand$body'(P1, S0, S1, G1),
	'expand$body'(P2, S1, S,  G2),
	'expand$and'(G1, G2, G).
'expand$body'((P1;P2), S0, S, (G1;G2)) :- !,
	'expand$or'(P1, S0, S, G1),
	'expand$or'(P2, S0, S, G2).
'expand$body'((!), S, S, (!)) :- !.
'expand$body'({Goal}, S, S, Goal) :- !.
'expand$body'(Terminals, S0, S, true) :-
	append(Terminals, S, S0), !.	/* checks that Terminals is a list */
'expand$body'(NonTerm, S0, S, Goal) :-
	'expand$goal'(NonTerm, S0, S, Goal).


/*  given two flat conjunctions, either or both of which may be 'true',
    return a new flat conjunction.  One reason for bothering about this
    is so that {Filters} which are conjunctions will be flattened.  Another
    is to get rid of some of the 'true's introduced in place of terminal
    lists.  The alternative terminal expansion using 'C' instead of load-time
    appending can also generate conjunctions that need flattening.
*/
'expand$and'((A,B), C, (A,D)) :- !,
	'expand$and'(B, C, D).
'expand$and'(true, A, A) :- !.
'expand$and'(A, true, A) :- !.
'expand$and'(A, B, (A,B)).

/*  when a rule has embedded disjunctions, we have to be careful not to
    promote terminal tests out of one disjunct into the other.  If the
    alternative expansion using 'C' is used, the need for this special
    check disappears, and 'expand$body' can be called instead.  We don't
    need to bother about flattening disjunctions because the grammar rule
    preprocessor never introduces any of its own, and we can assume that
    the read/1 predicate returns disjunctions in a suitable form.
*/
'expand$or'(P, S0, S, G) :-
	'expand$body'(P, S1, S, G1), !,
	'expand$or'(S1, S, S0, G1, G).

'expand$or'(S0, S, S0, G, G) :-
	var(S0), S0 \== S, !.  		/* no extra tests will be promoted */
'expand$or'(S1, S, S0, G, (S0=S1,G)).	/* block promotion at load time */


/*  test whether a list is matched by a non-terminal.
    By analogy with call/1, the "non-terminal" may actually be
    the body of a grammar rule, not just a non-terminal goal.
    This is mainly intended as a debugging and development aid.
*/
phrase(Rule, List) :-
	'expand$body'(Rule, Dummy, [], Goal), !,
	Dummy = List, /* Dummy might start with terminals */
	call(Goal).

