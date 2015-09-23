%   File   : WRITE.PL
%   Author : Richard A. O'Keefe.
%   Updated: 22 October 1984
%   Purpose: Portable definition of write/1 and friends.

:- public
	portable_display/1,
	portable_listing/0,
	portable_listing/1,
	portable_print/1,
	portable_write/1,
	portable_writeq/1,
	portray_clause/1.
	
:- mode
	classify_name(+, -),
	classify_alpha_tail(+),
	classify_other_tail(+),
	'functor spec'(+, -, -, -),
	'list clauses'(+, +, +, +),
	'list magic'(+, +),
	'list magic'(+, +, +),
	'list magic'(+, +, +, +),
	maybe_paren(+, +, +, +, -),
	maybe_space(+, +),
	portray_clause(+),
	put_string(+),
	put_string(+, +),
	write_args(+, +, +, +, +),
	write_atom(+, +, +, -),
	write_oper(+, +, +, +, -),
	write_out(+, +, +, +, -),
	write_out(+, +, +, +, +, +, -),
	write_tail(+, +),
	write_VAR(+, +, +, -),
	write_variable(?).

/*  WARNING!
    This file was written to assist portability and to help people
    get a decent set of output routines off the ground fast.  It is
    not particularly efficient.  Information about atom names and
    properties should be precomputed and fetched as directly as
    possible, and strings should not be created as lists!

    The four output routines differ in the following respects:
    [a] display doesn't use operator information or handle {X} or
	[H|T] specially.  The others do.
    [b] print calls portray/1 to give the user a chance to do
	something different.  The others don't.
    [c] writeq puts quotes around atoms that can't be read back.
	The others don't.
    Since they have such a lot in common, we just pass around a
    single Style argument to say what to do.

    In a Prolog which supports strings;
	write(<string>) should just write the text of the string, this so
	that write("Beware bandersnatch") can be used.  The other output
	commands should quote the string.

    listing(Preds) is supposed to write the predicates out so that they
    can be read back in exactly as they are now, provided the operator
    declarations haven't changed.  So it has to use writeq.  $VAR(X)
    will write the atom X without quotes, this so that you can write
    out a clause in a readable way by binding each input variable to
    its name.
*/


portable_display(Term) :-
	write_out(Term, display, 1200, punct, _).


portable_print(Term) :-
	write_out(Term, print, 1200, punct, _).


portable_write(Term) :-
	write_out(Term, write, 1200, punct, _).


portable_writeq(Term) :-
	write_out(Term, writeq, 1200, punct, _).



%   maybe_paren(P, Prio, Char, Ci, Co)
%   writes a parenthesis if the context demands it.

maybe_paren(P, Prio, Char, _, punct) :-
	P > Prio,
	!,
	put(Char).
maybe_paren(_, _, _, C, C).



%   maybe_space(LeftContext, TypeOfToken)
%   generates spaces as needed to ensure that two successive
%   tokens won't run into each other.

maybe_space(punct, _) :- !.
maybe_space(X, X) :- !,
	put(32).
maybe_space(quote, alpha) :- !,
	put(32).
maybe_space(_, _).



%   put_string(S)
%   writes a list of character codes.

put_string([]).
put_string([H|T]) :-
	put(H),
	put_string(T).


%   put_string(S, Q)
%   writes a quoted list of character codes, where the first
%   quote has already been written.  Instances of Q in S are doubled.

put_string([], Q) :-
	put(Q).
put_string([Q|T], Q) :- !,
	put(Q), put(Q),
	put_string(T, Q).
put_string([H|T], Q) :-
	put(H),
	put_string(T, Q).



%   write_variable(V)
%   is system dependent.  This just uses whatever Prolog supplies.

write_variable(V) :-
	write(V).



%   write_out(Term, Style, Priority, Ci, Co)
%   writes out a Term in a given Style (display,write,writeq,print)
%   in a context of priority Priority (that is, operators with
%   greater priority have to be quoted), where the last token to be
%   written was of type Ci, and reports that the last token it wrote
%   was of type Co.

write_out(Term, _, _, Ci, alpha) :-
	var(Term),
	!,
	maybe_space(Ci, alpha),
	write_variable(Term).
write_out('$VAR'(N), Style, _, Ci, Co) :- !,
	write_VAR(N, Style, Ci, Co).
write_out(N, _, _, Ci, alpha) :-
	integer(N),
	(   N < 0, maybe_space(Ci, other)
	;   maybe_space(Ci, alpha)
	),  !,
	name(N, String),
	put_string(String).
write_out(Term, print, _, Ci, alpha) :-
	portray(Term),
	!.
write_out(Atom, Style, Prio, _, punct) :-
	atom(Atom),
	current_op(P, _, Atom),
	P > Prio,
	!,
	put(40),
	(   Style = writeq, write_atom(Atom, Style, punct, _)
	;   name(Atom, String), put_string(String)
	),  !,
	put(41).
write_out(Atom, Style, _, Ci, Co) :-
	atom(Atom),
	!,
	write_atom(Atom, Style, Ci, Co).
write_out(Term, display, _, Ci, punct) :- !,
	functor(Term, Fsymbol, Arity),
	write_atom(Fsymbol, display, Ci, _),
	write_args(0, Arity, Term, 40, display).
write_out({Term}, Style, _, _, punct) :- !,
	put(123),
	write_out(Term, Style, 1200, punct, _),
	put(125).
write_out([Head|Tail], Style, _, _, punct) :- !,
	put(91),
	write_out(Head, Style, 999, punct, _),
	write_tail(Tail, Style).
write_out((A,B), Style, Prio, Ci, Co) :- !,
	%  This clause stops writeq quoting commas.
	maybe_paren(1000, Prio, 40, Ci, C1),
	write_out(A, Style, 999, C1, _),
	put(44),
	write_out(B, Style, 1000, punct, C2),
	maybe_paren(1000, Prio, 41, C2, Co).
write_out(Term, Style, Prio, Ci, Co) :-
	functor(Term, F, N),
	write_out(N, F, Term, Style, Prio, Ci, Co).


write_out(1, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, fx, F), P is O-1
	;   current_op(O, fy, F), P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	write_atom(F, Style, C1, C2),
	arg(1, Term, A),
	write_out(A, Style, P, C2, C3),
	maybe_paren(O, Prio, 41, C3, Co).
write_out(1, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, xf, F), P is O-1
	;   current_op(O, yf, F), P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	arg(1, Term, A),
	write_out(A, Style, P, C1, C2),
	write_atom(F, Style, C2, C3),
	maybe_paren(O, Prio, 41, C3, Co).
write_out(2, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, xfy, F), P is O-1, Q = O
	;   current_op(O, xfx, F), P is O-1, Q = P
	;   current_op(O, yfx, F), Q is O-1, P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	arg(1, Term, A),
	write_out(A, Style, P, C1, C2),
	write_oper(F, O, Style, C2, C3),
	arg(2, Term, B),
	write_out(B, Style, Q, C3, C4),
	maybe_paren(O, Prio, 41, C4, Co).
write_out(N, F, Term, Style, Prio, Ci, punct) :-
	write_atom(F, Style, Ci, _),
	write_args(0, N, Term, 40, Style).


write_oper(Op, Prio, Style, Ci, Co) :-
	Prio < 700, !,
	write_atom(Op, Style, Ci, Co).
write_oper(Op, _, Style, Ci, punct) :-
	put(32),
	write_atom(Op, Style, punct, _),
	put(32).


write_VAR(N, Style, Ci, alpha) :-
	integer(N), N >= 0, !,
	maybe_space(Ci, alpha),
	Letter is N mod 26 + 65,
	put(Letter),
	(   N < 26
	;   Rest is N/26, name(Rest, String),
	    put_string(String)
	), !.
write_VAR(A, Style, Ci, Co) :-
	atom(A), !,
	write_atom(A, write, Ci, Co).
write_VAR(X, Style, Ci, punct) :-
	write_atom('$VAR', Style, Ci, _),
	write_args(0, 1, '$VAR'(X), 40, Style).


write_atom(('!'), _, _, punct) :- !,
	put(33).
write_atom((';'), _, _, punct) :- !,
	put(59).
write_atom([], _, _, punct) :- !,
	put(91), put(93).
write_atom({}, _, _, punct) :- !,
	put(123), put(125).
write_atom(Atom, Style, Ci, Co) :-
	name(Atom, String),
	(   classify_name(String, Co),
	    maybe_space(Ci, Co),
	    put_string(String)
	;   Style = writeq, Co = quote,
	    maybe_space(Ci, Co),
	    put(39), put_string(String, 39)
	;   Co = alpha,
	    put_string(String)
	),  !.

%   classify_name(String, Co)
%   says whether a String is an alphabetic identifier starting
%   with a lower case letter (Co=alpha) or a string of symbol characters
%   like ++/=? (Co=other).  If it is neither of these, it fails.  That
%   means that the name needs quoting.  The special atoms ! ; [] {} are
%   handled directly in write_atom.  In a basic Prolog system with no
%   way of changing the character classes this information can be
%   calculated when an atom is created, and just looked up.  This has to
%   be as fast as you can make it.

classify_name([H|T], alpha) :-
	H >= 97, H =< 122,
	!,
	classify_alpha_tail(T).
classify_name([H|T], other) :-
	memberchk(H, "#$&=-~^\`@+*:<>./?"),
	classify_other_tail(T).

classify_alpha_tail([]).
classify_alpha_tail([H|T]) :-
	(  H >= 97, H =< 122
	;  H >= 65, H =< 90
	;  H >= 48, H =< 57
	;  H =:= 95
	), !,
	classify_alpha_tail(T).

classify_other_tail([]).
classify_other_tail([H|T]) :-
	memberchk(H, "#$&=-~^\`@+*:<>./?"),
	classify_other_tail(T).



%   write_args(DoneSoFar, Arity, Term, Separator, Style)
%   writes the remaining arguments of a Term with Arity arguments
%   all told in Style, given that DoneSoFar have already been written.
%   Separator is 0'( initially and later 0', .

write_args(N, N, _, _, _) :- !,
	put(41).
write_args(I, N, Term, C, Style) :-
	put(C),
	J is I+1,
	arg(J, Term, A),
	write_out(A, Style, 999, punct, _),
	write_args(J, N, Term, 44, Style).



%   write_tail(Tail, Style)
%   writes the tail of a list of a given style.

write_tail(Var, _) :-			%  |var]
	var(Var),
	!,
	put(124),
	write_variable(Var),
	put(93).
write_tail([], _) :- !,			%  ]
	put(93).
write_tail([Head|Tail], Style) :- !,	%  ,Head tail
	put(44),
	write_out(Head, Style, 999, punct, _),
	write_tail(Tail, Style).
write_tail(Other, Style) :-		%  |junk]
	put(124),
	write_out(Other, Style, 999, punct, _),
	put(93).


/*  The listing/0 and listing/1 commands are based on the Dec-10
    commands, but the format they generate is based on the "pp" command.
    The idea of portray_clause/1 came from PDP-11 Prolog.

    BUG: the arguments of goals are not separated by comma-space but by
    just comma.  This should be fixed, but I haven't the time right not.
    Run the output through COMMA.EM if you really care.

    An irritating fact is that we can't guess reliably which clauses
    were grammar rules, so we can't print them out in grammar rule form.

    We need a proper pretty-printer that takes the line width into
    acount, but it really isn't all that feasible in Dec-10 Prolog.
    Perhaps we could use some ideas from NIL?
*/

portable_listing :-
	current_predicate(_, Pred),
	nl,
	clause(Pred, Body),
	portray_clause((Pred:-Body)),
	fail.
portable_listing.


%   listing(PredSpecs)

%   Takes a predicate specifier F/N, a partial specifier F, or a
%   list of such things, and lists each current_predicate Pred
%   matching one of these specifications.

portable_listing(V) :-
	var(V), !.       % ignore variables
portable_listing([]) :- !.
portable_listing([X|Rest]) :- !,
	portable_listing(X),
	portable_listing(Rest).
portable_listing(X) :-
	'functor spec'(X, Name, Low, High),
	current_predicate(Name, Pred),
	functor(Pred, _, N),
	N >= Low, N =< High,
	nl, 
	clause(Pred, Body),
	portray_clause((Pred:-Body)),
	fail.
portable_listing(_).


'functor spec'(Name/Low-High, Name, Low, High) :- !.
'functor spec'(Name/Arity, Name, Arity, Arity) :- !.
'functor spec'(Name, Name, 0, 255).


portray_clause(:-(Command)) :-
	(   Command = public(Body), Key = (public)
	;   Command = mode(Body),   Key = (mode)
	;   Command = type(Body),   Key = (type)
	;   Command = pred(Body),   Key = (pred)
	;   Command = Body,	    Key = ''
	),  !,
	nl,
	numbervars(Body, 0, _),
	'list clauses'(Body, Key, 2, 8).
portray_clause((Pred:-Body)) :-
	numbervars(Pred+Body, 0, _),
	portable_writeq(Pred),
	'list clauses'(Body, 0, 2, 8), !.
portray_clause((Pred)) :-
	portray_clause((Pred:-true)).


'list clauses'((A,B), L, R, D) :- !,
	'list clauses'(A, L, 1, D), !,
	'list clauses'(B, 1, R, D).
'list clauses'(true, L, 2, D) :- !,
	put(0'.), nl.
'list clauses'((A;B), L, R, D) :- !,
	'list magic'(fail, L, D),
	'list magic'((A;B), 0, 2, D),
	'list magic'(R, '.
').
'list clauses'((A->B), L, R, D) :- !,
	'list clauses'(A, L, 5, D), !,
	'list clauses'(B, 5, R, D).
'list clauses'(Goal, L, R, D) :-
	'list magic'(Goal, L, D),
	portable_writeq(Goal),
	'list magic'(R, '.
').

'list magic'(!,    0, D) :- !,
	write(' :- ').
'list magic'(!,    1, D) :- !,
	write(',  ').
'list magic'(Goal, 0, D) :- !,
	write(' :- '),
	nl, tab(D).
'list magic'(Goal, 1, D) :- !,
	put(0',),
	nl, tab(D).
'list magic'(Goal, 3, D) :- !,
	write('(   ').
'list magic'(Goal, 4, D) :- !,
	write(';   ').
'list magic'(Goal, 5, D) :- !,
	write(' ->'),
	nl, tab(D).
'list magic'(Goal, Key, D) :-
	atom(Key),
	write(':- '), write(Key),
	nl, tab(D).


'list magic'(2, C) :- !, write(C).
'list magic'(_, _).


'list magic'((A;B), L, R, D) :- !,
	'list magic'(A, L, 1, D), !,
	'list magic'(B, 1, R, D).
'list magic'(Conj,  L, R, D) :-
	E is D+8,
	M is L+3,
	'list clauses'(Conj, M, 1, E),
	nl, tab(D),
	'list magic'(R, ')').


/*	Test code for portray_clause.
	If it works, test_portray_clause(File) should write out the
	contents of File in a more or less readable fashion.

test_portray_clause(File) :-
	see(File),
	repeat,
	    read(Clause, Vars),
	    (   Clause = end_of_file
	    ;   test_bind(Vars), portray_clause(Clause), fail
	    ),
	!,
	seen.

test_bind([]) :- !.
test_bind([X='$VAR'(X)|L]) :-
	test_bind(L).
:-public test_portray_clause/1.
*/



