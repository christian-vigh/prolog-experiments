%   File   : DCG.PL
%   Author : Richard A. O'Keefe
%   Updated: Tuesday July 26th, 1988.
%   Purpose: Definite Clause Grammar rule to Prolog clause translator.

/*  This file is written in the ISO 8859/1 character set.  The "Copyright"
    line contains after the right parenthesis a character which when
    transmitted was character 169, the international copyright symbol.

    Copyright (C)) 1988, Quintus Computer Systems, Inc.

    This file is distributed in the hope that it will be useful,
    but without any warrantee.  You are expressly warned that it is meant
    to serve as a model, not for direct use:  all error checking and
    reporting has been omitted, and mistakes are almost surely present.
    Permission is granted to anyone to distribute verbatim copies of
    this source code as received, in any medium, provided that the
    copyright notice, the nonwarrantee warning, and this permission
    notice are preserved.  Permission is granted to distribute modified
    versions of this source code, or of portions of it, under the above
    conditions, plus the conditions that all changed files carry
    prominent notices stating who last changed them and that the derived
    material is subject to this same permission notice.  Permission is
    granted to include this material in products for which money is
    charged, provided that the customer is given written notice that the
    code is (or is derived from) material provided by Quintus Computer
    Systems, Inc., and that the customer is given this source code on
    request.

	----------------------------------------------------------------

    Now that we've got that (adapted from the GNU copyright notice)
    out of the way, here are the technical comments.

    The predicates are all named 'dcg <something>'/<some arity> in order
    to keep out of the way, with the exception of phrase/2 and phrase/3
    which bear their proper names.  Only phrase/[2,3] and 'dcg rule'/2
    are meant to be called directly, and 'dcg rule'/2 is meant to be called
    from expand_term/2.  You need to keep 'dcg body'/4 and its dependents
    around at run time so that variables as nonterminals in DCG rule bodies
    will work correctly.

    So that Quintus have _something_ left to sell, this code has been
    rewritten from scratch with no error checking or reporting code at
    all, and a couple of places accept general grammar rule bodies where
    they are really supposed to demand lists of terminals.  However, any
    rule which is valid according to the Quintus Prolog manual will be
    translated correctly, except that this code makes no attempt to handle
    module: prefixes.  (The change is trivial.)	

    Note that 'dcg rule'/2 and phrase/[2,3] are steadfast.
*/

%   dcg rule(+Grammar Rule, -Equivalent Clause)

'dcg rule'(-->(Head0,Body0), Clause) :-
	'dcg head'(Head0, Head, PushBack, S0, S),
	'dcg body'(Body0, Body1, S0, S),
	'dcg conj'(Body1, PushBack, Body),
	Clause = :-(Head,Body).


%   dcg head(+Head0, -Head, -PushBack, -S0, -S)
%   recognises both
%	NonTerminal, [PushBackList] -->
%   and
%	NonTerminal -->
%   It returns the difference pair S0\S which the body is to parse.
%   To avoid error checking, it will accept an arbitrary body in place
%   of a pushback list, but it should demand a proper list.

'dcg head'((Head0,PushBack0), Head, PushBack, S0, S1) :- !,
	'dcg goal'(Head0, Head, S0, S),
	'dcg body'(PushBack0, PushBack, S, S1).
'dcg head'(Head0, Head, true, S0, S) :-
	'dcg goal'(Head0, Head, S0, S).


%   dcg goal(+Goal0, -Goal, +S0, +S)
%   adds the arguments S0, S at the end of Goal0, giving Goal.
%   It should check that Goal0 is a callable term.

'dcg goal'(Goal0, Goal, S0, S) :-
	functor(Goal0, F, N),
	N1 is N+1,
	N2 is N+2,
	functor(Goal, F, N2),
	arg(N2, Goal, S),
	arg(N1, Goal, S0),
	'dcg args'(N, Goal0, Goal).


%   dcg args(+N, +Goal0, +Goal)
%   copies the first N arguments of Goal0 to Goal.

'dcg args'(N, Goal0, Goal) :-
	(   N =:= 0 -> true
	;   arg(N, Goal0, Arg),
	    arg(N, Goal,  Arg),
	    M is N-1,
	    'dcg args'(M, Goal0, Goal)
	).


%   dcg body(+Body0, -Body, +S0, +S)
%   translates Body0 to Body, adding arguments as needed to parse S0\S.
%   It should complain about bodies (such as 2) which are not callable
%   terms, and about lists of terminals which are not proper lists.
%   To avoid error checking, [a|foo] is accepted as [a],foo, but it
%   really should complain.  ONLY the forms lists here should be treated;
%   other non-terminals which look like calls to built-ins could well be
%   commented on (no error reporting here) but should be expanded even
%   so.  Thus X=Y as a nonterminal is to be rewritten as =(X,Y,S0,S),
%   perhaps with a warning.  If you want the translation X=Y, use {X=Y}.

'dcg body'(Var, Body, S0, S) :- var(Var), !,
	Body = phrase(Var,S0,S).
'dcg body'((A0,B0), Body, S0, S) :- !,
	'dcg body'(A0, A, S0, S1),
	'dcg body'(B0, B, S1, S),
	'dcg conj'(A, B, Body).
'dcg body'(->(A0,B0), ->(A,B), S0, S) :- !,
	'dcg body'(A0, A, S0, S1),
	'dcg body'(B0, B, S1, S).
'dcg body'(;(A0,B0), ;(A,B), S0, S) :- !,
	'dcg disj'(A0, A, S0, S),
	'dcg disj'(B0, B, S0, S).
'dcg body'({}(A), A, S, S) :- !.
'dcg body'(!, !, S, S) :- !.
'dcg body'([], true, S, S) :- !.
'dcg body'([H0|T0], Body, S0, S) :- !,
	'dcg term'(H0, H, S0, S1),
	'dcg body'(T0, T, S1, S),
	'dcg conj'(H, T, Body).
'dcg body'(NT0, NT, S0, S) :-
	'dcg goal'(NT0, NT, S0, S).


%   dcg term(+T0, -T, +S0, +S)
%   generates code (T) which succeeds when there is a terminal T0
%   between S0 and S.  This version uses the DEC-10 Prolog predicate
%   'C'/3 for compatibility with DEC-10 Prolog, C Prolog, Quintus Prolog.
%   This is the only place that knows how terminals are translated, so
%   you could supply instead the definition
%	'dcg term'(T0, S0=[T0|S], S0, S).
%   and reap the same benefits.  The one thing you must not do is
%   NO! 'dcg term'(T0, true, [T0|S], S). DON'T DO THAT!

'dcg term'(T0, 'C'(S0,T0,S), S0, S).


%  To see why dcg disj/4 is needed, consider the translation of
%  ( [] | [a] ).  We have to insert S1=S0 somewhere, but we do it at
%  "compile"-time if we can.

'dcg disj'(Body0, Body, S0, S) :-
	'dcg body'(Body0, Body1, S1, S),
	(   S1 == S -> 'dcg conj'(S1=S0, Body1, Body)
	;   S1 = S0, Body = Body1
	).


%   dcg conj(+A, +B, -C)
%   combines two conjunctions A, B, giving C.  Basically, we want to
%   ensure that there aren't any excess 'true's kicking around (in a
%   compiled system, that shouldn't matter).  There is room for some
%   leeway here: I have chosen to flatten A completely.

'dcg conj'(A, true, A) :- !.
'dcg conj'(A, B, C) :-
	'dcg CONJ'(A, B, C).

'dcg CONJ'(true, C, C) :- !.
'dcg CONJ'((A,As), C0, (A,C)) :- !,
	'dcg CONJ'(As, C0, C).
'dcg CONJ'(A, C, (A,C)).


%   'C'(S0, T, S)
%   is true when the terminal T "Connects" the "string positions" S0 and S.

'C'([T|S], T, S).


%   phrase(+NT0, ?S0)
%   is true when the list S0 is in the language defined by the
%   grammar rule body NT0.  E.g. phrase(([a],[b]), [a,b]).

phrase(NT0, S0) :-
	phrase(NT0, S0, []).


%   phrase(+NT0, ?S0, ?S)
%   is true when the list S0\S is in the language defined by the
%   grammar rule body NT0.  E.g. phrase(([a],[b]), [a,b|X], X).

phrase(NT0, S0, S) :-
	'dcg body'(NT0, NT, T0, T),
	T0 = S0, T = S,
	call(NT).

