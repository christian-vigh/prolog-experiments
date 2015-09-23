%   File   : TERMIN.PL
%   Author : R.A.O'Keefe
%   Updated: 15 October 1984
%   Purpose: Check for missing base cases.
%   updated for NIP: Ken Johnson 24-6-87

/*  termin(File)  loads a file of Prolog clauses, which may use
    disjunctions but not negations or foralls, and stores it as
    recorded terms in the firm recorded(if,(pred/arity),_)

    append will be stored as
	if(append/3, []).
	if(append/3, [append/3]).

    in fact it is a little bit cleverer than that.  If
    the predicate symbol on the left occurs on the right,
    the clause can be dropped entirely.  So we just get
	if(append/3, []).

    It also ensures that there is a pred(<predsym>) fact
    for each predicate defined.

    termin/0 copies the if/2 clauses as fi/2, dropping (and
    commenting on) any clauses that mention undefined predicates.
    Termin/1 will load util:system.pl the first time it is called
    so that built in p;redicates will not be lost.

    Then, for each pred, it tries to prove that symbol, which
    amounts to proving that it might have a terminating computation.
    If this fails, it might mean that the predicate will just fail,
    or it might mean that it won't terminate.  (p :- fail. and
    p :- p. look the same to termin.)
    Note that if P fails, every pred that calls P will fail too.
    This is the main thing that stops this being a practical tool.
*/

% Change the next clause when moving to a new directory

system_preds_are_in('/user1/ken/prolog/toolkit/terms/system_preds.pl').

termin(_) :-
	\+ system(_),
	system_preds_are_in(File),
	reconsult(File),
	system(Goal),
	process(Goal),
	fail.

termin(File) :-
	see(File),
	repeat,
	    read(Rule),
	    expand_term(Rule, Clause),
	    process(Clause),
	    Clause = end_of_file,
	!,
	seen.

process(end_of_file) :- !.

process(:-(_)) :- !.

process(:-(Head,Body)) :-
	!,
	note_head(Head, Pred),
	flat_rule(Body, Flat, []),
	sort(Flat, Sorted),
	note_rule(Pred, Sorted).

process(Head) :-
	note_head(Head, Pred),
	note_rule(Pred, []).

note_head(Head, F/N) :-
	functor(Head, F, N),
	(   pred(F/N)
	;
	    record(pred,(F/N),_)
	),
	!.

note_rule(Pred, Body) :-
	(   memberchk(Pred, Body)
	;
	    recorded(if,[Pred,Body],_)
	;
	    record(if,[Pred,Body],_)
	),
	!.

flat_rule((A,B), L0, L) :- !,
	flat_rule(A, L0, L1),
	flat_rule(B, L1, L).
flat_rule((A;_), L0, L) :-
	flat_rule(A, L0, L).
flat_rule((_;B), L0, L) :- !,
	flat_rule(B, L0, L).
flat_rule(!, L, L) :- !.
flat_rule(true, L, L) :- !.
flat_rule(fail, _, _) :- !,
	fail.
flat_rule(Head, [F/N|L], L) :-
	functor(Head, F, N).


termin :-
	(
		recorded(fi,_,Key),
		erase(Key),
		fail
	;
		(
			recorded(if,[Pred,Body],_),
			if_to_fi(Pred, Body), fail
		;	pred(Pred), check(Pred), fail
		;	true
		)
	).



if_to_fi(Pred, Body) :-
	member(Undef, Body),
	\+ pred(Undef),
	!,
	writef('! %t calls %t, which is not defined.\n', [Pred,Undef]).
if_to_fi(Pred, Body) :-
	record(fi,[Pred,Body],_).


check(Pred) :-
	prove(Pred, []),
	!.
check(Pred) :-
	writef('! %t has no terminating proof.\n', [Pred]).


prove(Query, Stack) :-
	\+ memberchk(Query, Stack),
	recorded(fi,[Query,Body],_),
	prove_body(Body, [Query|Stack]).

prove_body([], _) :- !.
prove_body([H|T], Stack) :-
	prove(H, Stack),
	prove_body(T, Stack).

