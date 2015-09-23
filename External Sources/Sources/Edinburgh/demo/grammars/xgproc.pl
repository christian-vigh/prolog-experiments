%   File   : XGPROC
%   Author : Fernando Pereira
%   Updated: 23 September 1984
%   Purpose: Translate XGs to Prolog

:- public
	(+)/1,
	(-)/1,
	list/1.

:- op(1001, xfy, ...).
:- op(500, fx, +).
:- op(500, fx, -).

:- mode
	and(+, +, -),
	case(+, +, ?, ?),
	conc(+, ?, -),
	consume0(+, +),
	tidy_consume(+),
	consume(+),
	expandlhs(+, ?, ?, ?, ?, -),
	expandlist(+, ?, ?, ?, ?, -),
	expandor(+, ?, ?, ?, ?, -),
	expandrhs(+, ?, ?, ?, ?, -),
	flatten(+, ?, ?),
	front(+, ?, ?),
	+(+),
	islist(+),
	new_pred(+),
	process(+, +),
	-(+),
	retractrule(+, ?),
	retractrules(+),
	tag(+, ?, ?, ?, ?, -),
	usurping(+, +),
	unwind(+, ?, ?),
	virtualrule(+).


new_pred(P) :-
	recorded(P, 'xg.pred', _),
	!.
new_pred(P0) :-
	functor(P0, F, N),
	functor(P, F, N),
	recordz(P, 'xg.pred', _),
	recordz('xg.pred', P, _).


+(File) :-
	consume0(File, +).

-(File) :-
	consume0(File, -).

consume0(File, Mode) :-
	seeing(Old),
	statistics(heap, [H0,Hf0]),
	see(File),
	tidy_consume(Mode),
	seen,
	see(Old),
	statistics(heap, [H,Hf]),
	U is H-Hf-H0+Hf0,
	write('** Grammar from file '), write(File), write(' : '),
	write(U), write(' words **'), nl, nl.

tidy_consume(Mode) :-
	consume(Mode),
	fail.
tidy_consume(_).

consume(Mode) :-
	repeat,
	read(X),
	(   X = end_of_file, !, clear
	;   process(X, Mode), fail
	).

process((L-->R), Mode) :- !,
	expandlhs(L, S0, S, H0, H, P),
	expandrhs(R, S0, S, H0, H, Q),
	new_pred(P),
	usurping(Mode, P),
	assertz((P :- Q)),
	!.
process((:- G), _) :- !,
	call(G).
process((P :- Q), Mode) :- !,
	usurping(Mode, P),
	new_pred(P),
	assertz((P :- Q)).
process(P, Mode) :-
	usurping(Mode, P),
	new_pred(P),
	assertz(P).

clear :-
	recorded('xg.usurped', P, R0),
	erase(R0),
	recorded(P, 'xg.usurped', R1),
	erase(R1),
	fail.
clear.

usurping(+, _) :- !.
usurping(-, P) :-
	recorded(P, 'xg.usurped', _),
	!.			% already usurped
usurping(-, P) :-
	functor(P, F, N),
	functor(Q, F, N),
	retractrules(Q),
	recordz(Q, 'xg.usurped', _),
	recordz('xg.usurped', Q, _).

retractrules(Q) :-		% forall(clause(Q,B), retractrule(Q,B))
	clause(Q, B),
	retractrule(Q, B),
	fail.
retractrules(_).

retractrule(_, virtual(_,_,_)) :- !.
retractrule(Q, B) :-
	retract((Q :- B)),
	!.

/* Rule --> Clause */

expandlhs(T, S0, S, H0, H1, Q) :-
	flatten0(T, [P|L], []),
	front(L, H1, H),
	tag(P, S0, S, H0, H, Q).

flatten0(X, _, _) :-
	var(X),
	write('! Variable as a non-terminal in the lhs of a grammar rule'), nl,
	!, fail.
flatten0(X, L0, L) :-
	flatten(X, L0, L).

flatten((X...Y), L0, L) :- !,
	flatten0(X, L0, [gap|L1]),
	flatten0(Y, L1, L).
flatten((X,Y), L0, L) :- !,
	flatten0(X, L0, [nogap|L1]),
	flatten0(Y, L1, L).
flatten(X, [X|L], L).

front([], H, H).
front([K,X|L], H0, H) :-
	case(X, K, H1, H),
	front(L, H0, H1).

case([T|Ts], K, H0, x(K,terminal,T,H)) :- !,
	unwind(Ts, H0, H).
case(Nt, K, H, x(K,nonterminal,Nt,H)) :-
	virtualrule(Nt).

virtualrule(X) :-
	functor(X, F, N),
	functor(Y, F, N),
	tag(Y, S, S, Hx, Hy, P),
	(   clause(P, virtual(_,_,_)), !
	;   new_pred(P), asserta((P :- virtual(Y,Hx,Hy)))
	).

expandrhs((X1,X2), S0, S, H0, H, Y) :- !,
	expandrhs(X1, S0, S1, H0, H1, Y1),
	expandrhs(X2, S1, S, H1, H, Y2),
	and(Y1, Y2, Y).
expandrhs((X1;X2), S0, S, H0, H, (Y1;Y2)) :- !,
	expandor(X1, S0, S, H0, H, Y1),
	expandor(X2, S0, S, H0, H, Y2).
expandrhs({X}, S, S, H, H, X) :- !.
expandrhs(L, S0, S, H0, H, G) :-
	islist(L),
	!,
	expandlist(L, S0, S, H0, H, G).
expandrhs(X, S0, S, H0, H, Y) :-
	tag(X, S0, S, H0, H, Y).

expandor(X, S0, S, H0, H, Y) :-
	expandrhs(X, S0a, S, H0a, H, Ya),
	(   S == S0a, !, and(S0=S0a, Ya, Yb)
	;   S0 = S0a, Yb = Ya
	),
	(   H == H0a, !, and(H0=H0a, Yb, Y)
	;   H0 = H0a, Y = Yb
	).

expandlist([], S, S, H, H, true).
expandlist([X], S0, S, H0, H, terminal(X,S0,S,H0,H) ) :- !.
expandlist([X|L], S0, S, H0, H, (terminal(X,S0,S1,H0,H1),Y)) :-
	expandlist(L, S1, S, H1, H, Y).

tag(P, A1, A2, A3, A4, Q) :-
	P =.. [F|Args0],
	conc(Args0, [A1,A2,A3,A4], Args),
	Q =.. [F|Args].

and(true, P, P) :- !.
and(P, true, P) :- !.
and(P, Q, (P,Q)).

islist([_|_]).
islist([]).

unwind([], H, H) :- !.
unwind([T|Ts], H0, x(nogap,terminal,T,H)) :-
	unwind(Ts, H0, H).

conc([], L, L) :- !.
conc([X|L1], L2, [X|L3]) :-
	conc(L1, L2, L3).

list(File) :-
	telling(Old),
	tell(File),
	list_clauses,
	told,
	tell(Old).

list_clauses :-
	recorded('xg.pred', P, _),
	functor(P, F, N),
	listing(F/N),
	nl,
	fail.
list_clauses.
