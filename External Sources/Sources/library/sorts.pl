%   File   : SORTS.PL
%   Author : R.A.O'Keefe
%   Updated: 23 July 1984
%   Purpose: Specify generalised sorting routines.

/*  Note: these definitions are to be read as specifications of what
    arguments are acceptable and what results should be obtained.
    They are NOT to be read as being the code which should actually
    be used.  I have C versions of these routines which are efficient
    with no redundant storage turnover and which make using the general
    routines nearly as fast as using the specific versions would be.
    The C code can also exploit any existing order or reversed order.
    Since keysort/2 and sort/2 are built into Dec-10 Prolog:
*/  THIS FILE CANNOT BE LOADED INTO DEC-10 PROLOG WITHOUT RENAMING.

:- public
	keysort/2,
	merge/3,
	merge/5,
	msort/2,
	sort/2,
	sort/4.

:- mode
	combine(+, +, -),
	compare(+, +, +, -),
	compare(+, +, +, +, -),
	halve(+, +, -, -),
	keysort(+, -),	
	merge(+, +, -),
	merge(+, +, +, +, -),
	msort(+, -),
	sort(+, -),
	sort(+, +, +, -).

sort(Key, Order, [], []).
sort(Key, Order, [X], [X]).
sort(Key, Order, [X,Y|L], Sorted) :-
	halve(L, [Y|L], Front, Back),
	sort(Key, Order, [X|Front], F),
	sort(Key, Order, Back, B),
	merge(Key, Order, F, B, Sorted).


halve([_,_|Count], [H|T], [H|F], B) :- !,
	halve(Count, T, F, B).
halve(_, B, [], B).


merge(Key, Order, [H1|T1], [H2|T2], [Hm|Tm]) :- !,
	compare(Key, Order, H1, H2, R),
	(   R = (<), !, Hm = H1, merge(Key, Order, T1, [H2|T2], Tm)
	;   R = (>), !, Hm = H2, merge(Key, Order, [H1|T1], T2, Tm)
	;   R = (=), !, Hm = H1, merge(Key, Order, T1, T2, Tm)
	).
merge(_, _, [], L, L) :- !.
merge(_, _, L, [], L).


compare(Key, Order, X, Y, R) :-
	compare(Key, X, Y, R0),
	combine(Order, R0, R).

compare(0, X, Y, R) :- !,
	compare(R, X, Y).
compare(N, X, Y, R) :-
	arg(N, X, Xn),
	arg(N, Y, Yn),
	compare(R, Xn, Yn).


combine(<, R, R).
combine(=<, >, >) :- !.
combine(=<, _, <).
combine(>=, <, >) :- !.
combine(>=, _, <).
combine(>, <, >) :- !.
combine(>, >, <) :- !.
combine(>, =, =).


keysort(R, S) :-
	sort(1, =<, R, S).


msort(R, S) :-
	sort(0, =<, R, S).


sort(R, S) :-
	sort(0, <, R, S).


merge(A, B, M) :-
	merge(0, =<, A, B, M).

