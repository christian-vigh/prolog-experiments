%   File   : /usr/lib/prolog/random
%   Author : R.A.O'Keefe
%   Updated: 27 October 83
%   Purpose: to provide a decent random number generator in C-Prolog.

%   This is algorithm AS 183 from Applied Statistics.  I also have a C
%   version.  It is really very good.  It is straightforward to make a
%   version which yields 15-bit random integers using only integer
%   arithmetic.

'$rstate'(27134, 9213, 17773).		%  initial state

getrand('$rstate'(X,Y,Z)) :-		%  return current state
	'$rstate'(X,Y,Z).


setrand('$rstate'(X,Y,Z)) :-
	integer(X), X > 0, X < 30269,
	integer(Y), Y > 0, Y < 30307,
	integer(Z), Z > 0, Z < 30323,
	retract('$rstate'(_,_,_)),
	asserta('$rstate'(X,Y,Z)), !.
	

%   random(R) binds R to a new random number in [0.0,1.0)

random(R) :-
	retract('$rstate'(A0,B0,C0)),
	A1 is (A0*171) mod 30269,
	B1 is (B0*172) mod 30307,
	C1 is (C0*170) mod 30323,
	asserta('$rstate'(A1,B1,C1)),
	T is (A1/30269.0) + (B1/30307.0) + (C1/30323.0),
	R is T-floor(T), !.


%   random(L, U, R) binds R to a random integer in [L,U)
%   when L and U are integers (note that U will NEVER be generated),
%   or to a random floating number in [L,U) otherwise.

random(L, U, R) :-
	integer(L), integer(U),
	random(X), !,
	R is L+floor((U-L)*X).
random(L, U, R) :-
	number(L), number(U),
	random(X), !,
	R is L+((U-L)*X).


%   File   : /usr/lib/prolog/randseq
%   Author : R.A.O'Keefe
%   Updated: Sunday October 23rd, 1983, 9:22:40 pm
%   Purpose: Generate K random integers 1..N

/*  There are two versions of this operation.

	randset(K, N, S)

    generates a random set of K integers in the range 1..N.
    The result is an ordered list, such as setof might produce.

	randseq(K, N, L)

    generates a random sequence of K integers, the order is as
    random as we can make it.
*/

randset(K, N, S) :-
	randset(K, N, S, []).


randset(0, _, S, S) :- !.
randset(K, N, [N|Si], So) :-
	random(X),
	X * N < K, !,
	J is K-1,
	M is N-1,
	randset(J, M, Si, So).
randset(K, N, Si, So) :-
	M is N-1,
	randset(K, M, Si, So).


randseq(K, N, S) :-
	randseq(K, N, L, []),
	keysort(L, R),
	strip_keys(R, S).

randseq(0, _, S, S) :- !.
randseq(K, N, [Y-N|Si], So) :-
	random(X),
	X * N < K, !,
	random(Y),
	J is K-1,
	M is N-1,
	randseq(J, M, Si, So).
randseq(K, N, Si, So) :-
	M is N-1,
	randseq(K, M, Si, So).


strip_keys([], []) :- !.
strip_keys([_-K|L], [K|S]) :-
	strip_keys(L, S).

