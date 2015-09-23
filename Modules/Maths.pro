/****h* Modules/Maths
 ===============================================================================
 *
 * NAME
 *	Math - Mathematical predicates.
 *
 * FILE
 *	Modules/Maths.pro
 *
 * CONTENTS
 *	Implements a few mathematical/algebraic functions.
 *
 * AUTHOR
 *	Christian Vigh, July 2005.
 *
 ===============================================================================
 ******/


:- module(maths).
:-	export([average/2]).
:-	export([c/3]).
:-	export([fact/2]).
:-	export([p/3]).
:-	export([pgcd/3]). 
:-	export([say/2]).
:-	export([sigma/2]).
:-	export([sum/2]).
:-	export([sumtoN/2]).
:- end_module(maths).
 
:- body(maths).



/****f* Modules.Maths/average
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	average/2
 *
 * SYNTAX
 *	average(List, Result)
 *
 * PURPOSE
 *	Sets [Result] to the average value of elements in [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose average value is to be computed.
 *	[Result] (o) -
 *		Average value of all the elements in [List].
 *
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

average([], 0).
average(List, Result) :-
	doaverage(List, 0, 0, Result).
	
doaverage([X|Tail], Counter, Sum, Result) :-
	NewSum is Sum + X,
	NewCounter is Counter + 1,
	doaverage(Tail, NewCounter, NewSum, Result).
doaverage([], Counter, Sum, Result) :-
	Result is Sum / Counter.

/******/





/****f* Modules.Maths/c
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	c/3
 *
 * SYNTAX
 *	c(N, P, Result).
 *
 * PURPOSE
 * 	Computes the number of combinations :
 *			    n!
 *		C(n,p) = ---------
 *			 p! (n-p)!
 *
 * ARGUMENTS
 *	[N] (i) -
 *		Number of elements in the set to be considered.
 *	[P] (i) -
 *		Number of elements from the set to compute combinations of.
 *	[Result] (o) -
 *		Result of P(n,p).
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

c(N, P, Result) :-
	N < 1; P < 1; N < P, 
	!, fail.
c(N, P, Result) :-
	fact(P, X),
	p(N, P, R),
	Result is R / X.

/******/



	

/****f* Modules.Maths/p
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	p/3
 *
 * SYNTAX
 *	p(N, P, Result)
 *
 * PURPOSE
 *	Computes the number of permutations :
 *			   n!
 *		P(n,p) = ------
 *			 (n-p)!
 *
 * ARGUMENTS
 *	[N] (i) -
 *		Number of elements in the set to be considered.
 *	[P] (i) -
 *		Number of elements from the set to compute permutations of.
 *	[Result] (o) -
 *		Result of P(n,p).
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

p(N, P, Result) :-
	N < 1; P < 1; N < P, 
	!, fail.
p(N, P, Result) :-
	fact(N, NFact),
	NP is N - P,
	fact(NP, NPFact),
	Result is NFact / NPFact.

/******/




/****f* Modules.Maths/fact
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	fact/2
 *
 * SYNTAX
 *	fact(Number, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the factorial of [Number].
 *
 * ARGUMENTS
 *	[Number] (i) -
 *		Number whose factorial is to be computed/
 *	[Result] (o) -
 *		Factorial of [Number].
 -------------------------------------------------------------------------------
 * SOURCE
 */

fact(Number, _) :-
	Number < 0,
	!, fail.
fact(0, 1) :- !.
fact(1, 1) :- !.
fact(Number, Result) :-
	NewNumber is Number - 1,
	fact(NewNumber, X),
	Result is X * Number.

/******/


	

/****f* Modules.Maths/pgcd
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	pgcd/3
 *
 * SYNTAX
 *	pgcd(X,Y,Result)
 *
 * PURPOSE
 *	Computes the pgcd of [X] and [Y].
 *
 * ARGUMENTS
 *	[X], [Y] (i) -
 *		Values for which to compute the pgcd.
 *	[Result] (o) -
 *		Greatest common divisor of [X] and [Y].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

pgcd(X, X, X).
pgcd(A, B, Result) :-
	A > B,
	NewA is A - B,
	pgcd(NewA, B, Result).
pgcd(A, B, Result) :-
	A < B,
	NewB is B - A,
	pgcd(A, NewB, Result).

/******/





/****f* Modules.Maths/sigma
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	sigma/2
 *
 * SYNTAX
 *	sigma(List, Result)
 *
 * PURPOSE
 *	Unifies [Result] to the multiplication of every element in [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose elements are to be multiplied.
 *	[Result] (o) -
 *		Result of the multiplication.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

sigma([], 1).
sigma(X, 0) :-		% If one element is zero, then the result of the multiplication is zero
	belongs(0, X).	
sigma([X|Tail], Result) :-
	sigma(Tail, TempResult),
	Result is TempResult * X.

/******/




/****f* Modules.Maths/sum
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	sum/2
 *
 * SYNTAX
 *	sum(List, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the sum of all elements in [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose elements are to be summed.
 *	[Result] (o) -
 *		Sum of all the elements in [List].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

sum([], 0).		% Allow for empty lists
sum([X], X).
sum([H|T], X) :-
	sum(T, XBis), X is XBis + H.

/******/





/****f* Modules.Maths/sumtoN
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	sumtoN/2
 *
 * SYNTAX
 *	sumtoN(N, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the sum of integers from 1 to [N].
 *
 * ARGUMENTS
 *	[N] (i) -
 *		Last integer to sum.
 *	[Result] (o) -
 *		Sum of all the elements from 1 to N.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

sumtoN(0, 0).
sumtoN(N, Result) :-
	NewN is N - 1,
	sumtoN(NewN, X),
	Result is X + N.

/******/


:- end_body(maths).