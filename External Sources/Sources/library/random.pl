% The following code produces the same random numbers as my previous 
% ranpkg.pl, but is more accurately documented and slightly more 
% efficient.  

% ranpkg.pl	random number package	Allen Van Gelder, Stanford

% rannum produces a random non-negative integer whose low bits are not
% all that random, so it should be scaled to a smaller range in general.
% The integer is in the range 0 .. 2^(w-1) - 1,
% where w is the word size available for integers, e.g., 18 for DEC-10,
% and 16 or 32 for VAX and most IBM.
%
% ranunif produces a uniformly distributed non-negative random integer over
% a caller-specified range.  If range is R, the result is in 0 .. R-1.
%
% ranstart must be called before the first use of rannum or ranunif,
% and may be called later to redefine the seed.
% ranstart/0 causes a built-in seed to be used.
% ranstart(N), N an integer, varies this, but the same N always
% produces the same sequence of numbers.
%
% According to my reading of Knuth, Vol. 2, this generator has period
% 2^(w-1) and potency w/2, i.e., 8, 9, or 16 in practice.  Knuth says
% potency should be at least 5, so this looks more than adequate.
% Its drawback is the lack of randomness of low-order bits.

:- public ranstart/0.
:- public ranstart/1.
:- public rannum/1.
:- public ranunif/2.

:- mode ranstart(+), rannum(-), ranunif(+, -).

ranstart :- ranstart(8'365).

ranstart(N) :-
	Wsize is 18,				% bits available for int.
	MaxInt is \(1 << (Wsize - 1)),		% all bits but sign bit are 1.
	Incr is (8'154 << (Wsize - 9)) + 1,	% per Knuth, v.2 p.78
	Mult is 8'3655,				% OK for 16-18 Wsize
	Prev is Mult * (8 * N + 5) + Incr,
	recorda(ranState, ranState(Mult, Prev, Wsize, MaxInt, Incr), Ref).

rannum(Raw) :-
	recorded(ranState, ranState(Mult, Prev, Wsize, MaxInt, Incr), Oldref),
	erase(Oldref),
	Curr is Mult * Prev + Incr,
	recorda(ranState, ranState(Mult, Curr, Wsize, MaxInt, Incr), Ref),
	(	Curr > 0,
		Raw is Curr
	;
		Curr < 0,
		Raw is Curr /\ MaxInt		% force positive sign bit
	).

ranunif(Range, Unif) :-
	Range > 0,
	recorded(ranState, ranState(Mult, Prev, Wsize, MaxInt, Incr), Oldref),
	erase(Oldref),
	Curr is Mult * Prev + Incr,
	recorda(ranState, ranState(Mult, Curr, Wsize, MaxInt, Incr), Ref),
	(	Curr > 0,
		Raw is Curr
	;
		Curr < 0,
		Raw is Curr /\ MaxInt		% force positive sign bit
	),
	Unif is (Raw * Range) >> (Wsize-1).
