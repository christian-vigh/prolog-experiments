%   File   : RANDOM.PL
%   Author : Allen Van Gelder, Stanford
%   Updated: 21 February 1984
%   Purpose: Random number package.

% rannum produces a random non-negative integer whose low bits are not
% all that random, so it should be scaled to a smaller range in general.
% The integer is in the range 0 .. 2^w - 1,
% where w is the word size less the sign bit, e.g., 17 for DEC-10,
% and 15 or 31 for VAX and most IBM.
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
% 2^(w+1) and potency (w+1)/2, i.e., 8, 9, or 16 in practice.  Knuth says
% potency should be at least 5, so this looks more than adequate.
% Its drawback is the lack of randomness of low-order bits.

:- public
	ranstart/0,
	ranstart/1,
	rannum/1,
	ranunif/2.

:- mode
	ranstart(+),
	rannum(-),
	ranunif(+, -).

ranstart :-
	ranstart(245).


ranstart(N) :-
	Wsize is 17,			% bits available other than sign-bit
	Incr is (108 << (Wsize-8)) + 1,	% per Knuth, v.2 p.78
	Mult is 1965,			% OK for 15-17 Wsize
	Prev is Mult * (8 * N + 5) + Incr,
	recorda(ranState, ranState(Mult,Prev,Wsize,Incr), _).


rannum(Raw) :-
	recorded(ranState, ranState(Mult,Prev,Wsize,Incr), Ref),
	Curr is Mult * Prev + Incr,
	erase(Ref),
	recorda(ranState, ranState(Mult,Curr,Wsize,Incr), _),
	(   Curr > 0, Raw = Curr
	|   Curr < 0, Raw is Curr + (1<<Wsize)
	),  !.


ranunif(Range, Unif) :-
	Range > 0,
	recorded(ranState, ranState(Mult,Prev,Wsize,Incr), Ref),
	Curr is Mult * Prev + Incr,
	erase(Ref),
	recorda(ranState, ranState(Mult,Curr,Wsize,Incr), _),
	(   Curr > 0, Raw = Curr
	|   Curr < 0, Raw is Curr + (1<<Wsize)
	),  !,
	Unif is (Raw * Range) >> Wsize.


