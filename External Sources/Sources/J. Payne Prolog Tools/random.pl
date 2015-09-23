%   File   : RANDOM.PL
%   Author : R.A.O'Keefe
%   Updated: 1 October 1984
%   NIP version: 13 May 1987
%   Purpose: Random number generator.

%   given an integer N >= 1, random(N, I) unifies I with a random
%   integer between 0 and N - 1.

random(N, I) :-
	(	recorded(seed,[A0,A1,A2],Key)
		->	erase(Key)
	;
			A0 = 3172, A1 = 9814, A2 = 20125
	),
	B0 is (A0*171) mod 30269,
	B1 is (A1*172) mod 30307,
	B2 is (A2*170) mod 30323,
	record(seed,[B0,B1,B2],_),
	R is A0/30269 + A1/30307 + A2/30323,
	I is trunc((R - trunc(R)) * N).

%	The next bit: K R Johnson, 13-5-87
%	Restart the random sequence from the beginning

randomise :-
	(	recorded(seed,[_,_,_],Key)
		->	erase(Key)
		;	true
	),
	record(seed, [3172,9814,20125], _).

%	Instantiate the seeds to your own favourite value

randomise(Seed) :-
	integer(Seed),
	Seed > 0,
	(	recorded(seed,[_,_,_], Key)
		->	erase(Key)
		;	true
	),
	S0 is Seed mod 30269,
	S1 is Seed mod 30307,
	S2 is Seed mod 30323,
	record(seed,[S0,S1,S2],_).

%   given an non-empty List, random(List, Elem, Rest) unifies Elem with
%   a random element of List and Rest with the other elements.

random(List, Elem, Rest) :-
	length(List, N),
	N > 0,
	random(N, I),
	nth0(I, List, Elem, Rest).


%   rand_perm(List, Perm) unifies Perm with a random permutation
%   of List.  What good this may be I'm not sure, and there's bound
%   to be a more efficient way of doing it.  Oh well.

rand_perm([], []).
rand_perm([H1|T1], [H2|T2]) :-
	random([H1|T1], H2, T3),
	rand_perm(T3, T2).

