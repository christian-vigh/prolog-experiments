%   File   : ORDER.PL
%   Author : R.A.O'Keefe
%   Updated: 12 June 1984, conv to K Johnson, NIP 11-8-87
%   Purpose: Define the "ordered" predicates.

:- public
	len/2,
	ordered/1,
	ordered/2.

:- mode
	ordered(+),
	    ordered_(+, +),
	ordered(+, +),
	    ordered_(+, +, +),
	len(?, ?),
	    len_(+, ?),
	    len_(+, +, -).

%   ordered(List)
%   is true when List is a list of terms [T1,T2,...,Tn] such that
%   for all k in 2..n Tk-1 @=< Tk, i.e. T1 @=< T2 @=< T3 ...
%   The output of keysort/2 is always ordered, and so is that of
%   sort/2.  Beware: just because a list is ordered does not mean
%   that it is the representation of an ordered set; it might contain
%   duplicates.  E.g. L = [1,2,2,3] & sort(L,M) => ordered(L) & M\=L.

ordered([]).
ordered([Head|Tail]) :-
	ordered_(Tail, Head).

ordered_([], _).
ordered_([Head|Tail], Left) :-
	Left @=< Head,
	ordered_(Tail, Head).



%   ordered(P, [T1,T2,...,Tn]) means P(T1,T2) & P(T2,T3) & ...
%   i.e. that the second argument is ordered if you regard the
%   first argument as =<.  This is good for generating prefixes
%   of sequences, e.g. L = [1,_,_,_,_] & ordered(times(2),L) yields
%   L = [1,2,4,8,16].

ordered(_, []).
ordered(Relation, [Head|Tail]) :-
	ordered_(Tail, Head, Relation).

ordered_([], _, _).
ordered_([Head|Tail], Left, Relation) :-
	apply(Relation, [Left,Head]),
	ordered_(Tail, Head, Relation).



%   To exploit ordered/2 fully, we need a way of generating lists of
%   a given length.  I trust that a Prolog Standard will demand that
%   length/2 be reversible.  Until then, here is a reversible length.
%   len_/2 generates a list of a given length.  len_/3 measures the
%   length of a given list.  It reports an error if you give it a
%   variable or a list with a variable tail because then it would
%   backtrack forever trying ever longer lists if there was a
%   failure upstream, and this is generally not a useful thing to do.
%   Note: this code is really hacky, that's because of the error
%   detection.  Making len_/3 fail for variables so that len/2 can
%   report the error on the original list, faugh!

len(List, Length) :-
	nonvar(Length),
	!,
	integer(Length),
	len_(Length, List).

len(List, Length) :-
	nonvar(List),		% we know that var(Length)
	len_(List, 0, Length),	% so len_/3 will work for proper lists
	!.			% and fail for vars and non-lists

len(List, Length) :-
	nl, write('! bad arguments in '),
	write(len(List,Length)),
	nl,
	break,
	abort.


len_(0, []).
len_(N, [_|Tail]) :-
	N > 0,
	M is N-1,
	len_(M, Tail).


len_([], Length, Length).
len_([_|Tail], SoFar, Length) :-
	nonvar(Tail),
	Next is SoFar+1,
	len(Tail, Next, Length).


