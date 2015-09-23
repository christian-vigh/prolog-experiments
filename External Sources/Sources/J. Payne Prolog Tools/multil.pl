
%   File   : MULTIL.PL
%   Author : Lawrence Byrd
%   Updated: 18 May 1983
%   Purpose: Multiple-list routines

%   This module runs compiled.  It needs some things from Util:Applic.Pl
%   The style of programming which would find these routines useful is
%   now frowned upon.  However, you may find their structure instructive.

:- public
	mlmaplist/2,
	mlmaplist/3,
	mlmaplist/4,
	mlmember/2,
	mlselect/3.

:- mode
	mlmaplist(+, +), 
	mlmaplist(+, +, ?),
	mlmaplist(+, +, ?, ?),
	mlmember(?, +),
	mlmember(+, +, ?),
	mlselect(?, +, ?),
	mlselect(+, +, ?, ?),
	ml_putback(+, ?, ?),
	ml_taketop(+, -, -),
	ml_allempty(+).



%   ml_taketop(Lists, Heads, Tails)
%   is true when Lists is a list of non-empty lists, Heads is a list
%   whose elements are the heads of the elements of Lists, and Tails
%   is a list whose elements are the tails of Lists.

ml_taketop([], [], []).
ml_taketop([[Head|Tail]|Lists], [Head|Heads], [Tail|Tails]) :-
	ml_taketop(Lists, Heads, Tails).

%   ml_allempty(Lists)
%   is true when Lists is a list, all of whose elements are nil ([]).
%   It is used to test whether all the lists being mapped over have
%   come to an end at once.  Since ml_taketop will succeed precisely
%   when all the lists have at least one member, we could produce a
%   set of routines that terminate when any list runs out by simply
%   omitting this test.  As it is, the ml* routines demand that all
%   the lists be the same length.

ml_allempty([]).
ml_allempty([[]|Tail]) :-
	ml_allempty(Tail).

%   mlmaplist(Pred, Lists)
%   applies Pred to argument tuples which are successive slices of the Lists.
%   Thus mlmaplist(tidy, [Untidy,Tidied]) would apply tidy([U,T]) to each
%   successive [U,T] pair from Untidy and Tidied.  It isn't tail-recursive,
%   because Pred (and hence apply) may backtrack.

mlmaplist(Pred, Lists) :-
	ml_taketop(Lists, Heads, Tails),
	apply(Pred, [Heads]),
	mlmaplist(Pred, Tails).
mlmaplist(_, Lists) :-
	ml_allempty(Lists).



%   mlmaplist(Pred, Lists, Extra)
%   is like mlmaplist/2, but passes the Extra argument to Pred as well
%   as the slices from the Lists.

mlmaplist(Pred, Lists, Extra) :-
	ml_taketop(Lists, Heads, Tails), !,
	apply(Pred, [Heads, Extra]),
	mlmaplist(Pred, Tails, Extra).
mlmaplist(_, Lists, _) :-
	ml_allempty(Lists).

%   mlmaplist(Pred, Lists, Init, Final)
%   is like mlmaplist/2, but has an extra accumulator feature.  Init is
%   the initial value of the accumulator, and Final is the final result.
%   Pred(Slice, AccIn, AccOut) is called to update the accumulator.

mlmaplist(Pred, Lists, AccOld, AccNew) :-
	ml_taketop(Lists, Heads, Tails),
	!,
	apply(Pred, [Heads, AccOld, AccMid]),
	mlmaplist(Pred, Tails, AccMid, AccNew).
mlmaplist(_, Lists, Accum, Accum) :-
	ml_allempty(Lists).

%   mlmember(Elems, Lists) and mlselect(Elems, Lists, Residues)
%   are the multi-list analogues of member and select.  The definition
%   of mlselect is difficult to blieve; it is almost certainly utterly
%   useless.  But it is retained, as that is how it has always been.
%   Example
%   ml_member([a,d,g], [[a,b,c],[d,e,f],[g,h,i]])
%   is true,
%   ml_member(X, [[a,b,c],[d,e,f],[g,h,i]])
%   instantiates X = [a,d,g]  X = [b,e,h]  X = [c,f,i]

ml_member(Elems, Lists) :-
	ml_taketop(Lists, Heads, Tails), !,
	ml_member(Heads, Tails, Elems).

ml_member(Heads, _, Heads).
ml_member(_, Tails, Elems) :-
	ml_member(Elems, Tails).

ml_select(Elems, Lists, Residues) :-
	ml_taketop(Lists, Heads, Tails),
	!,
	ml_select(Heads, Tails, Elems, Residues).

ml_select(Heads, Tails, Heads, Tails).
ml_select(Heads, Tails, Elems, Residues) :-
	ml_putback(Heads, Rests, Residues), !,
	ml_select(Elems, Tails, Rests).

%   ml_putback(+Heads, ?Tails, ?Lists)
%   is true when ml_taketop(Lists, Heads, Tails) is true, but is
%   rearranged for efficiency with different calling pattern.  It
%   only exists for the benefit of ml_select, and as the bug in the
%   latter went unnnoticed for 3 years, I) don't suppose it matters
%   much.

ml_putback([], [], []).
ml_putback([Head|Heads], [Tail|Tails], [[Head|Tail]|Lists]) :-
	ml_putback(Heads, Tails, Lists).




%   ml_putback(+Heads, ?Tails, ?Lists)
%   is true when ml_taketop(Lists, Heads, Tails) is true, but is
%   rearranged for efficiency with different calling pattern.  It
%   only exists for the benefit of mlselect, and as the bug in the
%   latter went unnnoticed for 3 years, I) don't suppose it matters
%   much.

ml_putback([], [], []).
ml_putback([Head|Heads], [Tail|Tails], [[Head|Tail]|Lists]) :-
	ml_putback(Heads, Tails, Lists).



