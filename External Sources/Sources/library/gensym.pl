%   File   : GENSYM.PL
%   Author : Lawrence Byrd?
%   Updated: 21 February 1984
%   Purpose: create new atoms
%   Needs  : append/3.

:- public
	cgensym/2,
	concat/3,
	gensym/2.

:- mode
	cgensym(+, ?),
	concat(+, +, ?),
	gensym(+, ?).


%   gensym(Prefix, V)
%   binds V to a new atom whose name begins with Prefix and ends with a
%   number.  E.g. gensym(a,X), gensym(a,Y), gensym(a,Z) might bind
%   X to a1, Y to a2, Z to a3.  It only succeeds once per call, to get
%   another binding for V you have to call it again.

gensym(Prefix, V) :-
	var(V),
	atomic(Prefix),
	(   retract(flag(gensym(Prefix), M))
	|   M = 0
	),
	N is M+1,
	asserta(flag(gensym(Prefix), N)),
	concat(Prefix, N, V),
	!.


%   cgensym(Prefix, V)
%   binds V to a new atom unless it is already bound.  Thus
%   cgensym(a, fred) would succeed, but cgensym(a, X) would bind
%   X to a new atom, maybe a4.  "c" standard for "conditional".

cgensym(Prefix, V) :-
	nonvar(V), !,
	atomic(V),
	atomic(Prefix).
cgensym(Prefix, V) :-
	gensym(Prefix, V).


%   concat(Name1, Name2, Name3)
%   is like append on atoms.  That is, it appends the name of Name1 and
%   the name of Name2, and binds Name3 to the atom named by the result.
%   Unlike append, it will only work one way round.  Examples:
%   concat(a, b, ab), concat(10, 23, 1023), concat(gs, 46, gs46).
%   concat(04, 05, 405)*??*

concat(N1, N2, N3) :-
	name(N1, Ls1),
	name(N2, Ls2),
	append(Ls1, Ls2, Ls3),
	name(N3, Ls3).




