%   File   : GENSYM.PL
%   Author : Lawrence Byrd?
%   Updated: 12 February 1985
%   Purpose: create new atoms



%   gensym(Prefix, V)
%   binds V to a new atom whose name begins with Prefix and ends with a
%   number.  E.g. gensym(a,X), gensym(a,Y), gensym(a,Z) might bind
%   X to a1, Y to a2, Z to a3.  It only succeeds once per call, to get
%   another binding for V you have to call it again.

gensym(Prefix, V) :-
	var(V),
	atomic(Prefix),
	(   recorded(gensym,[Prefix,M],K),
	    erase(K),
	    N is M + 1
	;
	   N = 1
	),
	record(gensym,[Prefix,N],_),
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
	gensym_append(Ls1, Ls2, Ls3),
	name(N3, Ls3).

%
%  This procedure will retract all occurances
%  of the flags set up by gensym. i.e. "flag(gensym(_),_)".
%

reset_gensym :-

	recorded(gensym,_,Key),
	erase(Key),
	reset_gensym.

reset_gensym:-!.


%
%  This procedure retracts the flag set up by
%  gensym for a particular prefix.
%

reset_gensym(Sym) :-

	recorded(gensym,[Sym,_],Key),
	erase(Key).

reset_gensym(_):-
	!.

gensym_append([],X,X).

gensym_append([H|T],U,[H|V]) :-
	gensym_append(T,U,V).
