%   File   : /usr/lib/prolog/dec10 (OLD VERSION -- RAOK)
%   Author : Paul F. Wilk
%   Purpose: Dec-10 compatibility file for C Prolog v1.4a

%   RAOK: not necessarily suitable for C Prolog v1.5a.

%   This file defines all the Dec-10 evaluable predicates not already
%   part of C Prolog.  It was written by P.F.Wilk.  R.A.O'Keefe
%   rewrote it so that predicates which cannot be emulated appear as
%   !  <goal> and debug or abort, and the rest appear as
%   ;  <goal> and either succeed or fail as appropriate.

ancestors(A) :-
	write('! '), write(ancestors(A)), nl,
	trace, break, abort.

compile(L) :-
	write('; '), write(compile(L)), nl,
	compile1(L).

compile1([]).
compile1([H|T]) :-
	reconsult(H),
	compile1(T).

depth(D) :-
	write('! '), write(depth(D)), nl,
	trace, break, abort.

gc :-
	write('; '), write(gc), nl.

gcguide(A,B,C) :-
	write('; '), write(gcguide(A,B,C)), nl.

incore(X) :-		% SHOULD NEVER APPEAR; USE call/1
	write('; '), write(incore(X)), nl,
	call(X).

log :-
	write('; '), write(log), nl.

maxdepth(D) :-
	write('; '), write(maxdepth(D)), nl.

nogc :-
	write('; '), write(nogc), nl.

nolog :-
	write('; '), write(nolog), nl.

plsys(S) :-
	write('! '), write(plsys(S)), nl,
	trace, break, abort.

reinitialise :-
	write('! '), write(reinitialise), nl,
	trace, break, abort.

restore(F) :-
	write('! '), write(restore(F)), nl,
	fail.

revive(A,B)  :-
	write('! '), write(revive(A,B)), nl,
	trace, break, abort.

statistics(A,B) :-
	write('! '), write(statistics(A,B)), nl,
	trace, break, abort.

subgoal_of(G)  :-
	write('! '), write(subgoal_of(G)), nl,
	trace, break, abort.

trimcore :-
	write('; '), write(trimcore), nl.

version :-
	write('; '), write(version), nl.

version(V) :-
	write('; '), write(version(V)), nl.




