/* INVOCA.PL : Invocation routines

						UTILITY
						Lawrence
						Updated: 31 March 81
*/

	%%%  Run this module interpreted
	%%%  INVOCA requires no other modules



  &(A,B) :- !, A, B.


  \\(A,B) :- A, ! ; B, !.



  any([]) :- !, fail.

  any([L|Rest]) :- L.

  any([L|Rest]) :- !, any(Rest).



  binding(N,L)
	:- asserta('$bind'(N)),
	   N > 0,
	   L,
	   retr(N2),
	   N3 is N2-1,
	   ( N3 =< 0  ;  asserta('$bind'(N3)), fail ),
	   !.

  binding(_,_,_)
	:- retr(_),
	   fail.


  retr(N) :- retract('$bind'(N)), !.



			% Findall X's such that P. This is a funny version
			%  which finds the FIRST solution that bagof would
			%  find, but returns the empty list if there are no
			%  solutions. (Logically dubious).

findall(X,P,List) :- bagof(X,P,List), !.

findall(X,P,[]).



  for(0,L) :- !.

  for(N,L) :- N > 0,
	      L,
	      N2 is N-1,
	      for(N2,L),
	      !.



  forall(A,C)
	:- A,
	   not(C),
	   !,
	   fail.

  forall(_,_) :- !.



  nobt(X) :- X, !.



  not(X) :- X, !, fail.

  not(X) :- !.



  thnot(X) :- X, !, fail.

  thnot(X) :- !.
