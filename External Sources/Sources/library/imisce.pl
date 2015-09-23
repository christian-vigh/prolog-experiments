/* IMISCE.PL : Miscellaneous routines (interpreted)

						UTILITY
						Lawrence
						Updated: 30 March 81
*/

	%%%  Run this module interpreted
	%%%  IMISCE requires no other modules




  continue.


  \=(X,X) :- !, fail.

  \=(X,Y) :- !.


  casserta(X) :- X, !.

  casserta(X) :- !, asserta(X).


  cassertz(X) :- X, !.

  cassertz(X) :- !, assertz(X).


  clean	:- nolog,
	   seeing(X),
	   see('prolog.log'),
	   rename('prolog.log',[]),
	   seen,
	   see(X),
	   log.


  diff(X,X) :- !, fail.

  diff(X,Y) :- !.


  gcc(X) :- nobt(X), asserta('$gcc'(X)), fail.

  gcc(X) :- retract('$gcc'(Y)), !, X = Y.



  subgoal(exact,L)
	:- numbervars(L,1,N),
	   ( subgoal_of(L) -> fail ; !, fail ).

  subgoal(exact,L).
