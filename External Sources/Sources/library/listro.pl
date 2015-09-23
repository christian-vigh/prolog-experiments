/* LISTRO.PL : List manipulating routines

						UTILITY
						Lawrence
						Updated: 31 March 81
*/

	%%%  Compile this module
	%%%  LISTRO requires module:  SETROU



  /* EXPORT */

  :- public append/3,
	    disjoint/1,
	    last/2,
	    listtoset/2,
	    nextto/3,
	    numlist/3,
	    perm/2,
	    perm2/4,
	    remove_dups/2,
	    rev/2,
	    select/3,
	    sumlist/2,
	    pairfrom/4.



  /* MODES */

	:- mode append(?,?,?).
	:- mode disjoint(?).
	:- mode last(?,?).
	:- mode listtoset(?,?).
	:- mode nextto(?,?,?).
	:- mode numlist(+,+,?).
	:- mode perm(?,?).
	:- mode perm2(?,?,?,?).
	:- mode remove_dups(?,?).
	:- mode rev(?,?).
	:- mode 	revconc(?,+,?).
	:- mode select(?,?,?).
	:- mode sumlist(+,?).
	:- mode pairfrom(+,?,?,?).




  append([],L,L).

  append([HD|TL],L,[HD|LL]) :- append(TL,L,LL).


  disjoint([]).

  disjoint([HD|TL])
	:- memberchk(HD,TL), !, fail  ;
	   disjoint(TL).


  last(X,[X]) :- !.

  last(X,[HD|TL]) :- last(X,TL).




  listtoset([],[]).

  listtoset([HD|TL],Ans)
	:- member(HD,TL),
	   !,
	   listtoset(TL,Ans).

  listtoset([HD|TL],[HD|Ans]) :- listtoset(TL,Ans).




  nextto(X,Y,[X,Y,..Rest]).

  nextto(X,Y,[HD|TL]) :- nextto(X,Y,TL).



  numlist(N,N,[N]) :- !.

  numlist(N1,N2,[N1|Rest])
	:- N1 < N2,
	   N3 is N1+1,
	   numlist(N3,N2,Rest).



  perm([],[]).

  perm(L,[X|Xs])
	:- select(X,L,R),
	   perm(R,Xs).


  perm2(X,Y,X,Y).

  perm2(X,Y,Y,X).




  remove_dups(A,B) :- listtoset(A,B).



  rev(L1,L2) :- revconc(L1,[],L2).


  revconc([],L,L).

  revconc([X|L1],L2,L3) :- revconc(L1,[X|L2],L3).



  select(X,[X|TL],TL).

  select(X,[Y|TL1],[Y|TL2]) :- select(X,TL1,TL2).


			% Sum a list of integers

sumlist([],0) :- !.

sumlist([Hd|Tl],Sum) :-
	sumlist(Tl,TlSum), Sum is Hd+TlSum, !.



			% Get a pair of elements from a list, also
			%  return the rest. Pairs are only returned
			%  once (not twice different ways round)

pairfrom([X|T],X,Y,R) :- select(Y,T,R).

pairfrom([H|S],X,Y,[H|T]) :- pairfrom(S,X,Y,T).
