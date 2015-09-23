/*  Program to solve any cryptarithmetic puzzle.

	Luis E. Jenkins
	Dept. of Artificial Intelligence
	University of Edinburgh
	
	October, 1981.
*/



solve(Arg1,Arg2,Resul) :-
     in_form(Arg1,Resul,Lt1,Dlen1),
     in_form(Arg2,Resul,Lt2,Dlen2),
     rev(Resul,Lt3),
     add_parallel(Lt1,Lt2,0,Lt3,[1,2,3,4,5,6,7,8,9,0]),
     out_form(Lt1,Lu1,Dlen1),
     out_form(Lt2,Lu2,Dlen2),
     report(Lu1,Lu2,Resul).


add_parallel([A],[B],Ca,[C],Poss) :-
       add(A,B,Ca,C,0,Poss,Unassig).

add_parallel([Letter1|Rest1],[Letter2|Rest2],Ca,[Letter3|Rest3],Poss) :-
       add(Letter1,Letter2,Ca,Letter3,Cr,Poss,Unassig),
       add_parallel(Rest1,Rest2,Cr,Rest3,Unassig).


add(Letter1,Letter2,Ca,Sres,Cr,Poss,Unassig) :-
   digit(Letter1,Poss,Pt),
   digit(Letter2,Pt,Pu),
   M is Letter1+Letter2+Ca,
   Gres is M mod 10,
   check(Sres,Gres,Pu,Unassig),
   calc_carry(Cr,M).


check(Sres,Gres,P,P) :-
      integer(Sres),
      Gres is Sres.

check(Sres,Gres,Poss,Unassig) :-
     var(Sres),
     select(Gres,Poss,Unassig),
     Sres is Gres.

calc_carry(1,Res) :- Res >= 10.

calc_carry(0,Res) :- Res < 10.


digit(Letter,L,L) :-
     integer(Letter).

digit(Letter,Poss,Unassig) :-
     var(Letter),
     select(Letter,Poss,Unassig).


in_form(Lin,Lref,Lout,Dlen) :-
	 pad(Lin,Lref,Lt,Dlen),
         rev(Lt,Lout).


pad(Lin,Lref,Lout,Dlen) :-
   length(Lin,Nlen),
   length(Lref,Len2),
   Dlen is Len2-Nlen,
   extend(Lin,Lout,Dlen,0).


extend(L,L,0,Char) :- !.

extend(Lin,Lout,N,Char) :-
      append([Char],Lin,Lt),
      M is N-1,
      extend(Lt,Lout,M,Char).


out_form(Lin,Lout,Dlen) :-
	rev(Lin,Lt),
	extend(Lu,Lt,Dlen,0),
	extend(Lu,Lout,Dlen,' ').


report(Arg1,Arg2,Resul) :-
      printlst(Arg1),
      printlst(Arg2),
      length(Resul,N),
      printchr('-',N),
      printlst(Resul).


printlst(List) :-
	nl,
	tab(10),
	putlt(List).


printchr(Char,N) :-
	nl,
	tab(10),
	putch(Char,N).

putlt([]).

putlt([Head|Tail]) :-
	write(Head),
	putlt(Tail).


putch(Char,0) :- !.

putch(Char,N) :-
	write(Char),
	M is N-1,
	putch(Char,M).


/* Examples of use:

solve([C,R,O,S,S],[R,O,A,D,S],[D,A,N,G,E,R]).
	(eg. 'CROSS' + 'ROADS' = 'DANGER')

solve([D,O,N,A,L,D],[G,E,R,A,L,D],[R,O,B,E,R,T]).
	(eg. 'DONALD' + 'GERALD' = 'ROBERT')

*/
