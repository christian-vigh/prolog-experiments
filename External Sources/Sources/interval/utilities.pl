%Copyright 1992
%John G. Cleary
%University of Calgary
%cleary@cpsc.ucalgary.ca

%Generally useful routines
%Repeatedly call goal until it fails then succeed
exec_all(Goal):- call(Goal), fail.
exec_all(_).

%Print execution time of first success of goal
time(Goal):-
	cputime(A),
	call(Goal),
	cputime(B),
	plus(C,B,A), print('Time:'), print(C).

%Extract all the variables in a term and return them in a list
:-mode(strip(?,-)).
strip(Term,List):- str(Term,List-[]).

str(Term,[Term|E]-E):- var(Term), !.
str(Term,E-E):- atomic(Term), !.
str([Hd|Tl],S-E):- !, str(Hd,S-Temp), str(Tl,Temp-E).
str(Term,S-E):- Term=..[_F|P], str(P,S-E).

