%Copyright 1992
%John G. Cleary
%University of Calgary
%cleary@cpsc.ucalgary.ca

%WARNING: machine dependence
xmaxint(2147483647).

%We lie about the minimum representable integer because we want X = --X
%to hold for bounds and a negative number with no positive representation
%fouls this up.  In intdef.pl we hide intgers < xminint from the users
%view and from all the rest of the code in the system
%This is known bug in Sepia that -(very large number) gnerates an expression
%not a constant
%The stuff with -T avoids an error if X is bound to a non-numeric
xminint(X):- T is -xmaxint, X=T. 

xmaxsqint(46340). %largest integer whose square is <= xmaxint

:-mode(xr(++,++,-)).
%This takes the difference of integers - machine dependency because assumes 
%that integer overflow is 2's complement
xr(L,H,R):- integer(L), integer(H), !, T is (H-L)+1, (T>0->R=T;xmaxint(R)).
xr(_,_,R):- xmaxint(R).

%Do multiply checking for overflow
:-mode(mach_mult(++,++,-)).
mach_mult(I,J,K):-
	Kt is I*J,
	%check for overflow
	(Kt<xminint 
         -> over(I,J,K)
         ;  (Jt is Kt // I, (Jt=J -> K=Kt ; over(I,J,K)))
        ).

%used by mach_mult for overflow checking
:-mode(over(++,++,-)).
over(I,J,pinf):- I>0,J>0, !. %Red cut
over(I,J,pinf):- I<0,J<0, !. %Red cut
over(_I,_J,minf).

%Add integers correctly dealing with overflow
:-mode(mach_add(++,++,-)).
mach_add(I,J,K):- 
	J =< 0, !, %integer(I), integer(J), 
        Kt is I+J, 
	((Kt =< I,Kt>=xminint) -> K=Kt ; K= minf).
mach_add(I,J,K):- 
	debug_check((integer(I), integer(J), J>0)),
        Kt is I+J, 
	(Kt>I  -> K=Kt ; K= pinf).
