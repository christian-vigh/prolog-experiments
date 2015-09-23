%--------------------------------------------------------------
%This routine computes all the non-trivial divisors of X and Y of Z.
%Z is implicitly > 1
%To prevent getting negative numbers and to remove the trivial solutions
%X is constrained to be > 1 and Y to be > X
%Try isolve(divisor(X,Y,12)).
divisor(X,Y,Z):- Z is X*Y, 1<X, X=<Y.

%--------------------------------------------------------------
%This computes numbers of the form X^2+Y^2=Z^2
%Try isolve(euclid(X,Y,169)).
euclid(X,Y,Z):- 1=<X, X<Y, 1<Z, Z*Z =:= X*X + Y*Y.

%--------------------------------------------------------------
%X^3+Y^3=Z
%Try isolve(rama(X,Y,1729)).
%Ramanujan is supposed to have mentioned this number on his death bed
rama(X,Y,Z):- Z is X*X*X + Y*Y*Y, 1=<X, X=<Y.

%--------------------------------------------------------------
%Factorial
%Try fact(5,N).
%Try fact(N,120)
%Warning:the recursive call to fact needs to be last to prevent infinite
%recursion on iverse calls such as fact(N,120).
fact(0,1).
fact(1,1).
fact(N,R):- 1<N, N=<R, M is N-1, R is N*fact(M).

%--------------------------------------------------------------
%The following routines deal with complex integers (Gaussian integers)
%cdivisor(X,Y,Z) is analogous to divisor for ordinary integers
%Try isolve(cdivisor(X,Y,c(5,7))).

%Multiplication of complex integers
ctimes(c(Xr,Xi),c(Yr,Yi),c(Zr,Zi)):-
	Zr is Xr*Yr - Xi*Yi,
	Zi is Xi*Yr + Xr*Yi.

%square of modulus
modulus2(c(Xr,Xi),R):- R is Xr*Xr + Xi*Xi.

%Check for non-unit divisors
cdivisor(X,Y,Z):- 
	ctimes(X,Y,Z),
	%The modulii provide redundant (but powerful) addittional constraints
	modulus2(X,MX), modulus2(Y,MY), modulus2(Z,MZ),
	MZ is MX*MY, 1<MX, MX=<MY,
	%Break the 4 way symmetry complex divisors have
	X=c(Xr,Xi), 0<Xr, 0=<Xi.

%--------------------------------------------------------------
%Compute a 3x3 magic square 
%Try magic(L).
val(X):- 1=<X, X=<9.

row(X1,X2,X3):- 15 is X1+X2+X3.

magic_goal(L):-
	L=[X11,X12,X13,X21,X22,X23,X31,X32,X33],
	%Removing symmetries can assume X11=1
	X11=1,
	all_val(L),
	%Break remaining symmetries
	X11<X12, X11<X21, X12<X13, X21<X31, X21<X12,
	all_diff(L),
	row(X11,X12,X13),
	row(X21,X22,X23),
	row(X31,X32,X33),
	row(X11,X21,X31),
	row(X12,X22,X32),
	row(X13,X23,X33).

all_val([]).
all_val([X|L]):- val(X), all_val(L).

all_diff([]).
all_diff([X|L]):- all_diff(X,L), all_diff(L).

delay all_diff(X,_) if var(X).
all_diff(_,[]).
all_diff(X,[Y|L]):- iineq(X,Y), all_diff(X,L).

magic(L):- isolve(L,magic_goal(L)).

%--------------------------------------------------------------
%The following code solves a well known cryptarithmetic puzzle
%     S E N D
%     M O R E
%   M O N E Y

%Wrap the problem in a pretty printer for the results
crypta:- crypta_goal([S,E,N,D,M,O,R,Y]),
	print1([' ',S,E,N,D]),
	print1([' ',M,O,R,E]),
	print1([  M,O,N,E,Y]),
	fail.
crypta.

print1([]):- nl.
print1([X|L]):- write(X), write(' '), print1(L).

%isolve forces solutions (try crypta_solve(L) on its own to see the
%intermediate step reached when this forcing is not done)
crypta_goal(L):- isolve(L,crypta_solve(L)).

%A set of constraints for the problem
crypta_solve(L):- L=[S,E,N,D,M,O,R,Y],
	M>0, S>0, %Leading digits may not be 0
	all_digit(L), %All are digits from 0 to 9
	all_diff(L), %see magic example for this routine
	a(0,D,E,Y,C1), %Add each column with carry digits between
	a(C1,N,R,E,C2),
	a(C2,E,O,N,C3),
	a(C3,S,M,O,M).

all_digit([]).
all_digit([X|L]):- 0=<X, X=<9, all_digit(L).

%Add one column with Ci the carry in and Co the carry out
a(Ci,D1,D2,R,Co):- 0=<Co, Co=<1, R+10*Co =:= Ci+D1+D2.

%----------------------------------------------------------
%This solves a puzzle presented in 
%"Mathematical Recreations" Ian Stewart, Scientific American 266(6) June 1992
%pp122-124
%To find solutions to i/a + 1/b + 1/c ... = 1
%Assume a<b<c<..
%To run it try
%s4(L) or s5(L)

%This first example does things explicitly for four variables
s4(L):- isolve(L,solve4(L)).
solve4([A,B,C,D]):-
	2=<A, 	A =< 4,
	A=<B, TB is B*(A-1), TB =< 3*A, TB > A,
	B=<C, TC is C*(A*B - A - B), TC =< 2*A*B, TC > A*B,
	C=<D, D*(A*B*C - A*B - A*C - B*C) =:= A*B*C,
	A*B*C + A*B*D + A*C*D + B*C*D =:= A*B*C*D. %The basic equation

%This carefully removes common subexpressions inorder to speed convergence
s4_fast(L):- isolve(L,solve4_fast(L)).
solve4_fast([A,B,C,D]):-
	2=<A, 	A =< 4,
	A=<B, TB is B*(A-1), TB =< 3*A, TB > A,
	B=<C, TC is C*(TB - A), TAB is A*B, TC =< 2*TAB, TC > TAB,
%       C=<D, D*(A*B*C - A*B - A*C - B*C) =:= A*B*C,
%             D*(C*(A*B-A-B) - A*B)
%                C*(B*(A-1)-A)
	C=<D, D*(TC - TAB) =:= TABC, TABC is TAB*C,
	D*(TAB + C*(A + B)) =:= TABC*(D-1). %The basic equation

%Five variables
s5(L):- isolve(L,solve5(L)).
solve5([A,B,C,D,E]):-
	2=<A, A=<B, B=<C, C=<D, D=<E,
	A =< 5,
	B =< 4*A/(A-1),
	C =< 3*A*B/(A*B - A - B),
	D =< 2*A*B*C/(A*B*C - A*B - A*C - B*C),
	E is A*B*C*D/(A*B*C*D - A*B*C - A*B*D - A*C*D - B*C*D),
        %The basic equation
	A*B*C*D + A*B*C*E + A*B*D*E + A*C*D*E + B*C*D*E =:= A*B*C*D*E.


