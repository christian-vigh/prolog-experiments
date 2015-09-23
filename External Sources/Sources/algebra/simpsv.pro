/* -------------------------------------------------------------------

                               SIMPSV.PRO

                              (version 1.0)

                          Copyright 1987 S.Vaghi


                Program for the symbolic simplification of
                            algebraic functions.

                ..........................................


	Example of how to use the program:

	    to simplify the expression
                                             (2*1)*(x^(2-1))
            one can

                a) simply enter
                                             s( (2*1)*(x^(2-1)), Z).
                   after the Prolog prompt,
            or
                b) enter
                                              Y = (2*1)*(x^(2-1)),
                                              s( Y, Z).
                   after the prompt.

            In both cases a two pass simplification is performed.


-------------------------------------------------------------------- */



                 /*  definition of operators */

?-op(11, yfx,  '^').                 /*  exponentiation     */
?-op( 9,  fx,  '~').                 /*  minus sign         */
?-op(11,  fx, 'ln').                 /*  natural logarithm  */



             /*  two pass simplification clause  */


             s(X,Y) :- simplify(X,Z), simplify(Z,Y).



      /*  list processing of the expression to be simplified  */


simplify(X,X) :- atomic(X), ! .

simplify(X,Y) :- X =..[Op,Z], simplify(Z,Z1), u(Op,Z1,Y), ! .

simplify(X,Y) :- X =..[Op,Z,W], simplify(Z,Z1),
                 simplify(W,W1),
                 r(Op,Z1,W1,Y), ! .



      /*  simplification clauses for addition  */


r('+',~X,~X,Z) :- b('*',2,X,W), u('~',W,Z) , ! .
r('+', X, X,Z) :- b('*',2,X,Z), ! .

r('+', X,~Y,Z) :- b('-',X,Y,Z), ! .
r('+',~X, Y,Z) :- b('-',Y,X,Z), ! .
r('+',~X,~Y,Z) :- b('+',X,Y,W), u('~',W,Z), ! .

r('+',   X, Y/Z, W) :- integer(X), integer(Y), integer(Z),
                       T is Z*X+Y,
                       b('/',T,Z,W), ! .
r('+', X/Z,   Y, W) :- integer(X), integer(Y), integer(Z),
                       T is X+Y*Z,
                       b('/',T,Z,W), ! .

r('+',   X, Y+Z, W) :- b('+',Y,Z,T), b('+',X,T,W), ! .
r('+', X+Y,   Z, W) :- b('+',X,Y,T), b('+',T,Z,W), ! .

r('+', X*Y,Z*Y,W) :- b('+',X,Z,T), b('*',Y,T,W), ! .
r('+', X*Y,Y*Z,W) :- b('+',X,Z,T), b('*',Y,T,W), ! .
r('+', Y*X,Z*Y,W) :- b('+',X,Z,T), b('*',Y,T,W), ! .
r('+', Y*X,Y*Z,W) :- b('+',X,Z,T), b('*',Y,T,W), ! .

r('+',X,Y,Z) :- integer(Y), b('+',Y,X,Z), ! .



      /*  simplification clauses for subtraction  */


r('-', X,~X,Z) :- b('*',2,X,Z), ! .
r('-',~X, X,Z) :- b('*',2,X,W), u('~',W,Z), ! .

r('-', X,~Y,Z) :- b('+',X,Y,Z), ! .
r('-',~X, Y,Z) :- b('+',X,Y,W), u('~',W,Z), ! .
r('-',~X,~Y,Z) :- b('-',Y,X,Z), ! .

r('-',   X, Y/Z, W) :- integer(X), integer(Y), integer(Z),
                       T is X*Z-Y,
                       b('/',T,Z,W), ! .
r('-', X/Z,   Y, W) :- integer(X), integer(Y), integer(Z),
                       T is X-Y*Z,
                       b('/',T,Z,W), ! .

r('-',   X, Y-Z, W) :- b('-',Y,Z,T), b('-',X,T,W), ! .
r('-', X-Y,   Z, W) :- b('-',X,Y,T), b('-',T,Z,W), ! .

r('-', X*Y, Z*Y, W) :- b('-',X,Z,T), b('*',Y,T,W), ! .
r('-', X*Y, Y*Z, W) :- b('-',X,Z,T), b('*',Y,T,W), ! .
r('-', Y*X, Z*Y, W) :- b('-',X,Z,T), b('*',Y,T,W), ! .
r('-', Y*X, Y*Z, W) :- b('-',X,Z,T), b('*',Y,T,W), ! .



      /*  simplification clauses for multiplication  */


r('*', X, X,Z) :- b('^',X,2,Z), ! .
r('*',~X,~X,Z) :- b('^',X,2,Z), ! .
r('*',~X, X,Z) :- b('^',X,2,W), u('~',W,Z), ! .
r('*', X,~X,Z) :- b('^',X,2,W), u('~',W,Z), ! .

r('*',      X, X^(~1), Z) :- b('/',X,X,Z), ! .
r('*', X^(~1),      X, Z) :- b('/',X,X,Z), ! .

r('*',   X, 1/X, Z) :- b('/',X,X,Z), ! .
r('*', 1/X,   X, Z) :- b('/',X,X,Z), ! .
r('*',   X, 1/Y, Z) :- b('/',X,Y,Z), ! .
r('*', 1/X,   Y, Z) :- b('/',Y,X,Z), ! .
r('*',   M, N/X, Z) :- atomic(M), atomic(N),
                       b('*',M,N,S), b('/',S,X,Z), ! .
r('*', M/X,   N, Z) :- atomic(M), atomic(N),
                       b('*',M,N,S), b('/',S,X,Z), ! .


r('*',  X, N/Y, Z) :- atomic(N), b('/',X,Y,S), b('*',N,S,Z), ! .
r('*',N/Y,   X, Z) :- atomic(N), b('/',X,Y,S), b('*',N,S,Z), ! .

r('*',     X,Y^(~1),Z) :- b('/',X,Y,Z), ! .
r('*',     X,  X^~Y,Z) :- b('-',Y,1,S), b('^',X,S,T), b('/',1,T,Z), ! .
r('*',X^(~1),     Y,Z) :- b('/',Y,X,Z), ! .
r('*',  X^~Y,     X,Z) :- b('-',Y,1,S), b('^',X,S,T), b('/',1,T,Z), ! .

r('*',  X,X^Y,Z) :- b('+',1,Y,S), b('^',X,S,Z), ! .
r('*',X^Y,  X,Z) :- b('+',Y,1,S), b('^',X,S,Z), ! .

r('*',~X,~Y,Z) :- b('*',X,Y,Z), ! .
r('*', X,~Y,Z) :- b('*',X,Y,W), u('~',W,Z), ! .
r('*',~X, Y,Z) :- b('*',X,Y,W), u('~',W,Z), ! .

r('*',Z^~X,Z^~Y,W) :- b('+',X,Y,S), b('^',Z,S,T), b('/',1,T,W), ! .
r('*',Z^~X, Z^Y,W) :- b('-',Y,X,S), b('^',Z,S,W), ! .
r('*', Z^X,Z^~Y,W) :- b('-',X,Y,S), b('^',Z,S,W), ! .
r('*', Z^X, Z^Y,W) :- b('+',X,Y,T), b('^',Z,T,W), ! .
r('*',X^~Z,Y^~Z,W) :- b('*',X,Y,S), b('^',S,Z,T), b('/',1,T,W), ! .
r('*', X^Z,Y^~Z,W) :- b('/',X,Y,S), b('^',S,Z,W), ! .
r('*',X^~Z, Y^Z,W) :- b('/',Y,X,S), b('^',S,Z,W), ! .
r('*', X^Z, Y^Z,W) :- b('*',X,Y,T), b('^',T,Z,W), ! .

r('*', X*Y,   Y, Z) :- b('^',Y,2,S), b('*',X,S,Z), ! .
r('*', Y*X,   Y, Z) :- b('^',Y,2,S), b('*',X,S,Z), ! .
r('*',   Y, X*Y, Z) :- b('^',Y,2,S), b('*',X,S,Z), ! .
r('*',   Y, Y*X, Z) :- b('^',Y,2,S), b('*',X,S,Z), ! .

r('*', X*Y, X*Z, W) :- b('*',Y,Z,S), b('^',X,2,T), b('*',T,S,W), ! .
r('*', Y*X, X*Z, W) :- b('*',Y,Z,S), b('^',X,2,T), b('*',T,S,W), ! .
r('*', X*Y, Z*X, W) :- b('*',Y,Z,S), b('^',X,2,T), b('*',T,S,W), ! .
r('*', Y*X, Z*X, W) :- b('*',Y,Z,S), b('^',X,2,T), b('*',T,S,W), ! .

r('*',  M, N*X, W) :- atomic(M), atomic(N),
                      b('*',M,N,P), b('*',P,X,W), ! .
r('*',M*X,   N, W) :- atomic(M), atomic(N),
                      b('*',M,N,P), b('*',P,X,W), ! .

r('*',   X, Y*Z, W) :- b('*',Y,Z,T), b('*',X,T,W), ! .
r('*', X*Y,   Z, W) :- b('*',X,Y,T), b('*',T,Z,W), ! .

r('*',X,Y,Z) :- integer(Y), b('*',Y,X,Z), ! .



      /*    simplification clauses for division
           (division is never actually performed)  */


r('/', 1, X/Y, Z) :- b('/',Y,X,Z), ! .
r('/',~1, X/Y, Z) :- b('/',Y,X,W), u('~',W,Z), ! .

r('/',~X,~Y,Z) :- b('/',X,Y,Z), ! .
r('/', X,~Y,Z) :- b('/',X,Y,W), u('~',W,Z), ! .
r('/',~X, Y,Z) :- b('/',X,Y,W), u('~',W,Z), ! .

r('/',      X, Y^(~1), Z) :- b('*',X,Y,Z), ! .
r('/', X^(~1),      Y, Z) :- b('*',X,Y,W), b('/',1,W,Z), ! .

r('/',   X, Y/Z, W) :- b('*',X,Z,T), b('/',T,Y,W), ! .
r('/', X/Y,   Z, W) :- b('*',Y,Z,T), b('/',X,T,W), ! .

r('/',     X,Y^(~Z),W) :- b('^',Y,Z,T), b('*',X,T,W), ! .
r('/',X^(~Z),     Y,W) :- b('^',X,Z,S), b('*',S,Y,T), b('/',1,T,W), ! .

r('/',X,X^(~Y),Z) :- b('+',1,Y,S), b('^',X,S,Z), ! .
r('/',X,   X^Y,Z) :- b('-',Y,1,S), b('^',X,S,T), b('/',1,T,Z), ! .
r('/',X^(~Y),X,Z) :- b('+',Y,1,S), b('^',X,S,T), b('/',1,T,Z), ! .

r('/',   X^Y,     X,Z) :- b('-',Y,1,S), b('^',X,S,Z), ! .
r('/',   X^N,X^(~M),Z) :- b('+',N,M,S), b('^',X,S,Z), ! .
r('/',X^(~N),   X^M,Z) :- b('+',N,M,S), b('^',X,S,T), b('/',1,T,Z), ! .
r('/',X^(~N),X^(~M),Z) :- b('-',M,N,S), b('^',X,S,Z), ! .
r('/',   X^N,   X^M,Z) :- b('-',N,M,W), b('^',X,W,Z), ! .

r('/',X^(~Z),   Y^Z,W) :- b('*',X,Y,S), b('^',S,Z,T), b('/',1,T,W), ! .
r('/',   X^Z,Y^(~Z),W) :- b('*',X,Y,S), b('^',S,Z,W), ! .
r('/',X^(~Z),Y^(~Z),W) :- b('/',Y,X,S), b('^',S,Z,W), ! .
r('/',   X^Z,   Y^Z,W) :- b('/',X,Y,T), b('^',T,Z,W), ! .



      /*  simplification clauses for exponentiation  */


r('^',X,~1,Y) :- b('/',1,X,Y), ! .

r('^',X,~Y,Z) :- b('^',X,Y,W), b('/',1,W,Z), ! .

r('^',X^Y,Z,W) :- b('*',Y,Z,T), b('^',X,T,W), ! .



      /*  catch all clause to cover cases not covered
          by the previous clauses                      */


r(X,Y,Z,W) :- b(X,Y,Z,W).



      /*  basic rules for the unary operator '~'  */


u('~', 0, 0) :- ! .
u('~',~X, X) :- ! .
u('~', X,~X) :- ! .
u('~',X^Y,~(X^Y) ) :- ! .



      /*  basic rules of addition  */


b('+', X, 0, X) :- ! .
b('+',~X, 0,~X) :- ! .
b('+', 0, X, X) :- ! .
b('+', 0,~X,~X) :- ! .
b('+', X,~X, 0) :- ! .
b('+',~X, X, 0) :- ! .

b('+',X,Y,Z) :- integer(X), integer(Y),
                Z is X+Y, ! .

b('+',X,Y,X+Y).


      /*  basic rules of subtraction  */


b('-', X, 0, X) :- ! .
b('-',~X, 0,~X) :- ! .
b('-', 0, X,~X) :- ! .
b('-', 0,~X, X) :- ! .
b('-',~X,~X, 0) :- ! .
b('-', X, X, 0) :- ! .

b('-',X,Y,Z) :- integer(X), integer(Y),
                Z is X-Y, ! .

b('-',X,Y,X-Y).



      /*  basic rules of multiplication  */


b('*', 0, X,0) :- ! .
b('*', 0,~X,0) :- ! .
b('*', X, 0,0) :- ! .
b('*',~X, 0,0) :- ! .

b('*', 1, X, X) :- ! .
b('*', 1,~X,~X) :- ! .
b('*',~1, X,~X) :- ! .
b('*',~1,~X, X) :- ! .
b('*', X, 1, X) :- ! .
b('*',~X, 1,~X) :- ! .
b('*', X,~1,~X) :- ! .
b('*',~X,~1, X) :- ! .

b('*', X/Y,   Y, X) :- ! .
b('*',   Y, X/Y, X) :- ! .

b('*',X,Y,Z) :- integer(X), integer(Y),
                Z is X*Y, ! .

b('*',X,Y,X*Y).



      /*  basic rules of division  */


b('/',0,0,'ERROR - indefinite form 0/0') :- ! .   /* indefinite form */
b('/',X,0,'ERROR - division by 0      ') :- ! .   /* division by 0   */

b('/',0, X,0) :- ! .
b('/',0,~X,0) :- ! .

b('/', X,1, X) :- ! .
b('/',~X,1,~X) :- ! .

b('/', 1,X,    1/X) :- ! .
b('/',~1,X, ~(1/X)) :- ! .

b('/', X,~1,~X) :- ! .
b('/',~X,~1, X) :- ! .

b('/', 1,  ~X,~(1/X)) :- ! .
b('/',~1,  ~X,   1/X) :- ! .
b('/', 1, 1/X,     X) :- ! .
b('/',~1, 1/X,    ~X) :- ! .

b('/', X,~X,~1) :- ! .
b('/',~X, X,~1) :- ! .
b('/',~X,~X, 1) :- ! .
b('/', X, X, 1) :- ! .

b('/',X,Y,X/Y).



      /*  basic rules of exponentiation  */


b('^',1,X,1) :- ! .

                                                  /* indefinite forms */

b('^',0, 0,'ERROR - indefinite form 0^0       ') :- ! .
b('^',0,~1,'ERROR - indefinite form 0^~1 = 1/0') :- ! .
b('^',0,~K,'ERROR - indefinite form 0^~K = 1/0') :- atomic(K), ! .


b('^',~X,1,~X) :- ! .
b('^', X,1, X) :- ! .
b('^', X,0, 1) :- ! .


                                        /* recursive clauses to
                                           calculate the n-th power
                                           of positive and negative
                                           integers                 */

b('^', X,N,Y) :- integer(X), integer(N),
                 M is N-1, b('^',X,M,Z),
                 Y is Z*X, ! .
b('^',~X,N,Y) :- integer(X), integer(N),
                 R is (N mod 2), R \= 0,
                 b('^',X,N,Z), Y = ~Z, ! .
b('^',~X,N,Y) :- integer(X), integer(Y),
                 R is (N mod 2), R = 0,
                 b('^',X,N,Z), Y =  Z, ! .

b('^',~X,Y, ~(X^Y)) :- ! .

b('^',X,Y,X^Y).


