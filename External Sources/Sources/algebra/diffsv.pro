/* -----------------------------------------------------------------

                              DIFFSV.PRO

                             (version 1.0)

                        Copyright 1987 S.Vaghi


              Program for the symbolic differentiation of
                           algebraic functions.

              ...........................................


     Example of how to use the program:

         to find the derivative, DY, of the function
                                                       y = x^2
         with respect to x, one can

                 a) simply enter
                                                d( x^2, x, DY).
                    after the Prolog prompt,
         or
                 b) enter
                                                 Y = x^2 ,
                                                 d( Y, x, DY).
                    after the prompt,
         or
                 c) enter the complete sequence including
                    simplification, that is
                                                  Y = x^2 ,
                                                  d( Y, x, Z),
                                                  s( Z, DY).

         Method c) is always recommended, in which case the
         program is used in combination with SIMPSV.PRO


   ----------------------------------------------------------------- */



                  /*  definition of operators */


?-op(11, yfx,  '^').                 /*  exponentiation     */
?-op( 9,  fx,  '~').                 /*  minus sign         */
?-op(11,  fx, 'ln').                 /*  natural logarithm  */



d(X,X,1).

d(C,X,0) :- atomic(C), C \= X.

d(~U,X,~D) :- d(U,X,D).


	d(C+U,X,D) :- atomic(C), C \= X, d(U,X,D), ! .
	d(U+C,X,D) :- atomic(C), C \= X, d(U,X,D), ! .

d(U+V,X,D1+D2) :- d(U,X,D1), d(V,X,D2).


	d(C-U,X,~D) :- atomic(C), C \= X, d(U,X,D), ! .
	d(U-C,X, D) :- atomic(C), C \= X, d(U,X,D), ! .

d(U-V,X,D1-D2) :- d(U,X,D1), d(V,X,D2).


	d(C*U,X,C*D) :- atomic(C), C \= X, d(U,X,D), ! .
	d(U*C,X,C*D) :- atomic(C), C \= X, d(U,X,D), ! .

d(U*V, X, D2*U+D1*V) :- d(U,X,D1), d(V,X,D2).


	d(1/U,X, ~D/(U^2) ) :- d(U,X,D), ! .
	d(C/U,X, C*DD) :- atomic(C), C \= X, d(1/U,X,DD), ! .
	d(U/C,X,  D/C) :- atomic(C), C \= X, d(U,X,D), ! .

d(U/V, X, (V*D1-U*D2)/(V^2) ) :- d(U,X,D1), d(V,X,D2).


	d( U^C, X, C*D*U^(C-1) ) :- atomic(C), C \= X, d(U,X,D), ! .
        d(U^~C, X,            Z) :-  d( 1/(U^C), X, Z), ! .
        d( U^(A/B),X, (A/B)*D*U^(A/B-1) ) :- atomic(A), atomic(B),
                                             A \= X, B \= X,
                                             d(U,X,D), ! .
        d(U^~(A/B),X,                  Z) :- d(1/U^(A/B) , X, Z), ! .

d(U^V, X, U^V*(V*D1/U+D2*ln(U) )) :- d(U,X,D1), d(V,X,D2).



