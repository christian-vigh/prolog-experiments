/*----- Synthesis of an exponentiation routine -----*/

:-op(700,xfx,=/=).


X after test(X,U) :- X after_test U.

w:loop(U,V,W) after loop(U,V,W).

R:X1*X2 after mult(R,R1,R2,X1,X2).
R:X1-X2 after subtc(R,R1,X2,X1) :- const(X2).
R:X1/X2 after divc(R,R1,X2,X1) :- const(X2).


zero(X) after_test zero(R,X).
even(X) after_test even(R,X).


test(X,U) needs C :- U needs C.
if(_,_,C,_) needs C.

loop(U,V,W) needs V<v0 & u:U & v:V & w:W.

mult(R,R1,R2,X1,X2) needs R1:X1 & R2:X2.
subtc(R,R1,X2,X1) needs R1:X1.
divc(R,R1,X2,X1) needs R1:X1.

zero(R,X) needs R:X.
even(R,X) needs R:X.


loop(_,_,_) affects u:_.
loop(_,_,_) affects v:_.

U affects R:_ :- R:_ after U.


never \X & X.
never R:X & R:Y & X=/=Y.

always X=/=X :-!,fail.
always X=/=Y.

always X-1<X.
always X/2<X.

const(1).
const(2).

always loop(U,V,W,loop(U,V,W)).

loop(U,V,W,W) if zero(V).
loop(U,V,W,X) if \zero(V) & even(V) & loop(U*U,V/2,W,X).
loop(U,V,W,X) if \zero(V) & \even(V) & loop(U,V-1,U*W,X).

initially u:u0.
initially v:v0.
initially w:w0.

want loop(u0,v0,w0,X) & w:X.

