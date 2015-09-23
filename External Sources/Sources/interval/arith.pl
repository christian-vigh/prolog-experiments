%Copyright 1992
%John G. Cleary
%University of Calgary
%cleary@cpsc.ucalgary.ca

%Redefine the standard Sepia arithmetic to use the integer interval package

:- local is/2.
A is B :- A iis B.

:-local '>'/2.
A > B :- RA iis A, RB iis B, igt(RA,RB).

:-local '<'/2.
A < B :- RA iis A, RB iis B, ilt(RA,RB).

:-local '>='/2.
A >= B :- RA iis A, RB iis B, igeq(RA,RB).

:-local '=<'/2.
A =< B :- RA iis A, RB iis B, ileq(RA,RB).

:-local '=:='/2.
A =:= B :- R iis A, R iis B.

:- local (=\=)/2.
A =\= B :- iineq(RA,RB), RA iis A, RB iis B.


