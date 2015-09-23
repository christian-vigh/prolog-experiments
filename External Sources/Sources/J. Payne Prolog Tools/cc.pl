%   File   : CC.PL
%   Author : R.A.O'Keefe
%   Updated: 25 August 1984
%   Purpose: "Conditional Compilation".

/*  The purpose of this file is to provide a "conditional compilation"
    feature in Prolog somewhat like #+ and #- in Mac/Franz/Zeta Lisp.
    The idea is that if you have something which must be different for
    Dec-10 Prolog, C Prolog, Prolog-X, &c, or perhaps for different
    operating systems, you can put
	#+ dialect(dec10), os(tops10).
	-- Dec-10 Prolog Tops-10 version- .
	#+ dialect(c), os(unix).
	-- C Prolog UNIX version- .
	#+ dialect(c), os(vms).
	-- C Prolog VMS version- .
    And so on.  (#+ features.) will throw away the next term if the
    test fails.  (n #+ features.) will throw away the next n terms.
    #- x is the same as #+ \+ (X).  This wants to be built into the
    reader, so that one doesn't have to write :- #+ ...
*/

:- public
	(#+)/1,		%    #+ features. clause.
	(#+)/2,		%  n #+ features. c1. ... cn.
	(#-)/1,		%    #- features. clause.
	(#-)/2,		%  n #- features. c1. ... cn.
	dialect/1,
	os/1.

:- op(1199,  fx, [(#+),(#-)]).
:- op(1199, xfx, [(#+),(#-)]).

#+ Test :-
	1 #+ Test.

N #+ Test :-
	call(Test),
	!.
N #+ Test :-
	skip_read(N).


#- Test :-
	1 #- Test.

N #- Test :-
	call(Test),
	!,
	skip_read(N).
N #- Test.


skip_read(0) :- !.
skip_read(N) :-
	read(_),
	M is N-1,
	skip_read(M).


dialect(dec10).
os(tops10).







