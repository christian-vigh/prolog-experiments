%   File   : DISTFI.EX
%   Author : R.A.O'Keefe
%   Updated: 10 May 1984
%   Purpose: Load Util:Distfix.Pl and define some examples.

:- compile(['util:rdtok.pl', 'util:distfi.pl']).

:- distfixop(850, fx, [append,A,to,Z,giving,L], append(A,Z,L)),
   distfixop(850, fx, [remove,A,from,L,giving,Z], append(A,Z,L)),
   distfixop(700, xfy, [S,is,the,set,of,X,such,that,P], setof(X,P,S)),
   distfixop(700, xfy, [B,is,the,bag,of,X,such,that,P], bagof(X,P,B)),
   distfixop(850, fx, [apply,P,to,Args], apply(P,Args)),
   distfixop(850, fx, [compare,X,with,Y,giving,R], compare(R,X,Y)),
   distfixop(850, fx, [the,principal,functor,of,T,is,F,with,arity,N],
				functor(T,F,N)),
   distfixop(850, fx, [number,the,variables,of,X,starting,from,S,
			up,to,N], numbervars(X,S,N)),
   distfixop(850, fx, [make,X,ground], numbervars(X,0,_)),
   distfixop(850, fx, [unify,X,with,Y], X = Y),
   distfixop(700, xfx, [X,unifies,with,Y], X = Y),
   distfixop(700, xfx, [X,does,not,unify,with,Y], \=(X,Y)),
   distfixop(850, fx, [select,Elem,from,List,leaving,Rest],
			select(Elem,List,Rest)),
   distfixop(999, fy, [if,Test,then,True,else,False], (Test->True;False)),
   distfixop(999, fy, [if,Test,then,True], (Test->True;true)),
   distfixop(850, yfx, [X,for,all,Y], forall(Y,X)).


dconsult(File) :-
	(File == user ; exists(File)),
	seeing(Old),
	see(File),
	repeat,
	    read(Foo, Vars),
	    expand_term(Foo, Baz),
	    dconsult(Baz, Vars),
	!,
	seen,
	see(Old).

dconsult(end_of_file, _) :- !.
dconsult(:-(Cmd), _) :-
	(call(Cmd) ; ttyput(63), ttynl),
	!, fail.
dconsult(?-(Ques), []) :-
	(call(Ques), display(yes) ; display(no)),
	!, ttynl, fail.
dconsult(?-(Ques), Vars) :-
	(call(Ques), dvars(Vars) ; display(no), ttynl),
	!, fail.
dconsult(:-(H,B), _) :-
	assertz(:-(H,B)),
	!, fail.
dconsult(Ques, Vars) :-
	seeing(user), !,
	dconsult(?-(Ques), Vars).
dconsult(H, _) :-
	assertz(H),
	!, fail.

dvars([V=T|Vars]) :-
	display(V), display(' = '), print(T), nl, !,
	dvars(Vars).
dvars([]) :-
	display('more (y/n)? '), ttyflush,
	get0(C), ttyskip(31),
	C\/32 =\= "y".



