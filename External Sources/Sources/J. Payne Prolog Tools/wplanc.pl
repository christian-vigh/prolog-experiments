/* WARPLAN-C CONDITIONAL PLAN GENERATOR */


plans:-plan_from(start,_).

plan_from(T0,A):-
   want C,
   consistent(C,A),
   plan(C,void,T0,P,T,A,[]),
   other_cases(T,P,A,exit).

plan(X1&X2,P0,T0,P,T,A,G):-!,
   plan(X1,P0,T0,P1,T1,A,G),
   plan(X2,P1,T1,P,T,A,G).
plan(X,P0,T0,P,T0,A,G):-
   (always X,P=P0; holds(X,T0),setadd(X,P0,P)).
plan(X,_,_,_,_,A,_):-minimality_violation(X,A),!,fail.
plan(X,_,T0,_,_,_,G):-recorded(loopcheck,Type,_),
   (Type=weak,mkground(X,0,_);Type=strong),
   listel(X:T0,G),!,fail.
plan(X,P0,T0,P,T,A,G):-
  (X after U,U needs C,consistent(C,A),
      inform(try(X,U,T0)),
      extra_assumptions(U,A),
      achieve(X,U,P0,T0,T,A,[X:T0,..G]),
      inform(got(X,U,T)),
      P=(X&P0);
   X if C,
      consistent(C,P0&A),
      plan(C,P0,T0,P,T,A,[X:T0,..G])).

achieve(X,U,P0,T0,(T;U),A,G):-
   preserved(P0,U),
   U needs C,
   consistent(C,P0&A),
   plan(C,P0,T0,P,T,A,G),
   preserved(P0,U).
achieve(X,U,P0,(T0;V),(T;V),A,G):-
   preserved(X,V),
   retrace(P0,V,P),
   achieve(X,U,P,T0,T,A,G),
   preserved(X,V).

holds(\X,(_;if(X,_,_,_))).
holds(X,(T;V)):-
   (X after V; preserved(X,V),holds(X,T),preserved(X,V)).
holds(X,start):-initially X.

preserved(X&Y,V):-!,preserved(X,V),preserved(Y,V).
preserved(X,V):-mkground(X&V,0,_),V affects X,!,fail.
preserved(X,V).

extra_assumptions(test(X,U),A):-!,mkmember(X,A).
extra_assumptions(_,_).

minimality_violation(X,A):-member(X,A).
minimality_violation(\X,A):-!,member(X,A).
minimality_violation(X,A):-member(\X,A).

other_cases((T;test(X,V)),P,A,W):-!,
   negate(X,A,A1),
   retrace(P,test(X,V),P1),
   plan_from((T;if(X,V,P1,W)),A1).
other_cases((T;V),P,A,W):-
   retrace(P,V,P1),
   other_cases(T,P1,A,(V;W)).
other_cases(start,_,_,W):-
   write('----------'),nl,
   write_plan(W,0),
   reply,fail.

retrace(P,V,C&P1):-V needs C,retrace1(P,V,C,P1).

retrace1(X1&X2,V,C,Y):-!,
   retrace1(X1,V,C,Y1),
   retrace1(X2,V,C,Y2),
   union(Y1,Y2,Y).
retrace1(X,V,_,void):-X1 after V,X==X1,!.
retrace1(\X,if(X1,_,_,_),_,void):-X==X1,!.
retrace1(X,_,C,void):-member(X1,C),X==X1,!.
retrace1(X,_,_,X).

consistent(C,P):-
   mkground(C&P,0,_),
   never S,
   subset(S,C&P),!,fail.
consistent(_,_).

listel(X,[X,.._]).
listel(X,[_,..L]):-listel(X,L).

union(void,X,X):-!.
union(X,void,X):-!.
union(X,Y,X&Y).

setadd(X,S,S):-member(X1,S),X==X1,!.
setadd(X,S,X&S).

member(X,S):-var(S),!,fail.
member(X,S1&S2):-!,(member(X,S1); member(X,S2)).
member(X,X).

mkmember(X,X&S):-!.
mkmember(X,Y&S):-mkmember(X,S).

subset(S1&S2,S):-!,subset(S1,S),subset(S2,S).
subset(X,S):-member(X,S).
subset(X,_):-always X.

negate(_,Y,_):-var(Y),!,fail.
negate(X,Y&Z,Y1&Z):-negate(X,Y,Y1),!.
negate(X,Y&Z,Y&Z1):-!,negate(X,Z,Z1),!.
negate(X,X1,\X):-X==X1.

mkground('$VAR'(N1),N1,N2):-!,N2 is N1+1.
mkground('$VAR'(N),N1,N1):-!.
mkground(X,N1,N2):-X=..[F,..A],mkgroundlist(A,N1,N2).

mkgroundlist([X,..A],N1,N3):-
   mkground(X,N1,N2),
   mkgroundlist(A,N2,N3).
mkgroundlist([],N1,N1).

inform(X):-debugon,!,write(X),nl,reply.
inform(X).

debugon :-
   recorded(debug,on,_).

ondebug :-
 ( recorded(debug,_,P), !, erase(P); true),
   recorda(debug,on,_).

offdebug :-
 ( recorded(debug,_,P), !, erase(P); true),
   recorda(debug,off,_).

reply :-
   repeat,
      ttyget0(C),
      obey(C), !.

obey(10):-!, ttyput(13).
obey(27):-!, offdebug.
obey(7) :- abort.

write_plan((if(_,U,_,W);W1),N):-!,
   tab(N),write(U),nl,
   N1 is N+1,
   write_plan(W,N1),
   write_plan(W1,N).
write_plan((U;W),N):-
   tab(N),write(U),nl,
   write_plan(W,N).
write_plan(exit,N):-
   tab(N),write(exit),nl.



