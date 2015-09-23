%------------------------------------------------------------------%
%  Translation of First-Order formulae into clauses                %
%  (C) 1998 Zdravko Markov                                         %
%------------------------------------------------------------------%
% negation:    ~                                                   %
% disjunction: #                                                   %
% conjunction: &                                                   %
% implication: ->                                                  %
% Quatifiers:                                                      %
%   Universal:   all(Variable, Formula)                            %
%   Existential: exists(Variable, Formula)                         %
%------------------------------------------------------------------%
%  Call: translate(+Formula, -Clauses).                            %
%  Example:                                                        %
%  ?- translate(all(X,exists(Y,(man(X)->woman(Y)&loves(X,Y)))),C). %
%  C = [cl([woman(s1(X))],[man(X)]),cl([loves(X,s1(X))],[man(X)])] %
%------------------------------------------------------------------%

:- op(850,fx,~).    % negation
:- op(900,xfy,#).   % disjunction
:- op(900,xfy,&).   % conjunction
:- op(950,xfy,->).  % implication

translate(FOL,Clauses) :-
    implout(FOL,X2),
    negin(X2,X3),
    skolem(X3,X4,[]),
    univout(X4,X5),
    conjn(X5,X6),
    clausify(X6,Clauses,[]).

% Removing implications

implout(P->Q,~P1#Q1) :-
    !, implout(P,P1),
    implout(Q,Q1).
implout(all(X,P),all(X,P1)) :-
    !, implout(P,P1).
implout(exists(X,P),exists(X,P1)) :-
    !, implout(P,P1).
implout(P&Q,P1&Q1) :-
    !, implout(P,P1),
    implout(Q,Q1).
implout(P#Q,P1#Q1) :-
    !, implout(P,P1),
    implout(Q,Q1).
implout(~P,~P1) :-
    !, implout(P,P1).
implout(P,P).

% Moving negations inward

negin(~P,P1) :-
    !, neg(P,P1).
negin(all(X,P),all(X,P1)) :-
    !, negin(P,P1).
negin(exists(X,P),exists(X,P1)) :-
    !, negin(P,P1).
negin(P&Q,P1&Q1) :-
    !, negin(P,P1),
    negin(Q,Q1).
negin(P#Q,P1#Q1) :-
    !, negin(P,P1),
    negin(Q,Q1).
negin(P,P).

neg(~P,P1) :-
    !, negin(P,P1).
neg(all(X,P),exists(X,P1)) :-
    !, neg(P,P1).
neg(exists(X,P),all(X,P1)) :-
    !, neg(P,P1).
neg(P&Q,P1#Q1) :-
    !, neg(P,P1),
    neg(Q,Q1).
neg(P#Q,P1&Q1) :-
    !, neg(P,P1),
    neg(Q,Q1).
neg(P,~P).

% Skolemization

skolem(all(X,P),all(X,P1),Vars) :-
    !, skolem(P,P1,[X|Vars]).
skolem(exists(X,P),P2,Vars) :-
    !, gensym(s,S),
    Sk =.. [S|Vars],
    subst(X,Sk,P,P1),
    skolem(P1,P2,Vars).
skolem(P&Q,P1&Q1,Vars) :-
    !, skolem(P,P1,Vars),
    skolem(Q,Q1,Vars).
skolem(P#Q,P1#Q1,Vars) :-
    !, skolem(P,P1,Vars),
    skolem(Q,Q1,Vars).
skolem(P,P,_).

gensym(Char,Atom) :-
    getnum(Char,N),
    numlist(N,L),
    name(Char,[M]),
    name(Atom,[M|L]).

getnum(C,M) :-
    retract(cn(C,N)),
    M is N+1,
    assertz(cn(C,M)),!.
getnum(C,1) :-
    assertz(cn(C,1)).

numlist(N,[M]) :-
    N<10, M is 48+N, !.
numlist(N,[M|T]) :-
    M is N mod 10 + 48,
    N1 is N // 10,
    numlist(N1,T).

subst(X,Y,P,Q) :-
    nonvar(P),
    P =.. [F|L1],
    subst1(X,Y,L1,L2),
    Q =.. [F|L2], !.
subst(X,Y,Z,Y) :-
    X == Z, !.
subst(_,_,X,X).

subst1(_,_,[],[]) :- !.
subst1(X,Y,[A|T1],[B|T2]) :-
    subst(X,Y,A,B),
    subst1(X,Y,T1,T2).

% Removing universal quantifiers

univout(all(_,P),P1) :- !,
    univout(P,P1).
univout(P&Q,P1&Q1) :- !,
    univout(P,P1),
    univout(Q,Q1).
univout(P#Q,P1#Q1) :- !,
    univout(P,P1),
    univout(Q,Q1).
univout(P,P).

% Conjunctive normal form

conjn(P#Q,R) :- !,
    conjn(P,P1),
    conjn(Q,Q1),
    conjn1(P1#Q1,R).
conjn(P&Q,P1&Q1) :- !,
    conjn(P,P1),
    conjn(Q,Q1).
conjn(P,P).

conjn1((P&Q)#R,P1&Q1) :- !,
    conjn(P#R,P1),
    conjn(Q#R,Q1).
conjn1(P#(Q&R),P1&Q1) :- !,
    conjn(P#Q,P1),
    conjn(P#R,Q1).
conjn1(P,P).

% Putting into clauses

clausify(P&Q,C1,C2) :- !,
    clausify(P,C1,C3),
    clausify(Q,C3,C2).
clausify(P,[cl(A,B)|Cs],Cs) :-
    inclause(P,A,[],B,[]), !.
clausify(_,C,C).

inclause(P#Q,A,A1,B,B1) :- !,
    inclause(P,A2,A1,B2,B1),
    inclause(Q,A,A2,B,B2).
inclause(~P,A,A,B1,B) :- !,
    notin(P,A),
    putin(P,B,B1).
inclause(P,A1,A,B,B) :-
    notin(P,B),
    putin(P,A,A1).

notin(X,[Y|_]) :- 
    X==Y, !, fail.
notin(X,[_|L]) :-
    !, notin(X,L).
notin(_,[]).

putin(X,[],[X]) :- !.
putin(X,[Y|L],[X|L]) :- X==Y,!.
putin(X,[Y|L],[Y|L1]) :-
    putin(X,L,L1).
