%--------------------------------------------------------------%
%   Situation Calculus (simple deductive planning)             %
%   by using depth-bound strategy                              %
%   (C) 1998 Zdravko Markov                                    %
%--------------------------------------------------------------%

% Sample queries
% ?- holds(on(a,c),S,1).
% no
% ?- holds(on(a,c),S,2).
% no
% ?- holds(on(a,c),S,3).
% S = [puton(a,c),puton(b,table),puton(a,table)] ?
%
% ?- holds((on(b,a),on(c,b)),S,3).
% S = [puton(c,b),puton(b,a),puton(a,table)] ? 

%---------------------------------------------------------------
% holds(+Goals,-Situation,+Steps).

% Initial Situation
holds(on(a,b),[],_).
holds(on(b,c),[],_).
holds(clear(a),[],_).
holds(on(c,table),[],_).

% Axioms
holds(on(A,B),[puton(A,B)|S],N) :- 
    N>0, M is N-1,
    neq(A,B),
    holds(clear(A),S,M),
    holds(clear(B),S,M).
holds(clear(C),[puton(A,B)|S],N) :-
    N>0, M is N-1,
    neq(A,B),
    holds(on(A,C),S,M),
    holds(clear(A),S,M),
    holds(clear(B),S,M).

% Frame Axioms
holds(on(X,Y),[puton(A,B)|S],N) :-
    N>0, M is N-1,
    neq(A,B),
    neq(A,X),
    neq(A,Y),
    holds(on(X,Y),S,M).
holds(clear(X),[puton(_,B)|S],N) :-
    N>0, M is N-1,
    neq(X,B),
    holds(clear(X),S,M).
holds(clear(table),_,_).

% Solving conjunctive goals
holds((A,B),S,N) :-
   holds(A,S,N),
   holds(B,S,N).

% Extensional definition of not equal
neq(a,table).
neq(table,a).
neq(b,table).
neq(table,b).
neq(c,table).
neq(table,c).
neq(a,b).
neq(a,c).
neq(b,a).
neq(c,a).
neq(b,c).
neq(c,b).
