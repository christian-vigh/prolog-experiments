%-------------------------------------------------------------%
%   Satisfiability solver                                     %
%   (C) 2005 Zdravko Markov                                   %
%-------------------------------------------------------------%
%  Call: sat(+Clauses, -True, -False).                        %
%  Clasues = [cl([P1,P2,...],[N1,N2,...]),...]                %
%  True = positive literals                                   %
%  False = negative literals                                  %
%-------------------------------------------------------------%

sat(Clauses,True,False) :-
   setof(L,Pos^Neg^(member(cl(Pos,Neg),Clauses),
                   (member(L,Pos);member(L,Neg))),Literals),
   choose(Literals,True), 
   subtract(Literals,True,False),
   forall(member(C,Clauses),model(C,True,False)).

model(cl(Pos,_),True,_) :-
   member(L,Pos),
   member(L,True), !.
model(cl(_,Neg),_,False) :-
   member(L,Neg),
   member(L,False), !.

choose(_,[]).
choose(H1,H2) :-
    length(H1,N),
    template(H2,N),
    sublist(H2,H1).

template(_,0) :- !, fail.
template([_],_).
template([_|T],N) :-
    M is N-1,
    template(T,M).

sublist([X],[X|_]).
sublist(X,[_|T]) :-
    sublist(X,T).
sublist([X|T],[X|V]) :-
    sublist(T,V).

   