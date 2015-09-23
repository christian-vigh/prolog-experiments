%-------------------------------------------------------------%
%   Clausal resolution                                        %
%   (C) 1998 Zdravko Markov                                   %
%-------------------------------------------------------------%
%  Call: resolve([cl([P1,P2,...],[N1,N2,...]), cl(...),...]). %
%   Pi are positive and Ni negative literals                  %
% The resolvents are printed and stored in the database along %
% with the original set of clauses as facts:                  %
%  cl([P1,P2,...],[N1,N2,...]).                               %
%-------------------------------------------------------------%

resolve(CL) :-
    retractall(cl(_,_)),
    assertcl(CL),
    resolve.

resolve :-
    resolvent(C), !,                % Find a resolvent
    write('Derived: '),             % print it out
    write(C), nl,                   % and
    resolve.                        % continue until no more 
resolve.                            % resolvents can be derived

resolvent(cl(P,N)) :-
    new_resolvent(P,N),             % Find a new resolvent.
    \+ cl(P,N),                     % If not already derived
    assertz(cl(P,N)).               % assert it in the database

new_resolvent(P,N) :-
    cl(P1,N1),                      % Retrieve clauses: P1<-N1
    cl(P2,N2),                      % and P2<-N2.
    del(L,P1,P3),                   % P3 = P1\{L}
    del(L,N2,N3),                   % N3 = N2\{L}
    append1(P2,P3,P),               % P = P2 U P3
    append1(N1,N3,N),               % N = N1 U N3
    \+ ((member(X,P),member(X,N))). % Remove tautologies

append1([],L,L) :- !.
append1([H|T],L,[H|V]) :-
    \+ member(H,L), !,
    append1(T,L,V).
append1([_|T],L,V) :-
    append1(T,L,V).

del(X,[X|T],T).
del(X,[Y|T],[Y|V]) :-
    del(X,T,V).

assertcl([]) :- !.
assertcl([X|T]) :-
   assertz(X),
   assertcl(T).

seteq(X,Y) :-
   subset(X,Y),
   subset(Y,X).
