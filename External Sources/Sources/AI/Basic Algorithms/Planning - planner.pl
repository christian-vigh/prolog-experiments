%--------------------------------------------------------------%
%   STRIPS like planner                                        %
%   (C) 1998 Zdravko Markov                                    %
%--------------------------------------------------------------%

% Examples of planning rules:

% rule(Action,Preconditions,Adds,Deletes,Procedure).
% The Procedure checks the correctness of the rule instantiation

rule(puton(X,Y),
    [clear(X),on(X,Z),clear(Y)],[on(X,Y),clear(Z)],[clear(Y),on(X,Z)],
    (nonvar(X),nonvar(Y),nonvar(Z),X\==Y,Y\==Z,X\==Z)).

rule(puton(X,table),
    [clear(X),on(X,Y)],[on(X,table),clear(Y)],[on(X,Y)],
    (nonvar(X),nonvar(Y),X\==Y)).

% Example query:

% ?- plan([on(a,b),on(b,c),on(c,table),clear(a)],[on(a,c)],S,Plan). 
% [on(a,c)]
% Considering action:    puton(a,c)
% Solving precondions:   [clear(a),on(a,b),clear(c)]
% Considering action:    puton(b,table)
% Solving precondions:   [clear(b),on(b,c)]
% Considering action:    puton(a,table)
% Solving precondions:   [clear(a),on(a,b)]
% Solving rest of goals: []
% Solving rest of goals: []
% Solving rest of goals: []
%
% S = [on(c,table),clear(a),on(a,table),clear(b),on(b,table),on(a,c)],
% Plan = [puton(a,table),puton(b,table),puton(a,c)] ? 

%---------------------------------------------------------------------
% Rule interpreter
% plan(+InitState,+GoalState,-EndState,-Plan)
% Taking care of clobbered goals by repeated execution

plan(State,Goals,State,[]) :-
    diff(Goals,State,[]), !.
plan(State,Goals,State2,Plan) :-
    plan1(State,Goals,State1,Plan1),
    plan(State1,Goals,State2,Plan2),
    append(Plan1,Plan2,Plan).

% plan1(+InitState,+GoalState,-EndState,-Plan)
% Basic planner (ingnoring the clobbered goals)

plan1(State,Goals,NewState,Plan) :- 
    write(Goals),nl,
    diff(Goals,State,[Goal|Rest]),      % Pick a Goal to solve.
    numbervars(Goal,0,0),               % Goal must be ground
    rule(Action,Precond,Add,Del,Proc),  % Pick a rule such that 
    member(Goal,Add),                   % Goal is in its Adds.
    instantiate(Precond,State),         % Instantiate Preconds.
    call(Proc),                         % Check the Instantiation.
    write('Considering action:    '),
    write(Action),nl,
    write('Solving precondions:   '),
    plan1(State,Precond,State1,Plan1),  % Solve Preconds.
    update(State1,Add,Del,State2), !,   % Update current state.
    write('Solving rest of goals: '),
    plan1(State2,Rest,NewState,Plan2),  % Solve the rest of goals.
    append(Plan1,[Action|Plan2],Plan).  % Collect the plans.
plan1(State,_,State,[]).


% instantiate(+Preconds,+State) - 
% Looking for partial instantiations (by member(Goal,Add))

instantiate([],_) :- !.
instantiate([X|T],L) :-
    \+ (functor(X,_,N),numbervars(X,0,N)), % not all variables free
    member(X,L), !,
    instantiate(T,L).
instantiate([_|T],L) :-
    instantiate(T,L).


% update(+State,+Adds,+Dels,-NewState).

update([],State,_,State) :- !.
update([S|T],Add,Del,State) :-
    member(S,Del), !,
    update(T,Add,Del,State).
update([S|T],Add,Del,[S|State]) :-
    \+ member(S,Add), !,
    update(T,Add,Del,State).
update([_|T],Add,Del,State) :-
    update(T,Add,Del,State).


% Auxilliary predicates

diff([],_,[]).
diff([X|T],L,R) :-
    member(X,L), !,
    diff(T,L,R).
diff([X|T],L,[X|R]) :-
    diff(T,L,R).

