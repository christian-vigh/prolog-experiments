%--------------------------------------------------------------%
%   Wumpus world simulation                                    %
%   (C) 1998 Zdravko Markov                                    %
%--------------------------------------------------------------%
%   The wumpus world is as described in the book of Russell &  %
%   Norvig, Artificial Intelligence: A Modern Approach,        %
%   Prentice Hall, Upper Saddle River, New Jersey, 1995.       %
%   Two additional perceptions are introduced:                 %
%   1. The agent can perceive light in the start/exit square   %
%   2. The agent can perceive the location where it is heading %
%--------------------------------------------------------------%

% Environment

start(1,1).  % defining the start position

condition(1,1,light).  % the start/exit square

condition(1,2,stench).
condition(1,4,stench).
condition(2,1,breeze).
condition(2,3,breeze).
condition(2,3,stench).
condition(2,3,glitter).
condition(3,2,breeze).
condition(3,4,breeze).
condition(4,1,breeze).
condition(4,3,breeze).

object(1,3,wumpus).
object(2,3,gold).
object(3,1,pit).
object(3,3,pit).
object(4,4,pit).

%--------------------------------------------------------------------
% Perception

% The agent can perceive the following:
%   1. the condition in the current square: stench(X,Y), breeze(X,Y),
%      glitter(X,Y), light(X,Y);
%   2. the bump event: bump;
%   3. the location where the agent is heading: heading(X,Y).
% The perception is added automatically to the agent's knowledge base
% and updated accordingly.
% The perception is *complete*, i.e. all symbols (except for heading)
% are present in the knowledge base - either as positive or as negative 
% literals.

perception :- 
    retractall(cl([light(_,_)],[])),
    retractall(cl([],[light(_,_)])),
    retractall(cl([stench(_,_)],[])),
    retractall(cl([],[stench(_,_)])),
    retractall(cl([breeze(_,_)],[])),
    retractall(cl([],[breeze(_,_)])),
    retractall(cl([glitter(_,_)],[])),
    retractall(cl([],[glitter(_,_)])),
    retractall(cl([bump],[])),
    retractall(cl([],[bump])),
    retractall(cl([heading(_,_)],[])),
    retractall(cl([],[heading(_,_)])),
    pos(X,Y),
    pos_or_neg([stench(X,Y),breeze(X,Y),glitter(X,Y),light(X,Y),
                bump,heading(_,_)]).

pos_or_neg([]) :- !.
pos_or_neg([P|T]) :-
    perceives(P), !,
    assertz(cl([P],[])),  % assert positive literal (P)
    pos_or_neg(T).
pos_or_neg([P|T]) :-
    assertz(cl([],[P])),  % assert negative literal (~P)
    pos_or_neg(T).

perceives(Atom) :-  % perceiving conditions at the current square
    pos(X,Y),
    functor(Atom,Condition,2),
    condition(X,Y,Condition).
perceives(Event) :- % perceiving events (e.g. "bump")
    event(Event).
perceives(heading(X,Y)) :- % perceiving the next position
    orientation(Dir),
    pos(X0,Y0),
    next_location(Dir,X0,Y0,X1,Y1),
    X is X1, Y is Y1.

next_location(north,X,Y,X,Y+1).
next_location(south,X,Y,X,Y-1).
next_location(east,X,Y,X+1,Y).
next_location(west,X,Y,X-1,Y).

%--------------------------------------------------------------------
% Actions

% The right hand side of the acion rules contain lists of actions.
% The left hand side of the rules contain literals from the agents KB.
% The possible actions are: go_forward, turn(left), turn(right), grab
% and climb. The actions are executed in the order they are specified
% in the list.

% Inferring actions

action :-
    rl(P,N),
    resolven(N),
    resolvep(P,[A]),
    nl, write(A),                  % print action(s)
    execute(A).                    % execute action(s)
action :-
    nl,write('No action inferred!'),nl,
    abort.

% UR-resolution (resolving a rule with unit clauses from KB)
% The resulting clause must be positive with action atoms only.

resolven([]) :- !.
resolven([L|T]) :-
    cl([L],[]),
    resolven(T).

resolvep([],[]) :- !.
resolvep([[A|B]|T],[[A|B]|P]) :- !,
    resolvep(T,P).
resolvep([L|T],P) :-
    cl([],[L]),
    resolvep(T,P).

% Executing actions

execute([]) :- !.
execute([A|T]) :-
    member(A,[go_forward,turn(left),turn(right),grab,climb]), !,
    call(A),
    execute(T).
execute([A|_]) :- 
    nl,write('Incorrect action: '),write(A),nl,
    abort.

go_forward :-
    retractall(event(bump)),
    orientation(Dir),
    move(Dir),
    check.

turn(Dir) :-
    retractall(event(bump)),
    retract(orientation(X)),
    turn_table(Dir,X,Y),
    assertz(orientation(Y)), 
    check, !.

grab :-
    pos(X,Y),
    nl, write('Grabbing ... '),
    object(X,Y,Object),
    write(Object), !.
grab :-
    write('Nothing to grab!').

climb :-
    nl, write('Climbing ... '),
    start(X,Y),
    pos(X,Y),
    write(ok), nl,
    abort.
climb :-
    write('No way out! (get back to the starting postion)').


% Updating the world

check :-
    pos(X,Y),
    orientation(Dir),
    nl,write([pos(X,Y),orientation(Dir)]),
    check(X,Y).

check(X,Y) :-
    object(X,Y,wumpus), !,
    nl, write('The wumpus ate the agent!'), nl,
    abort.
check(X,Y) :-
    object(X,Y,pit), !,
    nl,write('The agent fell into a pit!'),nl,
    abort.
check(_,_).

move(north) :-
    (pos(_,4),assertz(event(bump)),nl,write('Bump !');
     retract(pos(X,Y)),Y1 is Y+1,assertz(pos(X,Y1))), !.
move(south) :-
    (pos(_,1),assertz(event(bump)),nl,write('Bump !');
     retract(pos(X,Y)),Y1 is Y-1,assertz(pos(X,Y1))), !.
move(east) :-
    (pos(4,_),assertz(event(bump)),nl,write('Bump !');
     retract(pos(X,Y)),X1 is X+1,assertz(pos(X1,Y))), !. 
move(west) :-
    (pos(1,_),assertz(event(bump)),nl,write('Bump !');
     retract(pos(X,Y)),X1 is X-1,assertz(pos(X1,Y))), !.

turn_table(left,north,west).
turn_table(left,west,south).
turn_table(left,south,east).
turn_table(left,east,north).
turn_table(right,north,east).
turn_table(right,east,south).
turn_table(right,south,west).
turn_table(right,west,north).

%--------------------------------------------------------------------
% Wumpus world simulation

start(Reasoning_Knowledge,Action_Rules,Steps) :-
    init_world,
    add_to_kb(Reasoning_Knowledge),
    add_to_rb(Action_Rules),
    go(Steps).

init_world :-
    retractall(event(_)),
    retractall(pos(_,_)),
    retractall(orientation(_)),
    start(X,Y),
    assertz(pos(X,Y)),
    assertz(orientation(east)),
    retractall(cl(_,_)),
    retractall(rl(_,_)).

go(0) :- !.
go(N):-
    nl, write('Perceiving .. '),
    perception,
    nl, write('Reasoning .. '),
    resolve,
    nl, write('Acting .. '),
    action,
    M is N-1,
    go(M).

add_to_kb([]) :- !.
add_to_kb([F|T]) :-
    translate(F,C),
    assert_clause(C),
    add_to_kb(T).

add_to_rb([]) :- !.
add_to_rb([F|T]) :-
    translate(F,C),
    assert_rule(C),
    add_to_rb(T).

assert_clause([]) :- !.
assert_clause([C|T]) :-
    assertz(C),
    assert_clause(T).

assert_rule([]) :- !.
assert_rule([cl(P,N)|T]) :-
    assertz(rl(P,N)),
    assert_rule(T).

%--------------------------------------------------------------------
% Translation of FOL formulae into clauses

% The agent can define its reasoning as first order formulae using
% the following conjectives (implicitly universally quantified):

:- op(850,fx,~).    % negation
:- op(900,xfy,#).   % disjunction
:- op(900,xfy,&).   % conjunction
:- op(950,xfy,->).  % implication

translate(Formula,Clauses) :-
    implout(Formula,X1),
    negin(X1,X2),
    conjn(X2,X3),
    clausify(X3,Clauses,[]).

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

notin(X,[Y|_]) :- X==Y,
    !, fail.
notin(X,[_|L]) :-
    !, notin(X,L).
notin(_,[]).

putin(X,[],[X]) :- !.
putin(X,[Y|L],[X|L]) :- X==Y,!.
putin(X,[Y|L],[Y|L1]) :-
    putin(X,L,L1).

%--------------------------------------------------------------------
% Reasoning (resolution)

resolve :-
    resolvent(C), !,
    show_unit(C),
    resolve.
resolve.

show_unit(cl([P],[])) :- !,   % positive
    nl, write('Derived: '), 
    write(P).
show_unit(cl([],[P])) :- !,   % negative
    nl, write('Derived: '), 
    write(~P).
show_unit(_).

resolvent(cl(P,N)) :-
    new_resolvent(P,N),
    \+ cl(P,N),
    assertz(cl(P,N)).

new_resolvent(P,N) :-
    cl(P1,N1),cl(P2,N2),
    del(L,P1,P3),
    del(L,N2,N3), 
    numbervars(L,0,0),  % ground literals only!
    append1(P2,P3,P),
    append1(N1,N3,N),
    \+ ((member(X,P),member(X,N))).

append1([],L,L) :- !.
append1([H|T],L,[H|V]) :-
    \+ member(H,L), !,
    append1(T,L,V).
append1([_|T],L,V) :-
    append1(T,L,V).

del(X,[X|T],T).
del(X,[Y|T],[Y|V]) :-
    del(X,T,V).
