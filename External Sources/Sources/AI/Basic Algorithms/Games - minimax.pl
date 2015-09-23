%--------------------------------------------------------------%
% Searching game trees (minimax and alpha-beta pruning)        %
% (C) 1998 Zdravko Markov (adaptation from Ivan Bratko's book) %
% The game tree is represented by the following predicates:    %
%   moves(Pos,ListOfSuccessorPositions) - possible moves       %
%   utility(Pos,Utility) - utility function value of terminals %
%   min(Position) - MIN is to move at Positions                %
%   max(Position) - MAX is to move at Positions                %
%--------------------------------------------------------------%
%--------------------------------------------------------------%
% Minimax search                                               %
%   call: minimax(+Pos,-BestSuccessor,-Utility).               %
%--------------------------------------------------------------%

minimax(Pos,Succ,Utility) :-
    moves(Pos,SuccList), !,           % not a terminal position
    best_move(SuccList,Succ,Utility).
minimax(Pos,_,Utility) :-             % a terminal position
    utility(Pos,Utility).             % an explicit utility

best_move([P],P,V) :-
    minimax(P,_,V), !.
best_move([P1|Ps],Succ,Val) :-
    minimax(P1,_,V1),
    best_move(Ps,P2,V2),
    better(P1,V1,P2,V2,Succ,Val).

better(P1,V1,_,V2,P1,V1) :-
    min(P1),
    V1>V2, !.
better(P1,V1,_,V2,P1,V1) :-
    max(P1),
    V1<V2, !.
better(_,_,P2,V2,P2,V2).

%--------------------------------------------------------------%
%   Alpha-Beta search                                          %
%   call: alphabeta(+Pos,+Alpha,+Beta,-BestSuccessor,-Util).   %
%--------------------------------------------------------------%
alphabeta(Pos,Alpha,Beta,Succ,Util) :-
    moves(Pos,SuccList), !,
    next_move(SuccList,Alpha,Beta,Succ,Util).
alphabeta(Pos,_,_,_,Util) :- 
    utility(Pos,Util).

next_move([P|Ps],Alpha,Beta,Next,Eval) :- 
    alphabeta(P,Alpha,Beta,_,V),
    good_move(Ps,Alpha,Beta,P,V,Next,Eval).

good_move([],_,_,P,V,P,V) :- !.
good_move(_,_,Beta,P,V,P,V) :-
    min(P),
    V>Beta, !.
good_move(_,Alpha,_,P,V,P,V) :-
    max(P),
    V<Alpha, !.
good_move(Ps,Alpha,Beta,P,V,GoodP,GoodV) :-
    newbounds(Alpha,Beta,P,V,NewAlpha,NewBeta),
    next_move(Ps,NewAlpha,NewBeta,P1,V1),
    better(P,V,P1,V1,GoodP,GoodV).

newbounds(Alpha,Beta,P,V,V,Beta) :- 
    min(P),
    V>Alpha, !.
newbounds(Alpha,Beta,P,V,Alpha,V) :-
    max(P),
    V<Beta, !.
newbounds(Alpha,Beta,_,_,Alpha,Beta).

