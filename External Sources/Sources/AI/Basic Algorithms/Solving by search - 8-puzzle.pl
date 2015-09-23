%--------------------------------------------------------------%
%   8-puzzle problem domain                                    %
%   (C) 1998 Zdravko Markov                                    %
%--------------------------------------------------------------%
%   0 1 2
%   3 4 5   =>  board(0,1,2,3,4,5,6,7,8)
%   6 7 8
%--------------------------------------------------------------%

% empty tile placed on position 1
arc(board(0,B,C,D,E,F,G,H,I),board(B,0,C,D,E,F,G,H,I),2).
arc(board(0,B,C,D,E,F,G,H,I),board(D,B,C,0,E,F,G,H,I),2).

% empty tile placed on position 2
arc(board(A,0,C,D,E,F,G,H,I),board(0,A,C,D,E,F,G,H,I),2).
arc(board(A,0,C,D,E,F,G,H,I),board(A,C,0,D,E,F,G,H,I),2).
arc(board(A,0,C,D,E,F,G,H,I),board(A,E,C,D,0,F,G,H,I),2).

% empty tile placed on position 3
arc(board(A,B,0,D,E,F,G,H,I),board(A,0,B,D,E,F,G,H,I),2).
arc(board(A,B,0,D,E,F,G,H,I),board(A,B,F,D,E,0,G,H,I),2).

% empty tile placed on position 4
arc(board(A,B,C,0,E,F,G,H,I),board(0,B,C,A,E,F,G,H,I),2).
arc(board(A,B,C,0,E,F,G,H,I),board(A,B,C,E,0,F,G,H,I),2).
arc(board(A,B,C,0,E,F,G,H,I),board(A,B,C,G,E,F,0,H,I),2).

% empty tile placed on position 5
arc(board(A,B,C,D,0,F,G,H,I),board(A,0,C,D,B,F,G,H,I),2).
arc(board(A,B,C,D,0,F,G,H,I),board(A,B,C,0,D,F,G,H,I),2).
arc(board(A,B,C,D,0,F,G,H,I),board(A,B,C,D,F,0,G,H,I),2).
arc(board(A,B,C,D,0,F,G,H,I),board(A,B,C,D,H,F,G,0,I),2).

% empty tile placed on position 6
arc(board(A,B,C,D,E,0,G,H,I),board(A,B,0,D,E,C,G,H,I),2).
arc(board(A,B,C,D,E,0,G,H,I),board(A,B,C,D,0,E,G,H,I),2).
arc(board(A,B,C,D,E,0,G,H,I),board(A,B,C,D,E,I,G,H,0),2).

% empty tile placed on position 7
arc(board(A,B,C,D,E,F,0,H,I),board(A,B,C,0,E,F,D,H,I),2).
arc(board(A,B,C,D,E,F,0,H,I),board(A,B,C,D,E,F,H,0,I),2).

% empty tile placed on position 8
arc(board(A,B,C,D,E,F,G,0,I),board(A,B,C,D,0,F,G,E,I),2).
arc(board(A,B,C,D,E,F,G,0,I),board(A,B,C,D,E,F,0,G,I),2).
arc(board(A,B,C,D,E,F,G,0,I),board(A,B,C,D,E,F,G,I,0),2).

% empty tile placed on position 9
arc(board(A,B,C,D,E,F,G,H,0),board(A,B,C,D,E,0,G,H,F),2).
arc(board(A,B,C,D,E,F,G,H,0),board(A,B,C,D,E,F,G,0,H),2).


% Heuristic (number of misplaced tiles)
% Note that the goal state is (0,1,2,3,4,5,6,7,8)
% For another goal Change the defnition below

h(board(A,B,C,D,E,F,G,H,I),W) :-
    distance([A,B,C,D,E,F,G,H,I],[0,1,2,3,4,5,6,7,8],W).

distance([],[],0).
distance([X|T],[X|V],W) :- !,
    distance(T,V,W).
distance([_|T],[_|V],W) :-
    distance(T,V,W1),
    W is W1+1.
