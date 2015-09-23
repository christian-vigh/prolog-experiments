%---------------------------------------------------------------------%
%   The farmer, wolf, cabbage and goat problem domain                 %
%   Using the Prolog built-in depth first search to solve the problem %
%   (C) 1998 Zdravko Markov                                           %
%---------------------------------------------------------------------%

change_state(s,W,G,C,n,W,G,C).
change_state(n,W,G,C,s,W,G,C).
change_state(s,s,G,C,n,n,G,C).
change_state(n,n,G,C,s,s,G,C).
change_state(s,W,s,C,n,W,n,C).
change_state(n,W,n,C,s,W,s,C).
change_state(s,W,G,s,n,W,G,n).
change_state(n,W,G,n,s,W,G,s).

illegal(s,n,n,_).
illegal(n,s,s,_).
illegal(s,_,n,n).
illegal(n,_,s,s).

% A solution without lists (use "?- lisitng(visited)." to see the solution)

solve(s,s,s,s).
solve(B,W,G,C) :-
    change_state(B,W,G,C,B1,W1,G1,C1),
    \+ illegal(B1,W1,G1,C1),
    \+ clause(visited(B1,W1,G1,C1),_),
    assertz(visited(B1,W1,G1,C1)),
    solve(B1,W1,G1,C1).


% A solution with lists (use "?- solve([(n,n,n,n)],L)." to get the path with states)
/*
solve([(s,s,s,s)|L],Path) :- reverse([(s,s,s,s)|L],Path).
solve([(B,W,G,C)|L],Path) :-
    change_state(B,W,G,C,B1,W1,G1,C1),
    \+ illegal(B1,W1,G1,C1),
    \+ member((B1,W1,G1,C1),L),
    solve([(B1,W1,G1,C1),(B,W,G,C)|L],Path).
*/