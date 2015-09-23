% Backtracking solution for the 4-queens problem 


solve(A,B,C,D) :-
    domain(List),
    member(A,List), 
    member(B,List), 
    member(C,List), 
    member(D,List), % writeln(A-B-C-D), 
    consistent(A,B),
    consistent(A,C),
    consistent(A,D),
    consistent(B,C),
    consistent(B,D),
    consistent(C,D).

domain([(1,1),(1,2),(1,3),(1,4),
        (2,1),(2,2),(2,3),(2,4),
        (3,1),(3,2),(3,3),(3,4),
        (4,1),(4,2),(4,3),(4,4)]).


consistent((A,_),(A,_)) :-    % no queen at the same rank
    !,fail. 
consistent((_,B),(_,B)) :-    % no queen at the same file
    !,fail. 
consistent((A,B),(C,D)) :-    % no queens at the diagonals
    A is C+1,B is D+1,!,fail.
consistent((A,B),(C,D)) :- 
    A is C+1,B is D-1,!,fail.
consistent((A,B),(C,D)) :-
    A is C-1,B is D+1,!,fail.
consistent((A,B),(C,D)) :- 
    A is C-1,B is D-1,!,fail.
consistent(_,_).
