% 4-queens problem for the CSP solver using arc consistency (csp.pl)

queens(Q1,Q2,Q3,Q4) :-
    csp([dom(Q1,[(1,1),(1,2),(1,3),(1,4),
                 (2,1),(2,2),(2,3),(2,4),
                 (3,1),(3,2),(3,3),(3,4),
                 (4,1),(4,2),(4,3),(4,4)]),
         dom(Q2,[(1,1),(1,2),(1,3),(1,4),
                 (2,1),(2,2),(2,3),(2,4),
                 (3,1),(3,2),(3,3),(3,4),
                 (4,1),(4,2),(4,3),(4,4)]),
         dom(Q3,[(1,1),(1,2),(1,3),(1,4),
                 (2,1),(2,2),(2,3),(2,4),
                 (3,1),(3,2),(3,3),(3,4),
                 (4,1),(4,2),(4,3),(4,4)]),
         dom(Q4,[(1,1),(1,2),(1,3),(1,4),
                 (2,1),(2,2),(2,3),(2,4),
                 (3,1),(3,2),(3,3),(3,4),
                 (4,1),(4,2),(4,3),(4,4)])],
        [rel([Q1,Q2],consistent(Q1,Q2)),
         rel([Q1,Q3],consistent(Q1,Q3)),
         rel([Q1,Q4],consistent(Q1,Q4)),
         rel([Q2,Q3],consistent(Q2,Q3)),
         rel([Q2,Q4],consistent(Q2,Q4)),
         rel([Q3,Q4],consistent(Q3,Q4))]).

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
