%--------------------------------------------------------------%
%   Uninformed Graph Searching Algorithms                      %
%   The graph should be represented as a set of facts          %
%   arc(Node1,Node2,Cost) loaded in the database               %
%   (C) 1998 Zdravko Markov                                    %
%--------------------------------------------------------------%

%--------------------------------------------------------------%
%   Depth-first search by using a stack                        %
%   call: depth_first(+[[Start]],+Goal,-Path,-ExploredNodes).  %
%--------------------------------------------------------------%
depth_first([[Goal|Path]|_],Goal,[Goal|Path],0).
depth_first([Path|Queue],Goal,FinalPath,N) :-
    extend(Path,NewPaths), 
    append(NewPaths,Queue,NewQueue),
    depth_first(NewQueue,Goal,FinalPath,M),
    N is M+1.

extend([Node|Path],NewPaths) :-
    findall([NewNode,Node|Path],
            (arc(Node,NewNode,_), 
            \+ member(NewNode,Path)), % for avoiding loops
            NewPaths).

%--------------------------------------------------------------%
%   Built-in Prolog depth-first search                         %
%   call: prolog_search(+Start,+Goal,-Path).                   %
%--------------------------------------------------------------%
prolog_search(Goal,Goal,[Goal]).
prolog_search(Node,Goal,[Node|Path]) :-
    arc(Node,NewNode,_),
    prolog_search(NewNode,Goal,Path).

%--------------------------------------------------------------%
%   Breadth-first search                                       %
%   call: breadth_first(+[[Start]],+Goal,-Path,-ExploredNodes).%
%--------------------------------------------------------------%
breadth_first([[Goal|Path]|_],Goal,[Goal|Path],0).
breadth_first([Path|Queue],Goal,FinalPath,N) :-
    extend(Path,NewPaths), 
    append(Queue,NewPaths,NewQueue),
    breadth_first(NewQueue,Goal,FinalPath,M),
    N is M+1.

%--------------------------------------------------------------%
%   Depth-bound search                                         %
%   call: depth_bound(+DepthBound,+[[Start]],+Goal,-Path).     %
%--------------------------------------------------------------%
depth_bound(_,[[Goal|Path]|_],Goal,[Goal|Path]).
depth_bound(Depth,[Path|Queue],Goal,FinalPath) :-
    extend1(Depth,Path,NewPaths),
    append(NewPaths,Queue,NewQueue),
    depth_bound(Depth,NewQueue,Goal,FinalPath).

extend1(Depth,[Node|Path],NewPaths) :-
    length(Path,Len),
    Len < Depth-1, !,
    findall([NewNode,Node|Path],arc(Node,NewNode,_),NewPaths).
extend1(_,_,[]).

%--------------------------------------------------------------%
%  Iterative Deepening search                                  %
%  call: iterative_deepening(+[[Start]],+Goal,-Path).          %
%--------------------------------------------------------------%
iterative_deepening(Queue,Goal,Path) :-
    iterative_deepening1(1,Queue,Goal,Path).

iterative_deepening1(Depth,Queue,Goal,Path) :- 
    nl,write(depth=Depth),
    depth_bound(Depth,Queue,Goal,Path).
iterative_deepening1(Depth,Queue,Goal,Path) :- 
    Depth1 is Depth+1,
    iterative_deepening1(Depth1,Queue,Goal,Path).

%--------------------------------------------------------------%
%   Calculating Path cost                                      %
%   call: path_cost(+Path,-Cost).                              %
%--------------------------------------------------------------%
path_cost([A,B],Cost) :-
    arc(A,B,Cost).
path_cost([A,B|T],Cost) :-
    arc(A,B,Cost1),
    path_cost([B|T],Cost2),
    Cost is Cost1+Cost2.

reverse_path_cost([A,B],Cost) :-
    arc(B,A,Cost).
reverse_path_cost([A,B|T],Cost) :-
    arc(B,A,Cost1),
    reverse_path_cost([B|T],Cost2),
    Cost is Cost1+Cost2.

%--------------------------------------------------------------%
%   Uniform Cost Search                                        %
%   call: uni_cost(+[[Start]],+Goal,-Path,-ExploredNodes).     %
%--------------------------------------------------------------%
uni_cost([[Goal|Path]|_],Goal,[Goal|Path],0).
uni_cost([Path|Queue],Goal,FinalPath,N) :- 
    extend(Path,NewPaths),
    append(Queue,NewPaths,Queue1), 
    sort_queue(Queue1,NewQueue),
    uni_cost(NewQueue,Goal,FinalPath,M),
    N is M+1.

sort_queue(L,L2) :-
    swap(L,L1), !,
    sort_queue(L1,L2).
sort_queue(L,L).

swap([X,Y|T],[Y,X|T]) :-
    reverse_path_cost(X,CX),
    reverse_path_cost(Y,CY),
    CX>CY.
swap([X|T],[X|V]) :-
    swap(T,V).

