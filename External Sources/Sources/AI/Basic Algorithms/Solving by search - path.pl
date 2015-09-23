%--------------------------------------------------------------------------------------------
% Simple graph search in Prolog (add/remove the comments /* ... */ to use different versions)
%--------------------------------------------------------------------------------------------

%----------------------------------------------------------------
% The graph
%----------------------------------------------------------------

% arc(4,1). % What happens if you add this arc to the graph here? 

                         %          1
                         %       ___|____
arc(1,2).                %      |        |
arc(1,3).                %      2        3
arc(2,4).                %     _|_      _|_
arc(2,5).                %    |   |    |   |
arc(3,6).                %    4   5    6   7
arc(3,7).                %    |___ ____|   |
arc(6,8).                %        |        |
arc(8,9).                %        8        |
arc(7,9).                %        |________|
arc(4,8).                %            |
                         %            9

arc(4,1). % What happens if you add this arc to the graph here? 

%---------------------------------------------------------------
% Path search
%---------------------------------------------------------------

% Version 1: Non-recursive (incomplete)
/*
path(A,B) :- arc(A,B).
path(A,C) :- arc(A,B), arc(B,C).
path(A,D) :- arc(A,B), arc(B,C), arc(C,D).
*/
% ... need more for all paths ...

%---------------------------------------------------------------

% Version 2: Recursive 

/* 
path(A,B) :- arc(A,B).
path(A,C) :- arc(A,B), path(B,C).
*/

% What happens if you add arc(4,1) to the graph?

%---------------------------------------------------------------

% Version 3: return the path in  a list

path(A,B,[B]) :- arc(A,B).
path(A,C,[B|L]) :- arc(A,B), path(B,C,L).

%---------------------------------------------------------------

% Version 4: return the path in a list and avoid infinite loops
/*
path(A,B,Path) :- path(A,B,[A],L),reverse(L,Path).

path(A,B,Path,[B|Path]) :- 
   arc(A,B).

path(A,C,L,Path) :- 
   arc(A,B), 
   \+ member(B,L),          % avoid visited nodes
   path(B,C,[B|L],Path).
*/