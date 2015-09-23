%   File   : /usr/lib/prolog/search/depth_first
%   Author : R.A.O'Keefe
%   Updated: 21 December 1983
%   Purpose: define a schema for depth first search

%   This schema has four parameters:
%	starting_position(Start)
%		binds Start to the first position to try
%	solution(Position)
%		tests whether a Position is a solution or not
%	operator_applies(Operator, OldPosition, NewPosition)
%		enumerates all the operators which apply to the
%		OldPosition, and also gives the NewPosition which
%		results from that operator application.
%	equivalent(Pos1, Pos2)
%		tests whether the two positions are essentially
%		the same.  The idea is that we will only look at
%		a position once.

%   depth_first_search(DepthBound, Position, OperatorList)
%	returns the first solution it can find, and the list of
%	Operators which produced it: [O1,...,On] means that
%	applying O1 to the start position, then O2, then ... and
%	finally On produces Position.  The number of operators n
%	will not exceed DepthBound.  You may omit DepthBound, in
%	which case it is taken to be a very large number.
%	The eight-puzzle problem has a solution at depth 4, so 4
%	is a good depth to try.  I have tried depths up to 8 and
%	got an answer in a reasonable time.


depth_first_search(Position, History) :-
	depth_first_search(8'377777, Position, History).


depth_first_search(DepthBound, Position, History) :-
	starting_position(Start),
	depth_first_search([d(DepthBound,Start,[])], [], Position, History).


depth_first_search([d(_,Position,OpList)|_], _, Position, OpList) :-
	solution(Position),
	!.	%  assuming you want only one
depth_first_search([d(_,Position,_)|Rest], Seen, Answer, History) :-
	member(OldPos, Seen),
	equivalent(OldPos, Position),
	!,
	depth_first_search(Rest, Seen, Answer, History).
depth_first_search([d(0,_,_)|Rest], Seen, Answer, History) :- !,
	depth_first_search(Rest, Seen, Answer, History).
depth_first_search([d(Bound,Position,OpList)|Rest], Seen, Answer, History) :-
	findall(Op, NP^operator_applies(Op, Position, NP), Ops),
	NewBound is Bound-1,
	fill_out(Ops, NewBound, Position, OpList, Descendants),
	append(Descendants, Rest, NewRest),
	NewSeen = [Position|Seen],
	!,
	depth_first_search(NewRest, NewSeen, Answer, History).


fill_out([], _, _, _, []) :- !.
fill_out([Op|Ops], Bound, Position, OpList, [d(Bound,NewPos,[Op|OpList])|Rest]) :-
	operator_applies(Op, Position, NewPos), !,
	fill_out(Ops, Bound, Position, OpList, Rest).



