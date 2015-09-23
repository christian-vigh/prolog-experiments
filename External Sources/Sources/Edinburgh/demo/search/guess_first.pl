%   File   : /usr/lib/prolog/search/guess_first
%   Author : R.A.O'Keefe
%   Updated: 21 December 1983
%   Purpose: define a schema for guess first search

%   This schema has five parameters:
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
%	distance(Position, Distance)
%		returns an estimate of how far the Position is
%		from a solution.  This is only used to rank the
%		descendants of a node, so the actual values of
%		the estimate don't matter too much.  See BEST
%		for a method where the values *do* matter.

%   guess_first_search(Position, OperatorList)
%	returns the first solution it can find, and the list of
%	Operators which produced it: [O1,...,On] means that
%	applying O1 to the start position, then O2, then ... and
%	finally On produces Position.


guess_first_search(Position, History) :-
	starting_position(Start),
	guess_first_search([Start-[]], [Start], Position, History).


guess_first_search([Position-OpList|_], _, Position, OpList) :-
	solution(Position),
	!.	%  assuming you want only one
guess_first_search([Position-OpList|Rest], Seen, Answer, History) :-
	findall(Operator, new_position(Operator, Position, Seen), Ops),
	%   we can't use setof, because that fails when there is no such Op
	fill_out(Ops, Position, OpList, Seen, NewSeen, Descendants),
	rank(Descendants, ByOrderOfGuess),
	append(ByOrderOfGuess, Rest, NewRest),
	!,
	guess_first_search(NewRest, NewSeen, Answer, History).


new_position(Operator, Position, Seen) :-
	operator_applies(Operator, Position, NewPos),
	\+ (
	    member(OldPos, Seen),
	    equivalent(OldPos, NewPos)
	).


fill_out([], _, _, Seen, Seen, []) :- !.
fill_out([Op|Ops], Position, OpList, Seen, NewSeen, 
		[Distance-(NewPos-[Op|OpList])|New]) :-
	operator_applies(Op, Position, NewPos),
	distance(NewPos, Distance), !,
	fill_out(Ops, Position, OpList, [NewPos|Seen], NewSeen, New).


rank(Keyed, Ranked) :-
	keysort(Keyed, Sorted),
	strip(Sorted, Ranked).

strip([], []) :- !.
strip([_-H|T], [H|R]) :-
	strip(T, R).
