/* RULES.

Production Rule System for AI1
Alan Bundy 17.9.81

Use with UTIL. Example rules etc on file SUBTRA and SUM.
*/


go :-			% Top level goal
	apply_rule,	% Apply a production rule
	go.		% and recurse


apply_rule :-				% To apply a rule
	rule(Name,Condition, Action),	% Find a production rule
	satisfied(Condition),		% Check that its condition list is satisfied
	refract(Name,Condition),	% Check that rule has not already been fired
	writef('Rule %t fired\n',[Name]),
	Action.				% If so, run its action

satisfied(Cond1 & Cond2) :- !,			% To satisfy two conditions
	satisfied(Cond1), satisfied(Cond2).	% satisfy one after another

satisfied(Condition) :-					% To check a condition
	short_memory(B1,B2,B3,B4,B5,B6),		% Get short term memory
	memberchk(Condition,[B1,B2,B3,B4,B5,B6]).	% Match condition

add(Item) :-						% To add item to memory
	retract(short_memory(B1,B2,B3,B4,B5,B6)),	% recover & delete memory
	assert(short_memory(Item,B1,B2,B3,B4,B5)),	% add new item & drop B6
	writef('%t remembered\n',[Item]).


refract(Name,Cond) :-				% Refractoriness 
	history(HistList),			% Recall history
	memberchk(pair(Name,Cond),HistList),	% If we have been here before
	!, fail.				% then fail

refract(Name,Cond) :-
	retract(history(HistList)),		% otherwise update history
	assert(history([pair(Name,Cond) | HistList])).

history([]).		% Initial setting for history
