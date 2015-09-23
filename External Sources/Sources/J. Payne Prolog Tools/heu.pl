%   File   : /usr/lib/prolog/teach/mandc/heuristic
%   Author : Alan Bundy
%   Updated: 27 May 82
%   Purpose: Heuristic Search version of Missionaries and Cannibals.
%   Modified: For NIP, by elimination of singleton variables
%             Ken Johnson    April 1987

go1 :-						/* to solve the m & c problem */
	abolish(known, 2),			/* clear memory */
	assert(known(bank(3,3,1), bank(0,0,0))),	/* record initial state */
	heuristic([triple(5, bank(3,3,1), bank(0,0,0))]).	/* conduct search from it */


/* Agenda is a list of undeveloped states */
heuristic(_) :-
	known(bank(0,0,0), bank(3,3,1)), !,	/* if goal state found */
	nl, write('Goal state found'), nl, nl.	/* then print message and stop */

heuristic([triple(_,Left,Right) | Rest]) :-		/* otherwise */
	nl, write('Consider the state in which:'),
	write_state(Left,Right),			/* print state */
	successors(Left,Right,Rest,NewAgenda),		/* find all successors */
	heuristic(NewAgenda).				/* and recurse */


successors(Left,Right,OldAgenda,NewAgenda) :-	/* to find all successors */
	assert(agenda(OldAgenda)),		/* initiate agenda record */
	keep_trying(Left,Right),		/* make all moves */
	retract(agenda(NewAgenda)).		/* return and delete agenda record */

keep_trying(Left,Right) :-		/* to make all moves */
	make_move(Left,Right,NewLeft,NewRight),		/* make one move */
	evaluate(NewLeft,Score),	/* evaluate new state */
	agenda(Agenda),			/* find current agenda record */
	insert(Agenda,triple(Score,NewLeft,NewRight),NewAgenda),
					/* insert it into agenda */
	assert(agenda(NewAgenda)),	/* record new agenda record */
	retract(agenda(Agenda)),	/* delete old agenda record */
	fail.				/* force backtracking to occur */

keep_trying(_,_).	/* when no move moves left, then succeed */


/* insert state into agenda */
insert([],Triple,[Triple]). 	/* if agenda is empty then make this only member */

insert([triple(Score1,Left1,Right1) | Rest],	/* otherwise */
	triple(Score, Left, Right),		/* put state at front */
	[triple(Score,Left,Right), triple(Score1,Left1,Right1) | Rest]) :-
	Score =< Score1, !.	/* if its score is less than existing front runner */

insert([X|Rest], Triple, [X|NewRest]) :-	/* otherwise */
	insert(Rest,Triple,NewRest).	/* recurse into agenda */

/* evaluate state */
evaluate(bank(M,C,B), Score) :-
	Score is M+C-B.		/* Add up M and C and subtract B */

