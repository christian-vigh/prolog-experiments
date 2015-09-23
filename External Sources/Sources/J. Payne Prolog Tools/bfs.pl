%   File   : /usr/lib/prolog/teach/mandc/breadth1
%   Author : Alan Bundy
%   Updated: 24 June 82
%   Purpose: Breadth First Search version of Missionaries and Cannibals,
%	     Revised version.
%   Mod	   : Elimination of singleton variables for NIP
%          : Ken Johnson, April 1987

go2 :-					/* to solve the m & c problem */
	abolish(state, 3),		/* clear memory */
	assert(state(0,bank(3,3,1), bank(0,0,0))),	/* record initial state */
	breadth(0).			/* conduct search from it */


/* to develop breadth first search */
breadth(_) :-
	state(_,bank(0,0,0), bank(3,3,1)), !,		/* if goal state found */
	nl, write('Goal state found'), nl, nl.		/* print message and stop */

breadth(Depth) :-			/* otherwise */
	NewDepth is Depth+1,		/* increment depth */
	state(Depth,Left,Right),	/* pick a state at correct depth */
	nl, write('Consider the state in which:'),
	write_state(Left,Right),	/* print state */
	make_move(Left,Right,NewDepth,_,_),	/*make one move */
	fail.				/* force backtracking to occur */

breadth(Depth) :-			/* when old depth is exhausted */
	NewDepth is Depth+1,		/* increment depth */
	breadth(NewDepth).		/* recurse at new depth */

make_move(Left,Right,NewDepth,NewLeft,NewRight) :-	/* to make a move */
	apply_move(Left,Right,NewLeft,NewRight), 	/* apply a move */
	legal(NewLeft), legal(NewRight),		/* check it is legal */
	\+ state(_,NewLeft,NewRight),			/* and a new state */
	assert(state(NewDepth,NewLeft,NewRight)).	/* record it */


