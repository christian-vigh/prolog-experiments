%   File   : /usr/lib/prolog/search/eight_puzzle
%   Author : R.A.O'Keefe
%   Updated: 12 December 1983
%   Purpose: illustrate the searching methods


/*  The illustration I have chosen is the well known 8-puzzle.
    The state of the game is represented by a tuple of 9 labels,
    1 to 8 representing the movable tiles and x representing an
    empty square, together with an integer between 1 and 9 which
    says where the empty square is.  The operations are moving
    the empty square u(p), d(own), l(left), or r(right).
*/

solution(5/b(
	1,2,3,
	8,x,4,
	7,6,5)	).

starting_position(9/b(
	1,2,3,
	7,8,4,
	6,5,x)	).

equivalent(X, X).

operator_applies(Operator, OldX/OldB, NewX/NewB) :-
	operator_ok(Operator, OldX, NewX),
	new_board(OldX, OldB, NewX, NewB).

operator_ok(u, OldX, NewX) :- OldX > 3, NewX is OldX-3.
operator_ok(d, OldX, NewX) :- OldX < 7, NewX is OldX+3.
operator_ok(l, OldX, NewX) :- OldX mod 3 =\= 1, NewX is OldX-1.
operator_ok(r, OldX, NewX) :- OldX mod 3 =\= 0, NewX is OldX+1.


%   new_board(OldX, OldB, NewX, NewB)
%   creates a New Board which is essentially the same as the Old Board,
%   except that the labels at the Old and New X positions have been
%   swapped.

new_board(OldX, OldB, NewX, NewB) :-
	functor(OldB, F, N),
	functor(NewB, F, N),
	arg(OldX, OldB, x),
	arg(NewX, OldB, L),	%  L is a label 1..8
	arg(OldX, NewB, L),
	arg(NewX, NewB, x),
	new_board(N, OldB, NewB).

new_board(0, _, _) :- !.
new_board(N, OldB, NewB) :-
	arg(N, NewB, Lab),
	var(Lab),
	!,
	arg(N, OldB, Lab),
	M is N-1,
	new_board(M, OldB, NewB).
new_board(N, OldB, NewB) :-
	M is N-1,
	new_board(M, OldB, NewB).


distance(X1/Board1, Distance) :-
	solution(X2/Board2),
	distance(9, Board1, Board2, 0, Distance).

distance(0, _, _, Distance, Distance) :- !.
distance(N, Board1, Board2, SoFar, Distance) :-
	arg(N, Board1, Piece),
	arg(N, Board2, Piece),
	!,
	M is N-1,
	distance(M, Board1, Board2, SoFar, Distance).
distance(N, Board1, Board2, SoFar, Distance) :-
	M is N-1,
	Accum is SoFar+1,
	distance(M, Board1, Board2, Accum, Distance).



