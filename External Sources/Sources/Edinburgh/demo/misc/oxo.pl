%   File   : OXO
%   Author : R.A.O'Keefe
%   Updated: 28 November 1984
%   Purpose: A Noughts-and-Crosses production system.

/*  The point of this is not to play noughts and crosses, possibly the
    world's most boring game, but to illustrate the coding of a tiny
    little set of production rules in Prolog.
*/
:- ['util:ordset.pl'].


store(Fact) :-
	store_list([Fact]).

store_list([]).
store_list([Fact|Facts]) :-
	clause(Fact, true),
%%	write('* '), write(Fact), nl,	% trace repeated fact
	!,
	store_list(Facts).
store_list([Fact|Facts]) :-
	assert(Fact),
%%	write('+ '), write(Fact), nl,	% trace new fact
	setof(NewFact, rule(Fact,NewFact), NewFacts),
	ord_union(NewFacts, Facts, MoreFacts),
	!,
	store_list(MoreFacts).
store_list([_|Facts]) :-
	store_list(Facts).



others(1, 2, 3).	other2(1, 2, 3).	sumto4(1, 3).
others(1, 3, 2).
others(2, 3, 1).	other2(2, 3, 1).	sumto4(2, 2).
others(2, 1, 3).
others(3, 1, 2).	other2(3, 1, 2).	sumto4(3, 1).
others(3, 2, 1).


%   A Player has a winning move if there are two squares in a row.

rule(square(Player,Row,C1), winning(Player,Row,C3)) :-
	square(Player, Row, C2),
	others(C1, C2, C3).

%   A Player has a winning move if there are two in a column.

rule(square(Player,R1,Col), winning(Player,R3,Col)) :-
	square(Player, R2, Col),
	others(R1, R2, R3).

%   A Player has a winning move if there are two on the main diagonal.

rule(square(Player,R1,R1), winning(Player,R3,R3)) :-
	square(Player, R2, R2),
	others(R1, R2, R3).

%   A Player has a winning move if there are two in the other diagonal.

rule(square(Player,R1,C1), winning(Player,R3,C3)) :-
	sumto4(R1, C1),
	others(R1, R2, R3),
	sumto4(R2, C2),
	square(Player, R2, C2),
	sumto4(R3, C3).

%   A Player has won if he has just made a winning move.

rule(square(Player,Row,Col), won(Player)) :-
	winning(Player, Row, Col).

%   The game is a draw if all squares have been played in.

rule(square(_,R,C), drawn) :-
	other2(R, R1, R2),
	square(_, R1, C),
	square(_, R2, C),
	other2(C, C1, C2),
	square(_, R, C1),
	square(_, R, C2),
	square(_, R1, C1),
	square(_, R1, C2),
	square(_, R2, C1),
	square(_, R2, C2).

%   The game is over if it is won or drawn.

rule(won(_), over).
rule(drawn, over).

go(_, O, X) :-
	over,
	!,
	comment(O, X).
go(N, O, X) :-
	repeat,
	    write(N), write(': '), ttyflush,    
	    read(Move),
	    check(Move, R, C),
	!,
	store(square(X, R, C)),
	draw_board,
	reply(N, O, X).

check((Row,Col), Row, Col) :-
	integer(Row), 1 =< Row, Row =< 3,
	integer(Col), 1 =< Col, Col =< 3,
	\+ square(_, Row, Col),
	!.
check(Move, _, _) :-
	write('Already occupied or bad Row,Col.  Try again.'), nl,
	fail.


reply(N, O, X) :-
	over,
	!,
	comment(O, X).
reply(N, O, X) :-
	(   winning(O, R, C)
	;   winning(X, R, C)
	;   others(R, _, _),
	    others(C1, C2, C),
	    square(O, R, C1),
	    \+ square(_, R, C2)
	;   others(C, _, _),
	    others(R1, R2, R),
	    square(O, R1, C),
	    \+ square(_, R2, C)
	;   R = 2, C = 2
	;   others(R, _, _),
	    others(C, _, _)
	),
	\+ square(_, R, C),
	!,
	K is N+1,
	store(square(O, R, C)),
	L is K+1,
	draw_board,
	go(L, O, X).


comment(O, X) :-
	won(X), write('You won.'), nl.
comment(O, X) :-
	won(O), write('I won.'), nl.
comment(O, X) :-
	drawn, write('I didn''t lose.'), nl.


draw_board :-
	contents(1, 1), put(124), contents(1, 2), put(124), contents(1, 3),
	nl, write('---+---+---'), nl,
	contents(2, 1), put(124), contents(2, 2), put(124), contents(2, 3),
	nl, write('---+---+---'), nl,
	contents(3, 1), put(124), contents(3, 2), put(124), contents(3, 3),
	nl, nl.

contents(Row, Col) :-
	square(Player, Row, Col),
	image(Player, Image),
	!,
	write(Image).
contents(_, _) :-
	write('   ').

image(o, ' O ').
image(x, ' X ').


go :-
	abolish(square, 3),
	abolish(winning, 3),
	abolish(won, 1),
	abolish(drawn, 0),
	abolish(over, 0),
	repeat,
	    write('Do you want to play first (x) or second (o)? '),
	    ttyflush,
	    read(X),
	    member(X, [o, x]),
	!,
	write('Enter moves as "Row,Column" pairs'),
	ttynl,
	go(X).

go(o) :- !,
	reply(1, x, o).
go(x) :- !,
	go(1, o, x).


member(X,[X|_]).
member(X,[_|T]) :- member(X,T).
