/* SUBTRA.

Production Rules for Subtraction by Decomposition
from O'Shea and Young WP42
Alan Bundy 22.9.81

Example for RULES, use with UTIL
*/

/* The Rules */

rule(fin,next_column, shift_left & take_diff & write_answer & abort).
rule(b2a,s_gtr_m, add(borrow)).
rule(b2c,s_eq_m, result(0) & add(next_column)).
rule(bs2,borrow, decrement).
rule(bs3,borrow, add_ten_to_m).
rule(cm,process_column, compare).
rule(ts,process_column, take_diff & add(next_column)).



/* The Actions */

shift_left :-
	retract(mark(X)),
	X1 is X+1,
	assert(mark(X1)).

take_diff :-
	mark(X),
	column(X,M,S),
	pos_diff(M,S,R),
	assert(answer(X,R)).

pos_diff(M,S,R) :-
	M>=S, !, R is M-S.

pos_diff(M,S,R) :-
	R is S-M.
	
result(R) :-
	mark(X),
	assert(answer(X,R)).

add_ten_to_m :-
	mark(X),
	retract(column(X,M,S)),
	M1 is M+10,
	assert(column(X1,M1,S)).

decrement :-
	mark(X), X1 is X+1,
	retract(column(X1,M,S)),
	M1 is M-1,
	assert(column(X1,M1,S)).

compare :-
	mark(X),
	column(X,M,S),
	compare1(M,S,Verdict),
	add(Verdict).

compare1(M,M,s_eq_m).
compare1(M,S,s_gtr_m) :- S>M.
compare1(M,S,m_gtr_s) :- M>S.


write_answer :-
	column(1,M1,S1), answer(1,R1),
	column(2,M2,S2), answer(2,R2),
	writef('%t %t\n%t %t\n---\n%t %t\n---\n\n',
		[M2,M1,S2,S1,R2,R1]).


/* Short Term Memory */
short_memory(process_column,nil,nil,nil,nil,nil).
