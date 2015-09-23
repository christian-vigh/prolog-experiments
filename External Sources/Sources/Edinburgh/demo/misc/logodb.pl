%   File   : LOGODB
%   Author : Alan Bundy
%   Updated: 16 November 1979
%   Purpose: LOGO-type inference package.

%   Modified for NIP: Ken Johnson 15-4-87
%   In particular this version uses RECORD rather than ASSERT.

%   Make an inference, e.g. "infer(sticky(robin))."

infer(Goal) :-
	recorded(fact,Goal,_).

infer(Goal) :-
	recorded(backward_rule,implies(Ant,Goal),_),
	infer(Ant).

infer(both(Goal1,Goal2)) :-
	infer(Goal1),
	infer(Goal2).


assert_fact(Fact) :-
	record(fact,Fact,_),

	write('Asserting '), write(Fact), nl,

	forall(
		recorded(forward_rule,implies(Fact,Effect),_),
		assert_fact(Effect)
	      ).

assert_fact(_).	% Because FORALL will fail eventually.

forall(Generator, Test) :-
	call(Generator),
	call(Test),
	fail.

