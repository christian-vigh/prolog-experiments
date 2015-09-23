%   File   : DECONS.PL
%   Author : R.A.O'Keefe
%   Updated: 23 July 1984
%   Purpose: Construct and take apart Prolog control structures.

:- public
	prolog_bounded_quantification/3,
	prolog_clause/3,
	prolog_conjunction/2,
	prolog_disjunction/2,
	prolog_if_branch/3,
	prolog_negation/2.

:- mode
	prolog_bounded_quantification(?, ?, ?),
	prolog_clause/3,
	prolog_conjunction(?, ?),
	prolog_disjunction(?, ?),
	prolog_if_branch(?, ?, ?),
	prolog_negation(?, ?),
	pl_explode(+, +, +, -),
	pl_explode(+, +, +, -, ?),
	pl_implode(+, +, +, -),
	pl_implode2(+, +, +, -).



%   prolog_bounded_quantification(Form, Generator, Test)
%   handles the syntax of forall(Gen, Test).

prolog_bounded_quantification(forall(G,T), G, T) :- !.
prolog_bounded_quantification(\+ (G, \+ T), G, T) :- !.
prolog_bounded_quantification((G, (T->fail;true) -> fail; true), G, T).



%   prolog_clause(Clause, Head, Body)
%   handles the syntax of clauses.  Note that it is not used to
%   recognise whether a term is a clause or not; almost any Prolog
%   term can serve as a (unit) clause.  If is for building a clause
%   given the head and body, or for taking something known to be a
%   clause apart.

prolog_clause(Head, Head, true) :-
	Head \= (_ :- _), !.
prolog_clause((Head:-Body), Head, Body).



%   prolog_if_branch(Branch, Hypothesis, Conclusion)
%   handles the syntax of individual arms of if-then-elses.

prolog_if_branch((H->C), H, C).



%   prolog_negation(NegatedForm, PositiveForm) recognises
%   and/or generates negations.

prolog_negation(\+X, X) :- !.
prolog_negation((X->fail;true), X).



%   prolog_conjunction(Conjunction, ListOfConjuncts)
%   handles the syntax of conjuncts.  This code wraps call(_) around
%   variables, flattens conjunctions to (A;(B;(C;(D;E)))) form, and
%   drops "true" conjuncts.

prolog_conjunction(Conjunction, ListOfConjuncts) :-
	nonvar(Conjunction),
	!,
	functor(Conjunction, ',', 2),
	pl_explode(Conjunction, ',', 'true', L, []),
	ListOfConjuncts = L.
prolog_conjunction(Conjunction, ListOfConjuncts) :-
	pl_explode(ListOfConjuncts, ',', 'true', L0),
	pl_implode(L0, ',', 'true', Conjunction).



%   prolog_disjunction(Disjunction, ListOfDisjuncts)
%   handles the syntax of disjuncts.  This code wraps call(_) around
%   variables, flattens disjunctions to (A,(B,(C,(D,E)))) form, and
%   drops "false" disjuncts.

prolog_disjunction(Disjunction, ListOfDisjuncts) :-
	nonvar(Disjunction),
	!,
	functor(Disjunction, ';', 2),
	pl_explode(Disjunction, ';', 'fail', L, []),
	ListOfDisjuncts = L.
prolog_disjunction(Disjunction, ListOfDisjuncts) :-
	pl_explode(ListOfDisjuncts, ';', 'fail', L0),
	pl_implode(L0, ';', 'fail', Disjunction).



%   pl_explode(Form, Op, Zero, L0, L)
%   flattens a binary tree built using Op into a list between L0 and L,
%   eliminating Zero nodes, and wrapping call(_) around variable nodes.

pl_explode(V, _, _, [call(V)|L], L) :-
	var(V),
	!.
pl_explode(Z, _, Z, L, L) :- !.
pl_explode(F, O, Z, L0, L) :-
	F =.. [O,A,B],
	!,
	pl_explode(A, O, Z, L0, L1),
	pl_explode(B, O, Z, L1, L).
pl_explode(F, _, _, [F|L], L).



%   pl_explode(List, Op, Zero, L)
%   flattens each of the elements of List using pl_explode/5
%   and forms the result into a big list L.

pl_explode([], _, _, []) :- !.
pl_explode([H|T], O, Z, L0) :-
	pl_explode(H, O, Z, L0, L),
	pl_explode(T, O, Z, L).



%   pl_implode(List, Op, Zero, Tree)
%   forms the list [F1,...,Fn] into the tree Op(F1,Op(...Op(_,Fn))).

pl_implode([], _, Z, Z).
pl_implode([H|T], O, Z, Form) :-
	pl_implode2(T, Z, H, Form).

pl_implode2([], _, F, F).
pl_implode2([H|T], O, X, F) :-
	F =.. [O,X,Y],
	pl_implode2(T, O, H, Y).


