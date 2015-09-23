% evans
% Evan's Geometric Analogy Program - Rational Reconstruction
% Alan Bundy 26.10.79

% top level program

evans(FigA,FigB,FigC,AnsList,Ans) :-
	find_rule(FigA,FigB,Rule), rule_is(Rule),
	apply_rule(Rule,FigC,AnsObjs,AnsRels,Sims),
	ans_desc(AnsObjs,AnsRels,Sims),
	select_result(FigC,AnsList,AnsObjs,AnsRels,Sims,Ans),
	ans_is(Ans).


% find rule given figures

find_rule(FigA,FigB,Rule) :-
	relations(FigA,Source), relations(FigB,Target),
	objects(FigA,Alist), objects(FigB,Blist),
	similarities(FigA,FigB,Triples),
	select_set(Triples,Matches),
	takeaway1(Alist,Matches,Removals),
	takeaway2(Blist,Matches,Adds),
	make_rule(Removals,Adds,Matches,Source,Target,Rule).

%  Apply rule to figc to produce answer
apply_rule(rule(_,Adds,Matches,Source,Target),
		 FigC, AnsObjs,Target,Matches) :-
	relations(FigC,FigDesc), objects(FigC,_),
	seteq(FigDesc,Source),
	maplist(second,Matches,NewList), append(NewList,Adds,AnsObjs).

%  Select Result from those provided 
select_result(FigC,[FigN|_],AnsObjs,AnsRels,AnsSims,FigN) :-
	relations(FigN,NRels), seteq(NRels,AnsRels),
	similarities(FigC,FigN,NSims), seteq(NSims,AnsSims),
	objects(FigN,NObjs), seteq(NObjs,AnsObjs).

select_result(FigC,[_|Rest],AnsObjs,AnsRels,AnsSims,Ans) :-
	select_result(FigC,Rest,AnsObjs,AnsRels,AnsSims,Ans).


% select legal subset of similarity triples for matches
select_set(Triple,Match) :- select_set1([],[],Triple,Match).

select_set1(_,_,[],[]).

select_set1(Aused,Bused,[[Aobj,Bobj,Trans]|Rest],[[Aobj,Bobj,Trans]|Rest1]) :-
	notmember(Aobj,Aused), notmember(Bobj,Bused),
	select_set1([Aobj|Aused],[Bobj|Bused],Rest,Rest1).

select_set1(Aused,Bused,[[_,_,_]|Rest],Rest1) :-
	select_set1(Aused,Bused,Rest,Rest1).


% take away the triples from the list
takeaway1(List,Triples,Ans) :-
	maplist(first,Triples,Firsts), subtract(List,Firsts,Ans).

takeaway2(List,Triples,Ans) :-
	maplist(second,Triples,Seconds), subtract(List,Seconds,Ans).

%  First and second elements of a list 

first([A|_],A).
second([_,B|_],B).


%  Make rule from descriptions inherited from figs a & b
make_rule(Removals,Adds,Matches,Source,Target,Rule) :-
	maplist(first,Matches,Spairs), maplist(second,Matches,Tpairs),
	append(Removals,Spairs,L1), append(L1,Tpairs,L2),
	append(L2,Adds,Consts),
	unbind(Consts,Substs),
	subst(Substs,rule(Removals,Adds,Matches,Source,Target),Rule).

% find corresponding variable for each constant and produce substitution
unbind([],true).

unbind([Const|Rest],both(Const=_,Rest1)) :-
	unbind(Rest,Rest1).


%  Messages 
% ----------

rule_is(rule(Removals,Adds,Matches,Source,Target)) :-
	write('Rule is:'), nl,
	write('remove:      '), write(Removals), nl,
	write('add:         '), write(Adds), nl,
	write('match:       '), write(Matches), nl,
	write('source:      '), write(Source), nl,
	write('target:      '), write(Target), nl.

ans_desc(Objs,Rels,Sims) :-
	write('Answer description is:'), nl,
	write('objects:     '), write(Objs), nl,
	write('relations:   '), write(Rels), nl, 
	write('similarities:'), write(Sims), nl.

ans_is(Ans) :-
	write('Answer is:   '), write(Ans), nl.

%   Supporting utilities
%   --------------------

%   append(Prefix, Suffix, Combined)
%   is true when all three arguments are lists, and the members of Combined
%   are the members of Prefix followed by the members of Suffix.  It may be
%   used to form Combined from a given Prefix and Suffix, or to take a given
%   Combined apart.  E.g. we could define member/2 (from SetUtl.Pl) as
%	member(X, L) :- append(_, [X|_], L).

append([], L, L).
append([H|T], L, [H|R]) :-
	append(T, L, R).


% maplist/3
% written for Nip   Ken Johnson 15-4-87
% Does the same as Lawrence Byrd's Dec-10 version but does not
% use APPLY.
% As an example, define
%
%	one_more(A,B) :-
%		B is A + 1.
%
% Then
%	maplist(one_more, [0,1,2,3], List)
% succeeds and instantiates List to [1,2,3,4]

maplist(_, [], []).

maplist(Pred, [In|Rest_in], [Out|Rest_out]) :-
	functor(Term,Pred,2),
	arg(1, Term, In),
	arg(2, Term, Out),
	call(Term),
	maplist(Pred,Rest_in,Rest_out).

%   memberchk(+Element, +Set)
%   means the same thing, but may only be used to test whether a known
%   Element occurs in a known Set.  In return for this limited use, it
%   is more efficient when it is applicable.

memberchk(Element, [Element|_]) :- !.
memberchk(Element, [_|Rest]) :-
	memberchk(Element, Rest).



%   seteq(+Set1, +Set2)
%   is true when each Set is a subset of the other.  There are two
%   ways of doing this.  One is commented out.

seteq(Set1, Set2) :-
	subset(Set1, Set2),
	subset(Set2, Set1).
%	sort(Set1, Ord1),
%	sort(Set2, Ord2),
%	Ord1 == Ord2.

%   subset(+Set1, +Set2)
%   is true when each member of Set1 occurs in Set2.
%   It can only be used to test two given sets; it cannot be used
%   to generate subsets.  At the moment there is NO predicate for
%   generating subsets, but select/3 takes you part-way.

subset([], _).
subset([Element|Residue], Set) :-
	memberchk(Element, Set), !,
	subset(Residue, Set).


%   subst(Substitution, Term, Result) applies a substitution, where
%   <substitution> ::= <OldTerm> = <NewTerm>
%		    |  <Substitution> & <Substitution>
%		    |  <Substitution> # <Substitution> {deleted ...KJ}
%   The last two possibilities only make sense when the input Term is
%   an equation, and the substitution is a set of solutions.  The
%   "conjunction" of substitutions really refers to back-substitution,
%   and the order in which the substitutions are done may be crucial.
%   If the substitution is ill-formed, and only then, subst will fail.
%
% Originasl by R O'Keefe
% Modded K Johnson for NIP on 15-4-87

subst(both(Subst1,Subst2), Old, New) :-
	subst(Subst1, Old, Mid), !,
	subst(Subst2, Mid, New).

% subst(Subst1 # Subst2, Old, New1 # New2) :-
% 	subst(Subst1, Old, New1), !,
% 	subst(Subst2, Old, New2).

subst(Lhs = Rhs, Old, New) :- !,
	subst(Lhs, Rhs, Old, New).

subst(true, Old, Old).


subst(Lhs, Rhs, Old, Rhs) :-		%   apply substitution
	Old == Lhs, !.

subst(_, _, Old, Old) :-		%   copy unchanged
	var(Old), !.

subst(Lhs, Rhs, Old, New) :-		%   apply to arguments
	functor(Old, Functor, Arity),
	functor(New, Functor, Arity),
	subst(Arity, Lhs, Rhs, Old, New).

subst(0, _, _, _, _) :- !.

subst(N, Lhs, Rhs, Old, New) :-
	arg(N, Old, OldArg),
	subst(Lhs, Rhs, OldArg, NewArg),
	arg(N, New, NewArg),
	M is N-1,
	!,
	subst(M, Lhs, Rhs, Old, New).


%   subtract(+Set1, +Set2, ?Difference)
%   is like intersect, but this time it is the elements of Set1 which
%   *are* in Set2 that are deleted.

subtract([], _, []).
subtract([Element|Residue], Set, Difference) :-
	memberchk(Element, Set), !,
	subtract(Residue, Set, Difference).
subtract([Element|Residue], Set, [Element|Difference]) :-
	subtract(Residue, Set, Difference).

% notmember(Thing,Set)

notmember(_, []) :-
	!.

notmember(X,[X|_]) :-
	!,
	fail.

notmember(X,[_|T]) :-
	notmember(X,T).
