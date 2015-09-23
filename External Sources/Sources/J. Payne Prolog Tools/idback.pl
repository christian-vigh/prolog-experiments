%   File   : IDBACK.PL
%   Authors: Luis Moniz Pereira & Antonio Porto.
%   Updated: 8 October 1984
%   Purpose: Intelligent Backtracking DataBase interpreter.

/*  This code comes from the paper
	An Interpreter of Logic Programs Using Selective Backtracking; 
	Luis Moniz Pereira & Antonio Porto,
	Lisbon University report 3/80 CIUNL July 1980
	presented at the Debrecen Logic Programming Workshop 1980.
    That paper presents two interpreters.  One is for full Prolog
    complete with cuts and so on.  This is the one from Appendix 2,
    which is specialised for "data base" working.

    The requirements are these:
    [1] There must be no cuts in the data base or in the question.
    [2] The heads of non-unit clauses may only contain variables.
	Non-unit clauses are stored in a special way.  Head :- Body
	is stored as non_unit(Head, Body).  Read these as definitions
	of "virtual relations".
    [3] Unit clauses and the question contain only variables or ground
	terms as arguments.  In fact, regard this whole thing as a way
	of handling function-free code.  Unit clauses are stored as
	ordinary Prolog, but for each such relation a special linkage
	clause is needed.  For predicate p/n, this clause has the form
	unit(p(A1-N1,...,An-Nn), p(A1,...,An),  [N1,...,Nn]).
	Such a linkage clause should also be provided for such Prolog
	system predicates as are used; see IDBACK.DEF for a suitable
	set to start with.  These predicates should not bind any
	variables.  So <, atom, and so on are ok, but append isn't.
    [4] There are no multiple occurrences of variables in the head of
	any clause, unless all identical occurrences will match a
	ground term.
    [5] Unit clauses are assumed to come before non-unit clauses for
	the same predicate.  Indeed, it would be better if predicates
	were all unit clauses or all non-unit clauses.
    [6] Backtracking into the database query may only be used for
	finding alternative solutions,  not with the intention of
	exhaustively exploring a subspace (e.g. for certain types of
	side-effects).  {I'm not sure what this means.}

    Using the interpreter.
    You can leave your unit clauses as they stand.  Non-unit clauses
    will have to be converted to non_unit/2 clauses.  (non_unit is a
    heterological predicate (:-).)   Instead of call(Q),  use
    data_base_call(Q).  Q may be a simple goal, or a conjunction or
    a disjunction.
*/

:- public
	data_base_call/1.

:- mode
	data_base_query(+),
	    execute_query(+, +, -, +),
	    new_query(+, -, ?),
		new_query(+, +, +, ?), 
		    query_var(+, +, -),
	    no_backtrack_goal_until(+),
	    not_a_backtrack_goal(+),
	    old_query(+, +),
		old_query(+, +, +),
	    select(?),
	    select_goals(+),
	    select_all_modifying_goals_for(+),
	    set_goal_tags(+, +).

/* data
	backtrack_goal(integer).	*/
	%  backtrack_goal(N) can only exist while goal N is somewhere
	%  on the stack, and means that there may be some point in
	%  looking for alternative solutions to that goal.


data_base_call(Query) :-
	new_query(Query, Tagged, Vars),
	abolish(backtrack_goal, 1),
	execute_query(Tagged, 1, _, 0),
	(   old_query(Query, Tagged)
	;   abolish(backtrack_goal, 1),
	    select_all_modifying_goals_for(Vars),
	    fail		%   look for another solution.
	).


%   In this version of new_query, I have combined Pereira & Porto's
%   "copy" and "new_query" predicates.  For queries with few variables
%   (almost any reasonable query), this is much more efficient than
%   putting things into the data base and pulling them out again.
%   The point of this is to make of copy of the query with two changes:
%   all the variables are replaced by new ones, and all the ground
%   arguments are tagged with "t".
%   I've also combined it, in effect, with select_all_modifying_goals_for,
%   so that we can find all the tags of the query at once with no search
%   and no repetition.

new_query((Old1,Old2), (New1,New2), Vars) :- !,
	new_query(Old1, New1, Vars),
	new_query(Old2, New2, Vars).
new_query((Old1;Old2), (New1;New2), Vars) :- !,
	new_query(Old1, New1, Vars),
	new_query(Old2, New2, Vars).
new_query(Old, New, Vars) :-
	functor(Old, F, N),
	functor(New, F, N),
	new_query(N, Old, New, Vars).

new_query(0, _, _, _) :- !.
new_query(N, Old, New, Vars) :-
	M is N-1,
	arg(N, Old, OldArg),
	(   var(OldArg), query_var(Vars, OldArg, NewArg)
	;   NewArg = OldArg-t
	), 
	arg(N, New, NewArg),
	!,
	new_query(M, Old, New, Vars).

query_var(End, Old, New) :-
	var(End), !,
	End = l(Old,New,_).
query_var(l(Var,New,_), Old, New) :-
	Var == Old, !.
query_var(l(_,_,Vars), Old, New) :-
	query_var(Vars, Old, New).


%   old_query takes the instantiated form of the tagged query and unifies
%   it with the original query, stripping the tags as it goes.  There is
%   no need to refer back to the Vars table constructed by new_query as
%   we are unifying with an existing term, not building a new copy.

old_query((Old1,Old2), (New1,New2)) :- !,
	old_query(Old1, New1),
	old_query(Old2, New2).
old_query((Old1;Old2), (New1;New2)) :- !,
	old_query(Old1, New1),
	old_query(Old2, New2).
old_query(Old, New) :-
	functor(Old, F, N),
	functor(New, F, N),
	old_query(N, Old, New).

old_query(0, _, _) :- !.
old_query(N, Old, New) :-
	M is N-1,
	arg(N, New, Arg-_),
	arg(N, Old, Arg), !,
	old_query(M, Old, New).



execute_query((G1,G2), N1, Nn, Parent) :- !,
	execute_query(G1, N1, Nk, Parent),
	(   execute_query(G2, Nk, Nn, Parent)
	;   no_backtrack_goal_until(N1), !, fail
	).
execute_query((G1;G2), N1, Nn, Parent) :- !,
	(   execute_query(G1, N1, Nn, Parent)
	;   execute_query(G2, N1, Nn, Parent)
	).
execute_query(G, N1, Nn, Parent) :-
	unit(G, Goal, Tags),
	!,
	(   call(Goal),
	    (   set_goal_tags(Tags, N1), Nn is N1+1
	    ;   not_a_backtrack_goal(N1), !, fail
	    )
	;   select(Parent), select_goals(Tags), !, fail
	).
execute_query(G, N1, Nn, Parent) :-
	(   non_unit(G, Body),
	    N2 is N1+1, 
	    (   execute_query(Body, N2, Nn, N1)
	    ;   not_a_backtrack_goal(N1), !, fail
	    )
	;   select(Parent), !, fail
	).


set_goal_tags([Tag|Tags], Tag) :- !,
	set_goal_tags(Tags, Tag).
set_goal_tags([_|Tags], Tag) :- !,
	set_goal_tags(Tags, Tag).
set_goal_tags([], _).


%   Goal selection.
%   In the original code this section was devoid of cuts.  The purity
%   was more apparent than real, however, as select or select_goals
%   or whatever was always followed by a cut, or possibly cut,fail.

select(t) :- !.
select(0) :- !.
select(Tag) :-
	backtrack_goal(Tag),
	!.			%   This one has already been selected.
select(Tag) :-
	asserta(backtrack_goal(Tag)).


select_goals([]) :- !.
select_goals([Tag|Tags]) :-
	select(Tag),
	select_goals(Tags).


select_all_modifying_goals_for(End) :-
	var(End), !.
select_all_modifying_goals_for(l(_,_-Tag,Vars)) :-
	select(Tag),
	select_all_modifying_goals_for(Vars).


%   Backtracking control.

no_backtrack_goal_until(N) :-
	%   \+ (backtrack_goal(X), X >= N)
	backtrack_goal(X), X >= N, !, fail ; true.


not_a_backtrack_goal(N) :-
	%   \+ retract(backtrack_goal(N))
	retract(backtrack_goal(N)), !, fail ; true.

