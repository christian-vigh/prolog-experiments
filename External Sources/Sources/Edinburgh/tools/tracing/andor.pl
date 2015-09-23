%   File   : andor_debug
%   Author : Dave Bowen (documented by Paul Wilk)
%   Updated: 27 February 1984
%   Purpose: Meta-circular interpreter maintaining extended AND/OR tree
%   Needs  : findall/3 from UTIL:SETOF.PL.


/* MAIN DATA STRUCTURES

A1			=:=	A term instantiated to the current argument
				of the 	current goal being unified.

A2			=:=	A term instantiated to the current argument
				of the current choice being unified.

F			=:= 	The name of the principal functor of the
				current goal or current procedure.	

N			=:= 	The arity of the current goal.

T1			=:= 	A term instantiated to current goal
				being unified.


T2			=:= 	A term instantiated to the current choice
				being unified.

[AndNodes]		=:=	This is a list of "and" goals for the parent
				goal. When AndNodes instantiates to a list, the
				current goal can then be thought of as the
				parent goal.

[AndNode]		=:=	This is a list of one "and" goal and is used
				to catch a parent goal with only one goal in
				its Body.
				
[And | RestA]		=:=	When the parent goal has more than one goal
				in the Body, And instantiates to the first
				goal in the Body and RestA to to the tail.
				And will be used to construct an "or" node
				and RestA will be saved in

Choice			=:=	Choice instantiates to the head of the
				list of or_goals.
				
RestC			=:=	RestC instantiates to the
				tail of the list of or_goals; the choicepoints..

[Clauses]		=:=	The unordered set of all instances of the
				goal-term (Head:-Body); a list.

Cont			=:=	Cont instantiates to the continuation list
				of the parent goal for the current goal.
				Note that this is a list the head of which
				is the term cont/3 the tail is the rest of
				the continuation list.

% [bp(RestC,RestA,Goal,Substs,Cont,ParentFail)|Fail]

Fail			=:=	Holds a list of records, where each record
				contains backtrack information relating to a
				current goal, i.e. [bp/9 | Fail]. Each record
				bp(RestC,RestA,Goal,Substs,Cont,ParentFail)
				

Goals			=:=	Goals consists of a linked list of the goals to
				be satisfied
Head			=:=
OrNodes			=:=	OrNodes is a pointer to a pointer tuple, Or and
				Rest0.
Or			=:=	Or points to "or" nodes. "or" nodes consist
				of a list called Clauses and a list AndNodes.

% cont(RestG, RestO, Parentfail)

RestG			=:= 	continuation list for the parent goal;
				for when all descendant goals have been
				resolved.

RestO			=:= 	list of choice points still to be resolved
				against the current goal; in case the
				current or_goal choice fails.

ParentFail		=:=	Contains the backtracking information for the
				current goal's parent goal; in case the parent
				goal fails.

Substs			=:= 	A list of the bindings made during the
				unification of the current goal.

MidSubsts		=:= 	MidSubsts is used as a local variable in the
				nested call to unify/5 for nested terms.

NewSubsts		=:=	Note that NewSubsts will contain any new
				substitutions made locally in the call to
				unify/5.

Rest			=:= 	The rest of the substitution list.

Tree			=:= 	contains the current state of the search tree.
				The search tree consists of nodes represented
				as terms. There are "and" nodes and "or" nodes
				An "and" term consists of two pointers Goals
				and OrNodes.

Value			=:= 	The value of a unified term; be it an atom
				integer, variable or non-real variable.


% MAIN PROCEDURES

%debug/1

Initialise_Question

	The call to numbervars/3 unifies the variables in the initial question
	with the special term known as $Var; by convention. These variables
	are now referred to as non-real variables. Associated with each
	non-real variable is a unique number. This number is used to
	in the substitution list, to associate a variable with a value.

	Note that Tree is a shared variable occuring as the second and
	seventh argument of and_node. The contents of Tree, the current state
	of the search tree, are built up until a solution is found for the
	initial question, or the question fails. In either case fail/3
	will call ask/1 to see if the user wishes to display the search tree.

%and_node/8.

Exit_Parent

	If the body of the current goal is empty then the second argument
	of and_node, which will usually contain a list of and_goals, will
	be empty. Therefore the parent goalf is proven true; so call
	continue/5.

Reduce_Goal

	If the body of the current goal is NOT empty (i.e. their ARE
	continuation points) then solve the next goal.	

%do_and/8.

Create_AndNode_with_ChoicePoints

	Create an and_node using the current goal, passing forward
	continuation information in the term cont/3:
	copy the continuation list for the
	parent goal; copy the list of choice points which may still
	satisfy the current goal. Go and look for continuation

Create_AndNode_without_ChoicePoints

	Prune at this and_node after satisfying the current goal;
	this is the last continuation for the parent goal.

%do_goal/8.

Cut
	The current goal is a cut therefore commit to the choices made
	since the parent goal was invoked and discard any choices left
	prior to the cut; go to continue to determine if the parent goal
	has any remaining continuation points.

Test_ChoicePoints

	Invariably the choice points  will have been collected together
	and Or will be instantiated to this collection.

Find_ChoicePoints

	Initially,  apart from a cut or system predicate goal, when a new
	current	goal is chosen, the choice points, in the form of
	Clauses	will not yet have been collected together, and Or will be
	uninstantiated.

	Note that Cont instantiates to the continuation list of the
	parent goal for the current goal. Also note the head of the list is a
	is the term cont/3; the tail is the rest of the
	continuation list.

	So, clause(Head, Body) searches the program for a clause whose head
	matches Head. Body  instantiates to the body of this clause.
	findall repeats this for all the instances of the term Head.

	Clauses is a non-empty, unordered list, containing all clause
	instances (Head:-Body) such that clause(Head, Body) is true.

	Note that for the last goal in a parent goal:

	Clauses == [(Head1:-Body1), (Head2:-Body2),...]

	But more generally:

	Clauses ==	[[(Head1:-Body1), (Head1:-Body2),...] |
			[(Head2:-Body1), (Head2:-Body2),...]].

	where each element of the outer list represents a goal to be satisfied;
	collectively they form the current list of and_goals still to be
	satisfied.

	Each element of the inner lists represents an or_goal
	that will match the head of the corresponding and_goal.

	Note that N, the arity of the term is determined here. It is used
	later as a controlling variabl for the unification of terms.

%or_node/8.

Fail_to_MatchClause

	No principal functor of a procedure matched the current goal,
	i.e. the list Clauses is empty, so fail back.

Succeed_to_MatchClause

	A principal functor of a procedure matched the current goal.
		
%do_or/9.

Last_Choice

	There is only one and_goal still to be satisfied in the scope of the
	current goal. We are looking to get a current goal from the list of
	or_goals contained in Clauses.

Not_Last_Choice

	More than one choice left, so Choice instantiates to the current head
	of the list of or_goals and RestC instantiates to the tail.

	We will pass forward all the information relating to choice points
	using the term bp/9 which will instantiate FAIL.

	This information is necessary should the current or_goal choice fail,
	because we wont have to backtrack and look for the next or_goal choice
	as it can be taken from the head of RestC.

	If we backtracked we would loose all the current or_goal choice
	bindings which defeats the purpose of this debugger.

	When the match for and_node is made in do_choice/9 AndNode will
	instantiate to the body of the current or_goal choice.

%do_choice/9.

Unify_Choice

	Now take the chosen or goal apart again.
	If unification against the head of the current_choice is successful
	then this or-goal choice is suitable as an and_goal therefore commit
	to it.

	We won't backtrack looking for any more or_goal choices because we
	collect	ALL of them once the first one succeeds.

	Pass forward the body of the or-goal chosen. This will be used to
	form the next and_node and become the new current goal.

	Consequently, this call to and_node/8  will instantiate And to a
	term pointer which has as its value the head of the
	and_goals for the current or_goal. The current or_goal passed forward
	should now be thought of as the new parent goal.

Unify_Failed

	The or_choice used as the current goal failed to unify so pass
	forward the list of or_goal choices held by Fail (this was
	instantiated in do_or) and the current state of the proof tree
	held by Tree, to fail/3.

	Note that Fail could hold an empty list containing no or_goal
	choicepoints.

%continue/5

Proven_Question

	If continue/5 is called and the list of continuation points
	is empty then a solution has been found. Go and write the
	solution to the question. Ask the user if they require a display of
	the proof tree.

	Note that if another solution is required we
	fail back here and call fail/3.

Find_Continuation

	Usually when continue/5 is called there will be a list containing
	continuation points. The head of the first argument will match with
	cont/3, and the tail, which will be an empty list or contain
	continuation points, will instantiate Cont to be the current
	continuation list. Cont is passed forward to do_and/8.

%fail/3.

Fail
	There were no choice points left for the current and_goal therefore
	Fail consists of an empty list. Ask the user if they want to print
	the proof tree.

Get_ChoicePoint

	We have not exhausted the choicepoints for the current and_goal
	therefore get the head of Fail which contains all the information
	about the next choice point.

	Note that the information describing the unresolved, unordered set of
	choicepoints is contained in the term list bp/6. So, this is passed
	forward to or_goal.

	Also, note that the tail of the list bp becomes the new choice-point
	list, passed forward in Fail.

%unify/4

Test_for_Dereferencing

	The terms to be unified might not be integers or atoms so they may
	need dereferencing. Pass the values to unify_drf/3 for unification.

%deref/3

Found_VariableTerm

	Dereference the non-real varible term; this will be the current goal
	term and the head of the current choice on subsequent calls. Note
	that.

Found_Atom

	The term is an atom or an integer, so simply return its value;
	no dereferencing is required.

%unify_drf/4

Current_Goal_is_RealVariable

	The current goal unifying with the current choice is a
	real variable as yet uninstantiated. So, bind the real variable
	to the variable-number of the non-real variable. Unification of the
	particular term is true. If there are no more terms to be unified in
	the current goal then go to the next and_node. Add this binding to the
	head of the substitution list.

Current_Choice_is_RealVariable

	The current choice made for unifying with the current goal is a
	real variable as yet uninstantiated. So, bind the real variable
	to the number of the non-real variable. Unification of the
	particular term is true. If there are no more terms to be unified in
	the current goal then go to the next and_node. Add this binding
	to the head of the substitution list

Current_Patterns_both_NonVariableTerms

	Both the current goal and current choice or non-variable terms.
	functor/3 returns the arity and name of the principal functor
	of each of the terms. Unification will fail when functor/3 is called
	for the second time if the names of the principal functors
	instantiating the second argument F, are not the same. Or, if they
	are the same, they do not have the same arity.
	If the terms have the same principal functor and arity then we need to
	call unify/5 to see if the
	terms are compound structures, atoms or integers. Note that we
	carry forward the substition list.

%unify/5

Unify_Atom

	The arity of the term is 0 therefore the term is an integer or an
	atom. Or, this  the terminal case of a compound structure as we
	are using the arity returned by functor as a counter for determining
	which current argument we are matching. Note that nothing is added to
	the substitution list if this clause succeeds.

Unify_Structure

	The structures have the same principal functor and arity
	so match the arguments in turn. Note that the arity I
	returned by functor/3 can be used to iterate up the argument lists
	from right to left. arg/3 is used to take the Ith argument
	from the current goal and the current choice. Note that we are
	still carrying the substitution list. Also note that to keep
	the substitutions in the correct order in dealing with the
	further nested structures MidSubsts is used as a local variable
	to this nested call to unify/4. Note that on successful return
	from unify/4, MidSubsts is passed forward. New substitutions
	may or may not have been added. Subsequently, Unify/5 is called
	attempting to match the I-1th argument. Note that NewSubsts
	will contain any new substitutions made, local to the unify/5 call.
	
%derefvar/4

Dereference_Head_of_SubstitutionList

	N is the number of the variable. Unify this with the number of the
	numbered-variable in the head of the substitution list. If
	they unify then we have to find the value of the numbered-variable
	in the head of the substituion list. We call deref/3 to see if
	the binding is to a value or to yet another non-real variable.

Dereference_Tail_of_SubstitutionList

	If the numbered variable in the head of the substitution list fails
	to unify with the number of the current non-real variable then the
	tail of the substitution list is passed to derefvar, referenced by
	Rest. Note that the third argument is used to maintain the completeness
	of the substitution list while searching for the value of the non-real
	variable.

Found_RealVariable

	We have emptied the substitution list therefore the variable is
	unstantiated. So we pass back the value of the last non-real
	variable in the chain as the value to which to bind the
	current	variable term to.
*/

:- public (debug)/1.

:- op(1050,fy,(debug)).


debug Goals :-
	numbervars(Goals,0,N),
	and_node(Goals,Tree,[],[],[],[],Tree,N).



and_node(true,true,Substs,Cont,Fail,ParentFail,Tree,N) :- !,
	continue(Cont,Substs,Fail,Tree,N).
and_node(Goals,and(Goals,OrNodes),Substs,Cont,Fail,ParentFail,Tree,N) :-
	do_and(Goals,OrNodes,Substs,Cont,Fail,ParentFail,Tree,N).


do_and((Goal,RestG),(Or,RestO),Substs,Cont,Fail,ParentFail,Tree,N) :- !,
	do_goal(Goal,Or,Substs,[cont(RestG,RestO,ParentFail)|Cont],
		Fail,ParentFail,Tree,N).
do_and(Goal,Or,Substs,Cont,Fail,ParentFail,Tree,N) :-
	do_goal(Goal,Or,Substs,Cont,Fail,ParentFail,Tree,N).

		
do_goal(!,true,Substs,Cont,Fail,ParentFail,Tree,N) :- !,	% handle cut
	continue(Cont,Substs,ParentFail,Tree,N).
do_goal(Goal,Or,Substs,Cont,Fail,ParentFail,Tree,N) :-
	nonvar(Or), !,						% clauses exist
	or_node(Clauses,Or,Goal,Substs,Cont,Fail,Tree,N).
do_goal(Goal,Or,Substs,Cont,Fail,ParentFail,Tree,N0) :-
	functor(Goal,F,N),					% get clauses
	functor(Head,F,N),
	findall( (Head:-Body),
		 clause(Head,Body),
	         Clauses),
	numbervars(Clauses,N0,N1),
	or_node(Clauses,Or,Goal,Substs,Cont,Fail,Tree,N1).
	
or_node([],fail,Goal,Substs,Cont,Fail,Tree,N) :- !,		% no clauses
	nl, write('Warning: there are no clauses for '),
	write(Goal), nl,
	fail(Fail,Tree,N).
or_node(Clauses,or(Clauses,AndNodes),Goal,Substs,Cont,Fail,Tree,N) :-
	do_or(Clauses,AndNodes,Goal,Substs,Cont,Fail,Fail,Tree,N).

do_or([Choice],[AndNode],Goal,Substs,Cont,Fail,ParentFail,Tree,N) :- !,	% last clause
	do_choice(Choice,AndNode,Goal,Substs,Cont,Fail,ParentFail,Tree,N).
do_or([Choice|RestC],[And|RestA],Goal,Substs,Cont,Fail,ParentFail,Tree,N) :-
	do_choice(Choice,And,Goal,Substs,Cont,
		  [bp(RestC,RestA,Goal,Substs,Cont,ParentFail)|Fail],
		  ParentFail,Tree,N).
	
do_choice((Head:-Body),And,Goal,Substs,Cont,Fail,ParentFail,Tree,N) :-
	unify(Goal,Head,Substs,NewSubsts), !,
	and_node(Body,And,NewSubsts,Cont,Fail,ParentFail,Tree,N).
do_choice(Choice,And,Goal,Substs,Cont,Fail,ParentFail,Tree,N) :-
	fail(Fail,Tree,N).

continue([],Substs,Fail,Tree,N) :- !,
	(   nl, write_substs(Substs,Tree),
	    ask('More'), !,
	    nl, write(yes), nl
	;   fail(Fail,Tree,N)
	).
continue([cont(Goals,OrNodes,ParentFail)|Cont],Substs,Fail,Tree,N) :-
	do_and(Goals,OrNodes,Substs,Cont,Fail,ParentFail,Tree,N).

fail([],Tree,N) :- !,
	nl, write(no), nl,
	( ask('Print tree'), !
	; printm(0,[],Tree)
	).
fail([bp(RestC,RestA,Goal,Substs,Cont,ParentFail)|Fail],Tree,N) :-
	do_or(RestC,RestA,Goal,Substs,Cont,Fail,ParentFail,Tree,N).

unify(T1,T2,Substs,NewSubsts) :-
	deref(T1,Substs,DT1),
	deref(T2,Substs,DT2),
	unify_drf(DT1,DT2,Substs,NewSubsts).

unify_drf(T,'$VAR'(N),Substs,[N=T|Substs]) :- !.
unify_drf('$VAR'(N),T,Substs,[N=T|Substs]) :- !.
unify_drf(T1,T2,Substs,NewSubsts) :-
	functor(T1,F,N),
	functor(T2,F,N),
	unify(N,T1,T2,Substs,NewSubsts).

unify(0,T1,T2,Substs,Substs) :- !.
unify(I,T1,T2,Substs,NewSubsts) :-
	arg(I,T1,A1),
	arg(I,T2,A2),
	unify(A1,A2,Substs,MidSubsts),
	J is I-1,
	unify(J,T1,T2,MidSubsts,NewSubsts).

deref('$VAR'(N),Substs,Value) :- !,
	derefvar(N,Substs,Substs,Value).
deref(X,S,X).

derefvar(N,[N=V1|Rest],Substs,V2) :- !,
	deref(V1,Substs,V2).
derefvar(N,[_|Rest],Substs,V) :- !,
	derefvar(N,Rest,Substs,V).
derefvar(N,[],_,'$VAR'(N)).

/* Outputting routines */

write_substs(Substs,and(Goals,T)) :-
	write('Proved:'), nl,
	printm(0,Substs,Goals),
	( ask('Print tree'), !
	; printm(0,Substs,and(Goals,T))
	).

printm(M,_,V) :- var(V), !,
	write(V).
printm(M,Substs,or(C,A)) :- !,
	N is M+1,
	write('or('), printm_nl(N,Substs,C), put(0',),
	printm_nl(N,Substs,A), put(0')).
printm(M,Substs,and(G,O)) :- !,
	N is M+1,
	write('and('), printm_nl(N,Substs,G), put(0',),
	printm_nl(N,Substs,O), put(0')).
printm(M,Substs,[X|L]) :- !,
	N is M+1,
	write('[ '), printm(N,Substs,X),
	printml(N,Substs,L), write(' ]').
printm(M,Substs,(X,Y)) :- !,
	N is M+1,
	write('( '), printm(N,Substs,X), put(0',),
	printm_nl(N,Substs,Y), write(' )').
printm(M,Substs,X) :-
	dosubsts(X,Substs,V),
	print(V).

printm_nl(M,S,X) :- nl, tab(M*2), printm(M,S,X).

printml(_,_,V) :-
	var(V), !,
	write(' |'), write(V), put(0']).
printml(_,_,[]) :- !.
printml(M,Substs,[X|L]) :-
	put(0',), printm_nl(M,Substs,X),
	printml(M,Substs,L).

dosubsts(Var,S,Var) :- var(Var), !.	% Leave real variable alone
dosubsts('$VAR'(N),S,V) :- !,
	derefvar(N,S,S,T),
	dosubterms(T,S,V).
dosubsts(T,S,V) :-
	functor(T,F,N),
	functor(V,F,N),
	dosubsts(N,T,S,V).

dosubterms('$VAR'(N),_,'$VAR'(N)) :- !.	% unbound variable
dosubterms(T,S,V) :-			% substitute values of vars in T
	dosubsts(T,S,V).

dosubsts(0,T,S,V) :- !.
dosubsts(I,T,S,V) :-
	arg(I,T,Ti),
	arg(I,V,Vi),
	dosubsts(Ti,S,Vi),
	J is I-1,
	dosubsts(J,T,S,V).

/* Ask user question and fail if "y" typed */

ask(Question) :-
	nl, write(Question),
	write(' (y/n)? '),
	ttyflush,
	get0(C),
	skiptonl(C),
	C =\= "y".

skiptonl(31) :- !. % DEC-10
skiptonl(10) :- !. % VAX
skiptonl(_) :- get0(C), skiptonl(C).

