%   File   : /usr/lib/prolog/unfold
%   Author : P.F.Wilk
%   Updated: 31 May 1984
%   Purpose: Unit Resolution: particularly useful for optimizing prolog
%            code written in a data abstraction style; using
%	     single-choice-clause procedures.
%   Notes  : This version ovecomes the problems associated with cut goals.
%	     The philosophy of the program is to unfold what it can when
%	     instructed to do so.
%	     Unfortunately, the way I hacked this program for
%	     cut didn't allow me to generalize the side-effecting for
%	     nested disjunctions. So I will need to rethink this slightly.
%	     The program can be easily changed to output to a file.
%	     There are also some other things I can unfold but haven't got
%	     around too! Also, I haven't managed to collapse all adjacent
%	     cut goals in this verdion.
%	     Watch this space.

% TOP-level: unfold(File).

/* EXAMPLE:

% unfold declarations.

:- unfold extract/3.
:- unfold is_empty/1.
:- unfold is_queue/1.
%:- unfold '='/2.  %C-Prolog doesn't like this it thinks you're redefining E.P's
:- unfold p/2.
:- unfold add_to_head/3.


%single-choice-clause procedures

%X=X. C-Prolog doesn't like this see above.

is_queue(Front-Back).

add_to_head(Elem, Front-Back, [Elem|Front]-Back).

add_to_tail(Elem, Front-[Elem|Back], Front-Back).

extract(Elem, [Elem|Front]-Back, Front-Back).

is_empty(Front-Back) :- Front==Back.

p(X,Y) :- Y=0,!.

%program to be unfolded

qlength(Queue,0) :-
	is_empty(Queue),
	!,
	is_queue(Queue),
	!.
qlength(NewQueue,Queue,0) :-
	X,
	is_empty(Queue),
	!,
	is_queue(Queue),
	!,
	qlength(NewQueue,0),
	NewQueue,
	add_to_head(2,Queue,NewQueue).
qlength(Queue,0) :-
	is_empty(Queue),
	!,
	p(X,Y),
	!,
	qlength(Next,M).
qlength(Queue,N) :-
	extract(_, Queue, Next),
	qlength(Next,M),
	N is M + 1.
qlength(Queue,0) :-
	is_empty(Queue),
	!,
	p(X,Y),
	!,
	is_empty(Queue).
*/
% these are part of the C-Prolog system.
%:-[read.pl].
%:-compile(rdtok.pl).
%:-[setof.pl].
:-[metalog].

:- op(1010, fx, 'unfold').

unfold(File) :-
	see(File),
	seeing(File),
	repeat,
		read(Term,Variables),
		process(Term),
	seen.

process(Term) :-
	Term == end_of_file,
	!.
process((:-unfold Name/Arity)) :-
	assert((unfold(Name,Arity))),
	!,
	fail.
process((Head:-Body)) :-
	!,
	assert((beforecut((Head)))),
	transform_goals(Body,Head,!,TCutBody,TAfterCut),
	merge_goals(TCutBody,TAfterCut,NewClause),
	assert(newclause((NewClause))),
	clause(newclause((NewHead,RestClause)),_),
	assert((NewHead:-RestClause)),
	abolish(newclause,1),
	abolish(beforecut,1),
	abolish(cut,2),
	!,
	fail.
process(Head) :-
	assert(Head),
	!,
	fail.

transform_goals((Goal,RestGoals),CutBody,AfterCut,RestTCutBody,RestTAfterCut) :-!,
	assert((goal(Goal))),
	clause(goal(OldGoal),_),
	abolish(goal,1),
	transform(Goal,TGoal,CutBody,AfterCut,NewAfterCut,NewCutBody),
	test_transformation(NewCutBody,NewAfterCut,OldGoal,Goal,TGoal,NewGoal,TCutBody,TAfterCut),
	transform_goals(RestGoals,TCutBody,TAfterCut,RestTCutBody,RestTAfterCut).
transform_goals((Goal),CutBody,AfterCut,TCutBody,TAfterCut) :-
	assert((goal(Goal))),
	clause(goal(OldGoal),_),
	abolish(goal,1),
	transform(Goal,TGoal,CutBody,AfterCut,NewAfterCut,NewCutBody),
	test_transformation(NewCutBody,NewAfterCut,OldGoal,Goal,TGoal,NewGoal,TCutBody,TAfterCut).

	
transform(Var,TGoal,CutBody,!,NewAfterCut,NewCutBody) :-
	var(Var),
	!,
	TGoal=call(Var),
	concatenate(CutBody,TGoal,NewCutBody),
	NewAfterCut=!.
transform(Var,TGoal,CutBody,(!,AfterCut),NewAfterCut,NewCutBody) :-
	var(Var),
	!,
	TGoal=call(Var),
	concatenate((!,AfterCut),TGoal,NewAfterCut),
	NewCutBody=CutBody.
transform(!,TGoal,CutBody,!,NewAfterCut,NewCutBody) :-
	NewCutBody=CutBody,
	NewAfterCut=!,
	TGoal=!,
	assert((cut(Goal,Goal)
			:- write('WARNING: only unfolded an occurence of the goal '),
			functor(Goal,F,N),
			write(F),
			write('/'),
			write(N),
			nl,
			write_procedure(CutBody),
			nl)),
	!.
transform(!,TGoal,CutBody,(!,AfterCut),NewAfterCut,NewCutBody) :-
	concatenate(CutBody,(!,AfterCut),NewCutBody),
	new_beforecut(NewCutBody,OldCutBody),
	NewAfterCut=!,
	TGoal=!,
	assert((cut(Goal,Goal) :-
			write('WARNING: only unfolded an occurence of the goal '),
			functor(Goal,F,N),
			write(F),
			write('/'),
			write(N),
			nl,
			write_procedure(CutBody),
			write('so be careful not to remove it from the data base.'),
			nl)),
	!.
transform(Goal,TGoal,CutBody,AfterCut,NewAfterCut,NewCutBody) :-
	assert((goal(Goal))),
	functor(Goal, Name, Arity),
	unfold(Name,Arity),!,
	(
		findall((Goal,Body),clause(Goal,Body),List),
		!,
		test_goals(List,Goal,NewList),
		TGoal=NewList

	),
	(
		clause(cut(_,_),_),
		NewCutBody=CutBody,
		concatenate(AfterCut,TGoal,NewAfterCut)
	;
		concatenate(CutBody,TGoal,NewCutBody),NewAfterCut=!
	),
	abolish(goal,1),
	!.
transform(Goal,TGoal,CutBody,AfterCut,NewAfterCut,NewCutBody) :-
	TGoal=Goal,
	(
		clause(cut(_,_),_), NewCutBody=CutBody,
		concatenate(AfterCut,Goal,NewAfterCut),
		!
	;
		concatenate(CutBody,Goal,NewCutBody),
		NewAfterCut=AfterCut
	).

test_goals([(Goal,Body),RestList],Goal,NewList) :-
	clause((goal(OldGoal)),_),
	write('WARNING: more than one choice found when unfolding '),
	nl,
	write(Goal),
	nl,
	write('this occurence not unfolded.'),
	nl,
	NewList=OldGoal.
test_goals([(Goal,Body)],Goal,Body).
test_goals([],Goal,Goal) :-
	write('WARNING: no clause found so far with which to unfold '),
	write(Goal),nl,
	write('this occurence not unfolded.'),
	nl.


merge_goals((Head,TCutBody),!,(RestTCutBody)) :-
	(
		clause(cut(_,_),_),RestTCutBody=(Head,TCutBody,!),
		!
	;
		RestTCutBody=(Head,TCutBody)	
	),
	!.
merge_goals((Head,TCutBody),(!,TAfterCut),(Head,TCutBody,!,TAfterCut)) :-
	!.
merge_goals((Head),!,(RestTCutBody)) :-
	(
		clause(cut(_,_),_),RestTCutBody=(Head,!),
		!
	;
		RestTCutBody=Head	
	),
	!.
merge_goals((Head),(!,TAfterCut),(Head,!,TAfterCut)).

concatenate(Head,true,Head) :-
	!.
concatenate((Head,!),!,(Head,!)) :-
	!.	%TGoal is a ! and firstgoal is a !
concatenate((Head,!),(!,RestAfter),(Head,!,RestAfter)) :-
	!.	%TGoal is a ! followed by more goals and firstgoal is a !
concatenate((!),!,(!)) :-
	!. 	%TGoal is a ! and the Head is a !.
concatenate((!),(!,AfterCut),(!,AfterCut)) :-
	!.	%TGoal is a ! followeb by more goals and Head is a !.
concatenate((Head,CutBody),(!,AfterCut),(Head,CutBody,!,AfterCut)) :-
	!.
concatenate((Head),(!,AfterCut),(Head,!,AfterCut)) :-
	!.
concatenate((Head,CutBody),(TGoal),(Head,CutBody,TGoal)) :-
	!.
concatenate((Head),(TGoal),(Head,TGoal)).


test_transformation(NewCutBody,NewAfterCut,OldGoal,Goal,TGoal,NewGoal,TCutBody,TAfterCut):-
(
	(
		clause(cut(X,X),_),
		clause((beforecut((OldBeforeCut))),_),
		(
			variant((NewCutBody),(OldBeforeCut)),
			NewGoal=TGoal,
			TCutBody=NewCutBody,
			TAfterCut=NewAfterCut,
			new_beforecut(TCutBody,(OldCutBody)),
			!
		;
			cut(Goal,Goal),
			NewGoal=OldGoal,
			TCutBody=OldBeforeCut,
			TAfterCut=NewAfterCut,
			!	
		)
;
	clause((beforecut((OldBeforeCut))),_),
	new_beforecut(NewCutBody,(OldBeforeCut)),
	NewGoal=TGoal,
	TCutBody=NewCutBody,
	TAfterCut=NewAfterCut,
	!
	)
).

%test_body(true,true).
%test_body(Goals,Goals).

