%   File   : ADVICE.PL
%   Author : R.A.O'Keefe
%   Updated: 20 August 1984
%   Purpose: Interlisp-like advice package.



%   This module defines three commands and a predicate:

%	advise(Predicate, Port, Action)
%	unadvise(Predicate)
%	unadvise(Predicate, Port)
%	advised(Prediate)

%   which cause the Action to be performed at that Port (advise),
%   cause no action to be performed at that Port (unadvise), or
%   test whether a predicate is advised.

%   The Predicate argument must be a Prolog term.  a/2 will be taken
%   as referring to the predicate (/)/2, which is probably not what
%   you want.  Write a(_,_).  This is the same convention as that
%   used by current_predicate.  For advise/3 the arguments will be
%   retained and may be used as a further condition; the other two
%   predicates ignore the arguments except to note how many they are.

%   The Port argument may be call, exit, redo, or fail.
%   This corresponds exactly to the ports shown by the debugger.
%   Note that the "neck" port of some debuggers cannot be handled in
%   this indirect fashion.  Advice should be integrated with the
%   debugger so that spy-points are handled this way, perhaps as
%   advise(Goal, call, spy) or some such.

%   Unlike the InterLISP facility on which this is based, it is
%   not possible to advise built-in predicates.


%----------------------------------------------------------------------------%

%...advised(Goal)
%   is true when Goal is a Prolog goal whose predicate is under advice.
%   The Goal may have some of its arguments filled in, that doesn't 
%   matter.  This predicate may be used to enumerate the predicates
%   being advised, in the spirit of current_predicate.

advised(Goal) :-
	current_predicate(_, Goal),
	clause(Goal, 'a$call'(Goal,_)).



%...advised(Goal, Skel, Call)
%   is a version of advised/1 which returns the general skeleton for
%   goal, and if the goal is ill-formed or not being advised, prints
%   an error message.  It may NOT be used to enumerate goals, but as
%   you are not supposed to know about it, that is acceptable.  As a
%   result of picking up the mapped call from the clause, only advise
%   needs to know how the new name is generated.

advised(Goal, Skel, Call) :-
	functor(Goal, Functor, Arity),
	functor(Skel, Functor, Arity),
	clause(Skel, 'a$call'(Skel,Call)),
	!.
advised(Goal, _, _) :-
	display('! You are not advising '),
	display(Goal),
	ttynl,
	fail.



%----------------------------------------------------------------------------%

%...advise(Goal, Port, Action)
%   makes sure that the Goal is being advised, and then adds an
%   entry to its advice table.  On the Dec-10 and in C-Prolog, we
%   are able to put the advice "on the Goal's property list" by
%   using recorda/z.

advise(Goal, Port, Action) :-
	'a$fail'(Port),
	functor(Goal, Functor, Arity),
	functor(Skel, Functor, Arity),
	advise(Goal, Skel),
	recordz(Skel, advice(Port,Goal,Action), _).


%...a$fail(Port)
%   checks that the Port is a valid port name.

'a$fail'(Var) :-
	var(Var),
	!,
	display('! Variable as port name in advise/unadvise'),
	ttynl,
	fail.
'a$fail'(call) :- !.
'a$fail'(exit) :- !.
'a$fail'(redo) :- !.
'a$fail'(fail) :- !.
'a$fail'(Port) :-
	display('! unknown port name '),
	display(Port),
	ttynl.



%...advise(Goal)
%   takes a skeletal Goal, e.g. f(A,B,C), and makes sure that calls
%   to that goal will be routed to e.g. a$f(A,B,C) via a$call.  If
%   the goal is advised already, nothing is done.  If there are no
%   clauses for this goal, an error is announced (as this usually
%   indicates a typing mistake or a system predicate.  Otherwise
%   all the current clauses are renamed, and a new clause
%	f(A, B, C) :- a$call(f(A,B,C), a$f(A,B,C)).
%   is added.  BEWARE: this will not work if you change the predicate
%   using assert/retract.  It would have to be built in to the system
%   at a much lower level for that to work.

advise(_, Skel) :-				% already advised?
	clause(Skel, 'a$call'(Skel,_)),
	!.

advise(_, Skel) :-				% has clauses?
	current_predicate(Functor, Skel),
	!,
	concat('a$', Functor, Afunctor),
	Skel =.. [Functor |Args],
	Call =.. [Afunctor|Args],
	(   clause(Skel, Body, Ref),
	    assertz((Call :- Body)),
	    erase(Ref),
	    fail
	;   true
	),  !,
	assert((Skel :- 'a$call'(Skel,Call))).

advise(Goal, _) :-
	display('! You have no clauses for '),
	display(Goal),
	ttynl,
	fail.

advise(Pred_name) :-			% An addition KJ 26-6-87
	current_predicate(Pred_name, Skel),

	default_message(Pred_name, call, Message_for_call),
	advise(Skel, call, Message_for_call),
	default_message(Pred_name, redo, Message_for_redo),
	advise(Skel, redo, Message_for_redo),
	default_message(Pred_name, fail, Message_for_fail),
	advise(Skel, fail, Message_for_fail),
	default_message(Pred_name, exit, Message_for_exit),
	advise(Skel, exit, Message_for_exit).

default_message(Pred_name, Port, Message) :-
	name(Pred_name,Pred_chars),
	name(Port, Port_chars),
	append(Pred_chars, [58,32|Port_chars],C1),	% 58=';', 32=' '
	append(C1, [10], C2),
	name(Message,C2).

%----------------------------------------------------------------------------%

%...unadvise(Goal)
%   wipes out all of the advice for the Goal.

unadvise(Goal) :-
	advised(Goal, Skel, Call),
	retract((Skel :- 'a$call'(Skel,Call))),
	(   clause(Call, Body, Ref),
	    assertz((Skel :- Body)),
	    erase(Ref),
	    fail
	;   true
	),  !,
	'a$abolish'(Skel, _).


%...unadvise(Goal, Port)
%   wipes out all the advice for Goal saying what to do at this Port.

unadvise(Goal, Port) :-
	'a$fail'(Port),			% check that the Port is valid.
	advised(Goal, Skel, _),		% validate the Goal.
	'a$abolish'(Skel, Port).



%...a$abolish(Goal, Port)
%   wipes out all the advice for this port of the goal, given
%   that the goal and port have been validated.

'a$abolish'(Goal, Port) :-
	recorded(Goal, advice(Port,_,_), Ref),
	erase(Ref),
	fail.
'a$abolish'(_, _).



%...a$call(SourceGoal, MappedGoal)
%   routes a call on SourceGoal to call MappedGoal instead, but obeys
%   any advice that may be lying around.

%	'a$call'(Goal, Call) :-
%		!,
%		call(Call).

'a$call'(Goal, Call) :-
	(   recorded(Goal, advice(call,Goal,Action), _),
	    call(Action),
	    fail
	;
	    call(Call)
	;
	    recorded(Goal, advice(fail,Goal,Action), _),
	    call(Action),
	    fail
	),
	(   recorded(Goal, advice(exit,Goal,Action), _),
	    call(Action),
	    fail
	;
	    true
	;
	    recorded(Goal, advice(redo,Goal,Action), _),
	    call(Action),
	    fail
	).


/*----------------------------------------------------------------------+
|									|
|	pa		prints all advice				|
|	pa help		prints help about the advice package		|
|	pa Preds	prints the advice for Preds, where Preds	|
|			is the same sort of specification pp takes.	|
|	To use these commands you must have PP.PL loaded.		|
|									|
+----------------------------------------------------------------------*/

pa :-
	flag(advice, Old, Old),
	write('% The advice flag is '), write(Old), nl,
	advised(Pred),
	functor(Pred, F, N),
	pa_explicit(F/N),
	fail ; true.


pa(help) :- !,
	give_help('advice.hlp').
pa(Pattern) :-
	setof(Predicate, isCurrent(Pattern, pp, Predicate), Predicates),
	pa_explicit(Predicates).


pa_explicit([Head|Tail]) :-
	pa_explicit(Head),
	pa_explicit(Tail).
pa_explicit([]).
pa_explicit(Functor/Arity) :-
	functor(Head, Functor, Arity),
	write('% '), write(Functor/Arity),
	(   clause(Head, 'a$call'(Head,_)), !, nl,
	    (   recorded(Head, advice(Port,Head,Action), _),
		write(:-(advise(Head,Port,Action))), put(46), nl,
		fail
	    ;   true
	    )
	;   write(' is not advised'), nl
	).


concat(Atom1,Atom2,Atom12) :-
	name(Atom1,Chars1),
	name(Atom2,Chars2),
	append(Chars1,Chars2,Chars12),
	name(Atom12,Chars12).

append([],X,X).

append([H|T],U,[H|V]) :-
	append(T,U,V).

