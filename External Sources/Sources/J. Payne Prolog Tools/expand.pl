%   File   : /usr/lib/prolog/expand, UTIL:EXPAND.PL
%   Author : R.A.O'Keefe
%   Updated: Wednesday January 18th, 1984, 0:09:43 am
%   Purpose: Simple macro expansion for Prolog

% How to use this
% Put the macros you want to apply into a file and
% consult the file, as well as consulting this file.
% Then give the command
%	expand(input_file,output_file)
% This will generate an output file in which the macros have been
% expanded. The variable names will all have been replaced by
% underscores with numbers though so the resulting code may be
% enormously hard to check.

/* -------------------- EXAMPLE MACROS	*/

% macro(cons(H, T, [H|T]),	true).
% macro(head([H|T], H),		true).
% macro(tail([H|T], T),		true).
% macro(empty([]),		true).
% macro(positive(X),		X>0).

/* --------------------	EXAMPLE OF THEIR USE */

% append(Prefix, Suffix, Answer) :-
% 	head(Prefix, Head),
% 	tail(Prefix, Tail),
% 	cons(Head, Rest, Answer),
% 	append(Tail, Suffix, Rest).
% 
% append(Prefix, Answer, Answer) :-
% 	empty(Prefix).
% 
% 
% member(Element, List) :-
% 	head(List, Element).
% member(Element, List) :-
% 	tail(List, Rest),
% 	member(Element, Rest).
% 
% bug(Var) :-			% bug([2|_]) is meant to succeed
% 	nonvar(Var), !,		% bug(X) is meant to bind X to 'var'
% 	head(Var, 2).		% bug(any other non-variable) is
% bug(var).			% meant to fail
% 
% 
% greater(X, Y) :-
% 	Z is Y-Z,
%	positive(Z).
% 
%  -------------------- END EXAMPLES */
	
expand(OldFile, NewFile) :-
	see(OldFile),
	tell(NewFile),
	repeat,
		read(Term),
		expand_term(Term, Expanded),	%  Prolog built-in predicate
		expand_macros(Expanded),
	!,
	seen,
	told.


expand_macros(end_of_file).

expand_macros((Head :- OldBody)) :-
	expand_macros(OldBody, NewBody),
	writeq((Head :- NewBody)), put(46), nl,
	!, fail.		%  drive the repeat loop

expand_macros((:- OldBody)) :-
	expand_macros(OldBody, NewBody),
	writeq((:- NewBody)), put(46), nl,
	!, fail.		%  drive the repeat loop

expand_macros((?- OldBody)) :-
	expand_macros(OldBody, NewBody),
	writeq((?- NewBody)), put(46), nl,
	!, fail.		%  drive the repeat loop

expand_macros(Head) :-
	writeq(Head), put(46), nl,
	!, fail.


expand_macros(Var, call(Var)) :-
	var(Var), !.

expand_macros((OldA,OldB), Answer) :- !,
	expand_macros(OldA, NewA),
	expand_macros(OldB, NewB),
	get_rid_of_extra_true(NewA, NewB, Answer).

expand_macros((OldA;OldB), (NewA;NewB)) :- !,
	expand_macros(OldA, NewA),
	expand_macros(OldB, NewB).

expand_macros((OldA->OldB), (NewA->NewB)) :- !,
	expand_macros(OldA, NewA),
	expand_macros(OldB, NewB).

expand_macros(forall(OldA,OldB), forall(NewA,NewB)) :- !,
	expand_macros(OldA, NewA),
	expand_macros(OldB, NewB).

expand_macros(not(Old), \+(New)) :- !,
	expand_macros(Old, New).

expand_macros(\+(Old), \+(New)) :- !,
	expand_macros(Old, New).

expand_macros(Old, New) :-
	macro(Old, New),
	!.			% FORCE a unique expansion.

expand_macros(Old, Old).	% Not a macro.


%   Macros very often turn into 'true', and clauses that come out
%   looking like ... :- true,true,true,true,true. would be silly.
%   So if one member of a conjunction is 'true' we discard it.  A
%   final 'true' as in "p :- true" does no harm as Prolog will do
%   any removal necessary.  We **can't** remove 'true' disjuncts,
%   though we could in logic, as that would change the behaviour
%   of the program by backtracking a different number of times.

get_rid_of_extra_true(true, X, X) :- !.
get_rid_of_extra_true(X, true, X) :- !.


