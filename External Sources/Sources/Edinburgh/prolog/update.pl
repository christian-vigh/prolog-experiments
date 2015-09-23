%   File   : UPDATE.PL
%   Author : R.A.O'Keefe
%   Updated: 3 August 1984
%   Purpose: Utilities for updating "data-base" relations.

/*  The idea is that to update all the tuples in a relation you call
	update(r(A,...,Z), new_r(A,...,Z))
    where A...Z must be variables, and new_r is a goal or body which
    computes all the new tuples (unit clauses) including any tuples
    that haven't changed.

    Another thing one sometimes wants to do in data base work is to
    change a set of tuples.  The command for this is
	modify(OldPattern, Transformer, NewPattern).
    What this does is, for each tuple matching OldPattern such that
    Transformer succeeds (once), to replace that tuple by whatever
    the NewPattern turns into.  OldPattern and NewPattern must have
    the same principal functor.  Ideally, we would like the tuples
    to be modified in place.  Unfortunately, there is no way of
    doing that (we would need a "replace(OldRef, Clause, NewRef)"
    command which Prolog hasn't got), so the modified tuples move to
    the front of the table.  That shouldn't worry real relational
    work, as the order isn't supposed to matter.  The relation MUST
    not be modified by the Transformer.
*/

:- public
	modify/3,
	update/2.
:- mode
	modify(+, +, +),
	    modifY(+, +, +),
	update(+, +).

 
update(Template, Generator) :-
	nonvar(Template),		%  check that the Template is well
	functor(Template, Predsym, N),	%  formed; it need not have clauses
	atom(Predsym),			%  but it must be something call likes
	!,
	recorda(., ., Ref),		%  mark the "stack"
	(   call(Generator),		%  for all proofs of Generator
	    recorda(., Template, _),	%  save an instance of Template
	    fail			%  failure-driven loop
	;   abolish(Predsym, N),	%  delete the old relation
	    recorded(., Tuple, DbRef),	%  for all saved instances
	    erase(DbRef),		%  pop it from the "stack"
	    (   DbRef = Ref,		%  if it is the stack mark,
		!			%  we have finished
	    ;   asserta(Tuple),		%  transfer it to the new relation
	        fail			%  failure-driven loop on recorded/3
	    )
	).
update(Template, Generator) :-
	nl, write('! bad argument in '),
	write(update(Template,Generator)), nl,
	fail.



modify(OldPattern, Transformer, NewPattern) :-
	nonvar(OldPattern),
	nonvar(NewPattern),
	functor(OldPattern, Predsym, N),
	functor(NewPattern, Predsym, N),
	atom(Predsym),
	!,
	recorda(., ., Ref),
	(   clause(OldPattern, true),
	    modifY(OldPattern, Transformer, NewPattern),
	    fail
	;   retract(OldPattern),
	    fail
	;   recorded(., Tuple, DbRef),
	    erase(DbRef),
	    (   DbRef = Ref,
		!
	    ;   asserta(Tuple),
		fail
	    )
	).
modify(OldPattern, Transformer, NewPattern) :-
	nl, write('! bad argument in '),
	write(modify(OldPattern,Transformer,NewPattern)), nl,
	fail.


%   Given a Prolog compiler which can handle (A->B;C), this predicate
%   should be unfolded inline as
%   Transformer -> recorda(., NewPattern, _) ; recorda(., OldPattern, _)

modifY(_, Transformer, NewPattern) :-
	call(Transformer),
	!,
	recorda(., NewPattern, _).
modifY(OldPattern, _, _) :-
	recorda(., OldPattern, _).


