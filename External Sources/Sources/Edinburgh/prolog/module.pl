%   File   : MODULE.PL
%   Author : R.A.O'Keefe
%   Updated: 16 October 1984
%   Purpose: Elementary module system for Dec-10 Prolog.
%   Needs  : append/3 from LISTUT.PL, writef/2 & fwritef/3 from WRITEF.PL,
%   	     and portable_writeq/1 from WRITE.PL (should be portray_clause)

/*  ASSUMING that the user's program has no atoms with colons
    in their names (other than the standard predicate =:=),
    this file simulates a module facility in Dec-10 Prolog
    by renaming predicates.  A predicate foo/5 belonging to
    module baz will be called 'baz:foo'/5.

    This is NOT meant to be a spectacularly good module
    system.  It is a sort of "least common denominator".
    The BSI standard <<MAY>> include something like this as
    "level 1" of its module specifications (level 0 will of
    course be no modules at all).  I have no idea what level
    2 will look like.  Any reasonable module system should be
    able to imitate this, and this will do until a real module
    system comes along.  The main thing it buys you is that it
    greatly reduces the likelihood of name collision.

    A module is identified with a file.
    You say :- translate(module), where module is a Prolog atom,	
    and it translates a "module.MPL" file to an 'equvialent'
    "module.PL" file.  "module" will be taken as the name of the
    module, unless it starts with

	:- module ModuleName.

    There are a number of declarations which are understood:

	:- export p1/n1, ..., pk/nk.

	Declares that these predicates are to be exported.
	This means
	(a) that there MUST be at least one clause for each of
	    these predicates in the module.
	(b) that these predicates MAY not be system or imported.
	(c) that any goal referring to any of them will keep
	    its current name.
	(d) there should be a :- public declaration.

	This information is coded as
		predicate(Symbol, Arity, export, Symbol).

	For backwards compatibility, :- public is treated as :- export.

	:- import p1/n1, ..., pk/nk.

	Declares that these predicates are to be imported.
	This means
	(a) that there MAY NOT be any clauses for any of these predicates.
	(b) that these predicates may not be exported.
	(c) that any goal referring to any of them will keep
	    its current name.

	This information is coded as
		predicate(Symbol, Arity, import, Symbol).

	The module may manipulate data base entries.  Level 1 says that
	these belong to the global name pool.  So they must either be
	imports or exports.  Now if you have compiled clauses for a
	predicate in Dec-10 Prolog, you can't get at them with 'retract'
	or 'clause', so we'd like something that warns the Dec-10 Prolog
	programmer that he has made a mistake (or rather has run into a
	Dec-10 Prolog problem) when he has explicit clauses for something
	he means to hit with assert and retract.  Import fits the bill.
	To make this less than totally obscure,  :- data is allowed as a
	synonym for :- import.  Ideally, the module preprocessor should be
	smart enough to check any explilcit asserts and retracts to ensure
	that they have been declared as data.  Note that there is nothing
	at all to stop someone saying ":- data 'mydb:hack'/3."

	System predicates are held in the table
		system(Goal)
	which is copied into predicate/4 when we start to translate
	a file.  All system predicates are automatically imported.

	Internal predicates are coded as
		predicate(Symbol, Arity, hidden, NewSymbol)
	where e.g. internal predicate foo/5 in module baz would be
		predicate(foo, 5, hidden, 'foo:baz').

	:- mode declarations are renamed, and written out.

	:- op(...) declarations are obeyed and written out.

	Clauses are renamed and written out.  They should really go
	through the pretty-printer, but portray_clause hasn't yet
	been brought back from C Prolog to Dec-10 Prolog, and I have
	no time to do it now.  The source code reformatter will do a
	reasonable job, at Edinburgh it's the TOP edit macro COMMA.EM,
	an EMACS version is in preparation in California.

	Note that the renaming assumes that any predicate not
	known to be exported, imported, or internal is internal.
	If an import or export declaration comes along later
	and says otherwise, that is a mistake.

	One peculiarity is that some fairly basic predicates in
	Dec-10 Prolog actually go through the interpreter, so
	:- public declarations may be needed.  These are not to
	be confused with :- export declarations; if foo/5 in module
	baz will be called by the interpreter, there will be a
	:- public 'foo:baz'/5. declaration.

	There is a table applies(pred(Args), Decl).  Decl is
	'direct', meaning that no :- public declaration is needed,
	or 'called', meaning that a :- public declaration is needed.
	For now, there is no way for the user to add to this table.
	The arguments to pred() are - meaning that the argument is
	not called, or N (>= 0) meaning that it is called with N
	extra arguments.

	The table defined(Symbol, Arity) means that a clause for
	Symbol/Arity (its name in the source file, not in the
	translation) has been seen.

    To run this program:
	compile(['util:listut.pl','util:writef.pl','util:write.pl',
		 'UTIL:MODULE.PL']).
    Then you can use translate/1.  Note that this is not a module
    itself, because it does not live with the programs it translates.
    No doubt this should change, but for now it will do.
*/


:- public
	translate/1.

/* import
	writef/2,
	fwritef/3,
	portable_writeq/1.
*/
:- mode
	applies(+, +, -),
	dec10_correction(+, +, +),
	enter_module(+),
	leave_module,
	leave_module(+, +, +),
	portray_clause(+),		% SORRY THIS ISN'T IN DEC-10 LIBRARY
	process(+),
	process_export(+),
	process_head(+),
	process_import(+),
	process_mode(+),
	process_op(+),
	rewrite_all(+, +, -, +),
	rewrite_all(+, +, +, +, +),
	rewrite_args(+, +, +, +, +),
	rewrite_one(+, -),
	rewrite_one(+, +, +),
	system(-),
	translate(+).

/* data
	defined/2,
	module_name/2,
	predicate/4.
*/
:- op(1199, fx, (module)).
:- op(1199, fx, (export)).
:- op(1199, fx, (import)).
:- op(1199, fx, (data)).


translate(Module) :-
	name(Module, Chars),
	append(Chars, ".MPL", InChars),
	append(Chars, ".PL", ExChars),
	name(InFile, InChars),
	see(InFile),
	name(ExFile, ExChars),
	tell(ExFile),
	enter_module(Module),
	repeat,
	    read(Term),
	    expand_term(Term, Clause),
	    (   Clause = end_of_file
	    ;   process(Clause), fail
	    ),
	!,
	seen,
	told,
	leave_module.


process(:-(public(Preds))) :- !,
	write(':- public'), nl,
	process_export(Preds),
	write(.), nl, nl.
process(:-(export(Preds))) :- !,
	write(':- public'), nl,
	process_export(Preds),
	write(.), nl, nl.
process(:-(import(Preds))) :- !,
	process_import(Preds).
process(:-(data(Preds))) :- !,
	process_import(Preds).
process(:-(module(Module))) :- !,
	leave_module,
	enter_module(Module).
process(:-(mode(Modes))) :- !,
	write(':- mode'), nl,
	process_mode(Modes),
	write(.), nl, nl.
process(:-(Ops)) :- !,
	process_op(Ops).
process(:-(Head,Body)) :- !,
	process_head(Head),
	rewrite_all(0, :-(Head,Body), Clause, direct),
	portray_clause(Clause).
process(Head) :-
	process_head(Head),
	rewrite_one(Head, Clause),
	portray_clause(Clause).


portray_clause(Clause) :-		% THIS SHOULD BE COPIED FROM
	numbervars(Clause, 0, _),	% C PROLOG, WHERE IT IS BASED
	portable_writeq(Clause),	% ON PP.PL.  I've used portable_
	write(.), nl.			% writeq to avoid the ',' problem.


process_head(Head) :-
	nonvar(Head),
	functor(Head, Symbol, Arity),
	atom(Symbol),
	(   predicate(Symbol, Arity, (import), _),
	    fwritef(user,
		'! Imported predicate %t has a clause.\n',
		[Symbol/Arity])
	;   defined(Symbol, Arity)
	;   assert(defined(Symbol, Arity)), nl
	), !.
process_head(Head) :-
	fwritef(user, '! Strange clause head: %t.\n', [Head]),
	fail.


process_export((Pred,Preds)) :-
	process_export(Pred),
	write(','), nl,
	process_export(Preds).
process_export(Symbol/Arity) :-
	put(9), writeq(Symbol/Arity),
	(   predicate(Symbol, Arity, hidden, _),
	    fwritef(user,
		'! :- export %t comes too late -- already assumed hidden.\n',
		[Symbol/Arity])
	;   predicate(Symbol, Arity, (import), _),
	    fwritef(user,
		'! :- export %t impossible -- it is system or imported.\n',
		[Symbol/Arity])
	;   predicate(Symbol, Arity, (export), _)
	;   assert(predicate(Symbol, Arity, (export), Symbol))
	),  !.


process_import((Pred,Preds)) :-
	process_import(Pred),
	process_import(Preds).
process_import(Symbol/Arity) :-
	(   predicate(Symbol, Arity, hidden, _),
	    fwritef(user,
		'! :- import %t comes too late -- already assumed hidden.\n',
		[Symbol/Arity])
	;   predicate(Symbol, Arity, (export), _),
	    fwritef(user,
		'! :- import %t impossible -- it is exported.\n',
		[Symbol/Arity])
	;   predicate(Symbol, Arity, (import), _)
	;   assert(predicate(Symbol, Arity, (import), Symbol))
	),  !.


process_mode((Mode,Modes)) :- !,
	process_mode(Mode),
	write(','), nl,
	process_mode(Modes).
process_mode(Head) :-
	rewrite_one(Head, Copy),
	put(9), writeq(Copy).


process_op((Op,Ops)) :-
	process_op(Op),
	process_op(Ops).
process_op(op(P,T,A)) :-
	write(':-  '), portable_writeq(op(P,T,A)), write(.), nl,
	op(P, T, A).

enter_module(ModuleName) :-
	abolish(predicate, 4),
	abolish(defined, 2),
	abolish(module_name, 2),
	(   system(Goal),
	    functor(Goal, Symbol, Arity),
	    assert(predicate(Symbol, Arity, (import), Symbol)),
	    assert(defined(Symbol, Arity)),
	    fail
	;   true
	),
	name(ModuleName, Chars),
	append(Chars, [0':|Tail], Appended),
	assert(module_name(Tail, Appended)).


%   leave_module is called when we have closed the output file and
%   are writing to the terminal again.  That's why it uses writef
%   instead of fwritef.  It's a failure-driven loop: check that each
%   predicate is defined.

leave_module :-
	(   predicate(Symbol, Arity, Key, _),
	    leave_module(Symbol, Arity, Key),
	    fail
	;   true
	).


leave_module(Symbol, Arity, _) :-
	defined(Symbol, Arity),
	!.
leave_module(Symbol, Arity, (export)) :- !,
	writef('! Predicate %t/%t is %t but not defined.\n',
		[Symbol, Arity, exported]).
leave_module(Symbol, Arity, hidden) :- !,
	writef('! Predicate %t/%t is %t but not defined.\n',
		[Symbol, Arity, used]).
	%  I don't think there are any other cases.


/*  rewrite_one(OldTerm, NewTerm)
    rewrites a single goal to use its new predicate symbol.
    For simplicity, it does this even when it doesn't need
    rewriting.  The arguments are just copied across.
*/

rewrite_one(OldTerm, NewTerm) :-
	nonvar(OldTerm),
	functor(OldTerm, OldSymbol, Arity),
	atom(OldSymbol),
	!,
	predicate(OldSymbol, Arity, NewSymbol),
	functor(NewTerm, NewSymbol, Arity),
	rewrite_one(Arity, OldTerm, NewTerm).
rewrite_one(Term, Term).


rewrite_one(0, _, _) :- !.
rewrite_one(N, OldTerm, NewTerm) :-
	arg(N, OldTerm, Arg),
	arg(N, NewTerm, Arg),
	M is N-1,
	rewrite_one(M, OldTerm, NewTerm).


predicate(Symbol, Arity, NewSymbol) :-
	predicate(Symbol, Arity, _, NewSymbol),
	!.
predicate(Symbol, Arity, NewSymbol) :-
	name(Symbol, Chars),
	module_name(Chars, NewChars),
	name(NewSymbol, NewChars),
	assert(predicate(Symbol, Arity, hidden, NewSymbol)).


/*  The basic idea in handling clauses is that we are rewriting a
    term that will be called with N extra arguments.  (At the top
    level, N is 0.)  We want to rewrite all levels of goals in
    this term, hence the name.
*/

rewrite_all(-, Term, Term, _) :- !.
rewrite_all(ExtraArgs, OldTerm, NewTerm, Called) :-
	nonvar(OldTerm),
	functor(OldTerm, OldSymbol, Arity),
	atom(OldSymbol),
	!,			% compound(OldTerm)
	FullArity is Arity+ExtraArgs,
	predicate(OldSymbol, FullArity, NewSymbol),
	dec10_correction(Called, NewSymbol, FullArity),
	functor(NewTerm, NewSymbol, Arity),
	functor(Template, NewSymbol, FullArity),
	rewrite_args(Template, OldTerm, NewTerm, Arity, Called).
rewrite_all(ExtraArgs, OldTerm, fail, _) :-
	integer(OldTerm),
	!,
	fwritef(user, '! %t/%t will be called as a goal!!\n',
		[OldTerm,ExtraArgs]).
rewrite_all(_, Term, Term, _).


rewrite_args(Template, OldTerm, NewTerm, Arity, OldCalled) :-
	applies(Template, OldCalled, Called),
	!,
	rewrite_all(Arity, OldTerm, NewTerm, Template, Called).
rewrite_args(_, OldTerm, NewTerm, Arity, _) :-
	rewrite_one(Arity, OldTerm, NewTerm).


%   dec10_correction makes sure that a predicate which is supposed 
%   to be internal to the module but is given to the interpreter has
%   a :- public declaration.  There is no need to do this for 'import'
%   and 'export' predicates.  We split 'hidden' into two groups; the
%   'hidden' predicates proper, and the 'called' predicates.  This
%   ensures that each public declaration gets generated just the once.

dec10_correction(direct, _, _) :- !.
dec10_correction(called, NewSymbol, Arity) :-
	retract(predicate(Symbol, Arity, hidden, NewSymbol)),
	!,
	write(':- public '), writeq(NewSymbol/Arity),
	write('.  % for "call"'), nl,
	assert(predicate(symbol, Arity, called, NewSymbol)).
dec10_correction(_, _, _).


rewrite_all(0, _, _, _, _) :- !.
rewrite_all(N, OldTerm, NewTerm, Template, Called) :-
	arg(N, OldTerm, OldArg),
	arg(N, Template, ExtraArgs),
	rewrite_all(ExtraArgs, OldArg, NewArg, Called),
	arg(N, NewTerm, NewArg),
	M is N-1,
	rewrite_all(M, OldTerm, NewTerm, Template, Called).


/*----------------------------------------------------------------------+
|									|
|	The following tables describe Dec-10 Prolog.			|
|	system/1 should be a built in predicate, as in C Prolog.	|
|									|
+----------------------------------------------------------------------*/

system(abolish(_,_)).
system(revive(_,_)).
system(asserta(_,_)).
system(asserta(_)).
system(assertz(_,_)).
system(assertz(_)).
system(retract(_)).
system(clause(_,_,_)).
system(clause(_,_)).
system(recorda(_,_,_)).
system(recordz(_,_,_)).
system(recorded(_,_,_)).
system(instance(_,_)).
system(erase(_)).
system(true).
system(length(_,_)).
system(name(_,_)).
system(op(_,_,_)).
system(var(_)).
system(atom(_)).
system(!).
system(statistics).
system(statistics(_,_)).
system(functor(_,_,_)).
system(call(_)).
system(expand_term(_,_)).
system(debug).
system(debugging).
system(display(_)).
system(get(_)).
system(get0(_)).
system(leash(_)).
system(nl).
system(nodebug).
system(print(_)).
system(put(_)).
system(skip(_)).
system(tab(_)).
system(trace).
system(ttyflush).
system(ttyget(_)).
system(ttyget0(_)).
system(ttynl).
system(ttyput(_)).
system(ttyskip(_)).
system(ttytab(_)).
system(write(_)).
system(writeq(_)).
system(ancestors(_)).
system(depth(_)).
system(maxdepth(_)).
system(subgoal_of(_)).
system(abort).
system(arg(_,_,_)).
system(assert(_)).
system(atomic(_)).
system(bagof(_,_,_)).
system(break).
system(close(_)).
system(compare(_,_,_)).
system(compile(_)).
system(consult(_)).
system(current_atom(_)).
system(current_functor(_,_)).
system(current_predicate(_,_)).
system(current_op(_,_,_)).
system(fail).
system(fileerrors).
system(gc).
system(gcguide(_)).
system(halt).
system(integer(_)).
system(keysort(_,_)).
system(listing).
system(listing(_)).
system(log).
system(nofileerrors).
system(nogc).
system(nolog).
system(nonvar(_)).
system(numbervars(_,_,_)).
system('C'(_,_,_)).		% generated in grammar rules
system(phrase(_,_)).
system(prompt(_,_)).
system(read(_)).
system(reconsult(_)).
system(rename(_,_)).
system(repeat).
system(restore(_)).
system(save(_)).
system(save(_,_)).
system(see(_)).
system(seeing(_)).
system(seen).
system(setof(_,_,_)).
system(sort(_,_)).
system(tell(_)).
system(telling(_)).
system(told).
system(trimcore).
system(plsys(_)).
system(end_of_file).
system('LC').
system('NOLC').
system(_^_).
system((_:-_)).
system((_,_)).
system((_;_)).
system(\+_).
system((_->_)).
system(spy _).
system(nospy _).
system(_=_).
system(_ is _).
system(_==_).
system(_\==_).
system(_=.._).
system(_<_).
system(_>_).
system(_=<_).
system(_>=_).
system(_@<_).
system(_@=<_).
system(_@>=_).
system(_@>_).
system(_=\=_).
system(_=:=_).

applies((0:-0),		X, X).
applies((0,0),		X, X).
applies((0;0),		X, X).
applies(call(0),	X, called).
applies(once(0),	X, called).
applies(phrase(2,-),	X, called).
applies(bagof(-,0,-),	X, called).
applies(setof(-,0,-),	X, called).
applies((0->0),		X, called).	% NB: (a->b;c) doesn't work!
applies(X^0,		X, direct).
applies(\+0,		X, called).
