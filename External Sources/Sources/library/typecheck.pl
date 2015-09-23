%   File   : TYPECH.PL
%   Author : Alan Mycroft & R.A.O'Keefe
%   Updated: 8 June 1984
%   Purpose: Prolog type-checker

:- public
	load/1,                 %  for users
	type_check/5.           %  for setof/3

%>>     This module uses unify/2 from Util:MetUtl.Pl .
%>>     It also uses append/3 from the utilities.


%   This program defines a "type-checked consult" operation
%       load(Files)
%   where Files is an atom or a list of atoms.  There is no
%   analogue of the reconsult operation.  In the Files type
%   declarations may be given in addition to the usual sort
%   of commands, questions, clauses, and declarations.  You
%   can put the type declarations in separate files, so that
%   load(['foo.typ','foo.pl']) can be used to type-check and
%   load the interpreted version, and compile('foo.pl') can
%   be used to compile the same code.  Note that declarations
%   have to be processed before clauses using the things
%   declared.

%   There are two new declarations:
%       type <type term> --> <constr>{| <constr>}.. .
%   e.g. type tree(T) --> empty | tree(T,tree(T),tree(T)).
%   and
%       pred <pred decl>{, <pred decl>}.. .
%   e.g. pred append(list(T), list(T), list(T)).
%   You may use a semicolon instead of a vertical bar if you like.
%   As a convenience for defining grammar rules,
%       rule p(T1,...,Tk).
%   has the same effect as
%       pred p(T1,...,Tk,list(T_),list(T_)).
%   where T_ is not further specified.  'C'/3 is predefined as
%       pred 'C'(list(X), X, list(X)).


:- op(1199, fx, [(type), (pred), (rule)]).
:- op(1198, xfy, (-->)).
:- op( 999, xfy, (:)).

%   load/1 is defined in the usual way.  The juggling with (no)fileerrors
%   is to determine whether failure to find a file causes an error message
%   and abort, or just (as here) a failure.  expandterm/2 is where grammar
%   rules are translated to ordinary Prolog.  In Dec-10 Prolog, the style
%   of programming which uses failure-driven loops is nearly obsolete,
%   thanks to the introduction of TRO to the compiler, and it was never
%   considered to be anything other than a hack.  However, this stuff has
%   to be useful in C Prolog and other Prologs which still lack TRO, so
%   the hack remains.

load(Files) :-
	recorded(void, (type), _),
	!,              % the definitions have been loaded
	load1(Files).
load(Files) :-
	recordz(void, (type), _),
	recordz(any,  (type), _),
	recordz((_:-_), type((void:-void), void), _),   % for assert
	load1(['util:prolog.typ'|Files]).


load1(Var) :-
	var(Var),
	!,
	write('! load: argument contains a variable'), nl,
	fail.
load1([Head|Tail]) :- !,
	load1(Head), !,  % discard this cut on non-TRO systems
	load1(Tail).
load1(File) :-
	atom(File),
	!,
	nofileerrors,
	seeing(Old),
	load2(File),
	fileerrors,
	see(Old).
load1(File) :-
	write('! load: argument not list or atom: '),
	write(File), nl,
	fail.

load2(File) :-
	see(File),
	repeat,
		read(Term),
		expand_term(Term, Expanded),
		process_term(Expanded, File),
		Expanded = end_of_file,
	!,
	seen,
	write(File), write(' loaded.'), nl.
load2(File) :-
	write('! load: can''t see '),
	write(File), nl,
	fail.



%   process_term(Expansion, File)
%   handles a command, question, clause, or declaration.
%   Questions are treated as if they were commands, which is just plain
%   wrong, but only in Prolog-X is a version of 'read' standardly
%   available which gives you a name->variable list so that you can
%   print the answers.  Type checking wants to be built into a Prolog
%   system top level from the word go, not added on as an afterthought.

process_term(end_of_file, File) :- !.
process_term((:- Command), File) :- !,
	process_command(Command, File).
process_term((?- Question), File) :- !,
	process_command(Question, File).
process_term({Unchecked}, File) :- !,
	assertz(Unchecked).
process_term((Head :- Body), File) :- !,
	type_check((Head:-Body), Clause),
	assertz(Clause).
process_term(Head, File) :-
	type_check((Head:-true), Clause),
	assertz(Clause).


%   process_command(Command, File)
%   mainly handles declarations.

process_command((type Type --> Constructors), File) :- !,
	process_type(Type, Constructors).
process_command((pred Predicates), File) :- !,
	process_pred(Predicates, (pred)).
process_command((rule GrammarRules), File) :- !,
	process_pred(GrammarRules, (rule)).
process_command([Files], File) :- !,
	load1(Files).
process_command((mode Modes), File) :- !,
	true.           %  could maybe note that we expect a type?
process_command((public PublicDeclarations), File) :- !,
	true.           %  could maybe note that we expect a type?
process_command(Question, user) :- !,
	(   type_check(Question, Checked)
	;   write('! ill-typed '), nl, fail
	),
	(   call(Checked),
		write(Checked), nl, write('more? '), ttyflush,
		read(no)
	;   write('no (more) answers'), nl
	).
process_command(Command, File) :-
	(   type_check(Command, Checked)
	;   write('! ill-typed '), write(Command),
		write(' in '), write(File), nl, fail
	),
	(   call(Checked), !
	;   write('! failed command '), write(Command),
		write(' in '), write(File), nl, fail
	).



%   The "typed premise" described in Mycroft & O'Keefe is stored two
%   ways.  The types of variables are held in a dynamic data structure
%   used only within a single clause.  The types of predicates and
%   functors are held in the data base.  We use the Dec-10 "recorded"
%   data base here to reduce clashes with user clauses and make access
%   a little bit faster, but ordinary clauses could just as easily be
%   used.  There are three cases:
%
%       recorded(Key, (type), _)
%                               - use type_def(Key) as clauses
%       means that the Key is a type constructor.   E.g. after
%       processing the declaration :- type tree(T) --> ... .
%       recorded(tree(T), (type), _)
%       would be true.
%
%       recorded(Key, type(Pat,void), _)
%                               - use has_type(Key, Pat, void) as clauses
%       means that the Key is defined as a predicate.  The Pat looks like
%       the Key, but has type terms for its arguments.  E.g. after
%       processing the declaration :- pred elem(T, tree(T)).
%       recorded(elem(_,_), type(elem(T,tree(T)),void), _)
%       would be true.
%
%       recorded(Key, type(Pat,Val), _)
%                               - use has_type(Key, Pat, Val) as clauses
%       means that the Key is defined as a function.  The Pat looks like
%       the Key, but has type terms for its arguments.  E.g. after
%       processing the declaration
%       :- type tree(T) --> empty | t(T,tree(T),tree(T)).
%       recorded(empty, type(empty,tree(T)), _) and
%       recorded(t(_,_,_), type(t(T,tree(T),tree(T)),tree(T)), _)
%       would both be true.
%
%       These declarations are the only way that data of the given forms
%       are recorded.


%   process_type(<type head>, <type body>)
%   checks that its arguments are well formed, and if they are,
%   records the type information about the <type head> and the
%   constructors.

process_type(Head, Body) :-
	check_type_head(Head),
	recordz(Head, (type), _),       % recorded here for recursive types
	check_type_body(Head, Body, Constructors),
	process_type_body(Constructors, Head).

process_type_body([Constructor|Constructors], Head) :-
	recordz(Constructor, type(Constructor,Head), _), !,
	process_type_body(Constructors, Head).
process_type_body([], _).


check_type_head(Head) :-
	var(Head),
	!,
	write('! Type head is a variable.'), nl,
	fail.
check_type_head(Head) :-
	recorded(Head, (type), _),
	!,
	write('! Already a type: '), write(Head), nl,
	fail.
check_type_head(Head) :-
	functor(Head, _, N),
	check_type_head(N, Head),
	!,
	fail.
check_type_head(_).

check_type_head(N, Head) :-
	arg(N, Head, Arg),
	nonvar(Arg),
	write('! Type head argument '), write(N),
	write(' is bad.'), nl.
check_type_head(N, Head) :-
	arg(N, Head, N),
	M is N-1,
	check_type_head(M, Head).


%   is_type_expr(Term)
%   succeeds when Term is a type expression, that is, when all the
%   constructors it is made from are type constructors.  If we had
%   type macros (:- type Lhs = Rhs) this is where we would expand
%   them.  That will come later.

is_type_expr(Type) :-
	var(Type),
	!.
is_type_expr(Type) :-
	recorded(Type, (type), _),
	!,
	functor(Type, _, N),
	is_type_expr(N, Type).
is_type_expr(Type) :-
	write('! not a type: '),
	write(Type), nl,
	fail.

is_type_expr(0, Type) :- !.
is_type_expr(N, Type) :-
	arg(N, Type, Arg),
	is_type_expr(Arg),
	M is N-1,
	!,
	is_type_expr(M, Type).

check_type_body(Head, Body, _) :-
	numbervars(Head, 0, M),
	numbervars(Body, M, N),
	N > M,
	!,
	write('! Rhs of type def contains variables not in lhs.'), nl,
	fail.
check_type_body(Head, Body, Constructors) :-
	check_type_body_(Body, Constructors, []).

check_type_body_(( Constr1 ; Constr2 ), CL, CR) :- !,
	check_type_body_(Constr1, CL, CM),
	check_type_body_(Constr2, CM, CR).
check_type_body_({Constructor}, [Constructor|CR], CR) :-
	nonvar(Constructor),
	functor(Constructor, _, N),
	is_type_expr(N, Constructor),
	!.
check_type_body_({Constructor}, _, _) :- !,
	write('! Dud constructor : '),
	write(Constructor), nl,
	fail.
check_type_body_(Constructor, CL, CR) :-
	check_type_body_({Constructor}, CL, CR).



%   process_pred(Preds, Sort)
%   processes pred (Sort=pred) and rule (Sort=rule) declarations.
%   Several predicates may be declared in one declaration, and they
%   may be separated by any mixture of commas and semicolons.  To
%   declare comma or semicolon as a predicate, you have to put it
%   inside braces { }, e.g. :- pred {void,void}.  The same escape
%   convention is used to let you define these symbols as constructors
%   in type declarations.

process_pred(Var, Sort) :-
	var(Var),
	!,
	write('! variable in '), write(Sort),
	write(' declaration.'), nl,
	fail.
process_pred((Preds1 , Preds2), Sort) :- !,
	process_pred(Preds1, Sort),
	process_pred(Preds2, Sort).
process_pred((Preds1 ; Preds2), Sort) :- !,
	process_pred(Preds1, Sort),
	process_pred(Preds2, Sort).
process_pred({Pred}, (pred)) :-
	recorded(Pred, type(_,void), _),
	!,
	write('! already declared: '), write(Pred), nl,
	fail.
process_pred({Pred}, (pred)) :- !,
	functor(Pred, _, N),
	is_type_expr(N, Pred),
	recordz(Pred, type(Pred,void), _).
process_pred({Rule}, (rule)) :- !,
	Rule =.. List,
	append(List, [list(T),list(T)], Full),
	Pred =.. Full,
	process_pred({Pred}, (pred)).
process_pred(Other, Sort) :-
	process_pred({Other}, Sort).



%   To perform type checking we use the Milner algorithm with overloading.
%   The idea is that we consider (via backtracking) all the type resolutions
%   possible, and accept the typing if exactly one type assignment exists.
%   For this (second order) operation we use 'setof'.  The typed premise
%   is in two parts.  The predicate and functor assignments are held in the
%   data base, the variable assignments (which only have significance within
%   the current clause) in held in Tenv (type environment) variables.
%   type tenv --> var | var(variable,type,tenv).


type_check(Given, Pruned) :-
	setof(P, To^type_check(Given, P, var, To, void), PP),
	(   PP = [Pruned], !
	;   write('! ambiguous overloaded functor.'), nl, !, fail
	).
type_check(:-(Head,Body), _) :-
	write('% no type can be assigned to the rule'), nl, !,
	pp_explicit(:-(Head,Body), Head).
type_check(Head, _) :-
	write('% no type can be assigned to the fact'), nl, !,
	pp_explicit(Head, Head).


%   type_check(Given, Pruned, TenvIn, TenvOut, Expected)
%   checks a Given term which is expected to have type Expected.
%   It may extend the type environment, as well as further specifying
%   existing variable assignments, and it returns the Given term
%   Pruned of ":Type" annotations.  The point of the latter is to
%   help the type system when it comes across an overloaded term that
%   it is otherwise unable to resolve.

type_check(X, X, Ti, Ti, Expected) :-
	Expected == any,
	!.              %  X had better not contain :annotations!
type_check(Var, Var, Ti, To, Context) :-
	var(Var),
	!,
	type_check(Var, Ti, To, Context).
type_check((Given:Type), Pruned, Ti, To, Expected) :- !,
	unify(Type, Expected),
	type_check(Given, Pruned, Ti, To, Expected).
type_check((Head:-Body), (Lhs:-Rhs), Ti, To, void) :- !,
	recorded(Head, type(HeadT,void), _),
	numbervars(HeadT, 0, _),        %  crucial
	functor(Head, F, N),
	functor(Lhs,  F, N),
	type_check(N, Head, Lhs, Ti, Tm, HeadT),
	type_check(Body, Rhs, Tm, To, void).
type_check(Int, Int, Ti, Ti, integer) :-
	integer(Int),
	!.              %  special hack for numbers
type_check(Given, Pruned, Ti, To, Expected) :-
	recorded(Given, type(GivenT, ResultT), _),
	unify(ResultT, Expected),
	functor(Given, F, N),
	functor(Pruned, F, N),
	type_check(N, Given, Pruned, Ti, To, GivenT).

%   Type check a variable

type_check(Var, Ti, Ti, Expected) :-
	varassoc(Ti, Var, VarT),
	!,
	unify(VarT, Expected).
type_check(Var, Ti, var(Var,Expected,Ti), Expected).

varassoc(var(V,T,_), Var, T) :-
	V == Var, !.
varassoc(var(_,_,R), Var, T) :-
	varassoc(R, Var, T).

%   Type check the arguments of a term.  The Given, Pruned, and Expected
%   arguments all have the same principal functor, and N is the number
%   of arguments still to be checked.

type_check(0, _, _, Ti, Ti, _) :- !.
type_check(N, Given, Pruned, Ti, To, Expected) :-
	arg(N, Given, Agiven),
	arg(N, Expected, Aexpected),
	type_check(Agiven, Apruned, Ti, Tm, Aexpected),
	arg(N, Pruned, Apruned),
	M is N-1,
	% There *MUSTN'T* be a cut here, as we want type_check/5
	% to be able to back-track and try another assignment.
	type_check(M, Given, Pruned, Tm, To, Expected).
