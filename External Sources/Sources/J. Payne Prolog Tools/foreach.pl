%   File   : FEACH.PL
%   Author : R.A.O'Keefe
%   Updated: 13 June 1984
%   Purpose: Redefine the foreach/5 predicate I deleted.

/*  foreach(Generator, Extractor, Combiner, Argument, Total)
    is a variation on findall, a sort of generalised summation.
    Generator enumerates interesting situations (by backtracking).
    For each situation found by the Generator, the Extractor is
    called once.  If the Extractor fails for any situation, the
    entire call to foreach fails, and any junk that may have been
    put into the data base is removed.  The Extractor is expected
    to bind the Argument, which plays the role of the Template in
    findall.  The code doesn't check that the Argument is bound
    to a non-variable, perhaps it should.  When the Generator runs
    out of situations to enumerate, the Combiner is used to form a
    tree of the arguments.  The Combiner may be a single atom, in
    which case there had better be at least one situtation, or it
    may be an [Op,Default] pair, in which case the default is used
    if there are no situations.  If the arguments found are A1,...,An
    the tree which is returned as the Total is op(A1,op(A2,...,op(_,An)))
    e.g. a+(b+(c+(d+e))).

    This predicate is OBSOLETE.  It is ONLY for an obscure part of MECHO.
    It cannot be assigned a type, as the type of the result depends on 
    the value of the 3rd argument in a strange way.  It would in general
    be much cleaner to use findall and then map down the list.  DANGER!
    BEWARE!  UNCLEAN! UNCLEAN!  Reading this may damage your mental health!
*/

:- public
	foreach/5.

:- mode
	foreach(+, +, +, -, ?),
	    foreach(+, -),
	    foreach(+, +, ?).


foreach(Generator, Extractor, Com, Argument, Tot) :-
	recorda(., -, _),	
	call(Generator),
	foreach(Extractor, Argument),
	write('! extractor failed in '),
	print(foreach(Generator,Extractor,Com,Argument,Tot)),
	!, fail.
foreach(_, _, [Op,Default], _, Total) :-
	atom(Op), nonvar(Default),
	recorded(., Term, Ref), erase(Ref),
	!,
	(   Term = -, Total = Default
	;   Term = -X, foreach(X, Op, Total)
	).
foreach(_, _, Op, _, Total) :-
	atom(Op),
	recorded(., Term, Ref), erase(Ref),
	!,
	Term = -X,
	foreach(X, Op, Total).
foreach(Gen, Ext, Combiner, Arg, Tot) :-
	write('! bad combiner in '),
	print(foreach(Gen,Ext,Combiner,Arg,Tot)), nl,
	recorded(., Term, Ref), erase(Ref),
	Term = -,
	!, fail.



%   foreach(Accumulator, Operator, Total)
%   picks up further situation arguments out of the data base and
%   sticks them onto the right of the accumulator.

foreach(SoFar, Op, Total) :-
	recorded(., Term, Ref), erase(Ref),
	!,
	(   Term = -, Total = SoFar
	;   Term = -X, Next =.. [Op,X,SoFar], foreach(Next, Op, Total)
	).



%   foreach(Extractor, Argument)
%   calls the Extractor and records the first solution Argument.
%   Note the cut: we'd really like to make it an error for there
%   to be more than one extraction for a given situation, but all
%   we can do is force there to be at most one.  If there aren't
%   any, we clean out the stack and *succeed*, this will cause the
%   first clause of foreach/5 to be resumed which will print an
%   error message and fail.

foreach(Extractor, Argument) :-
	call(Extractor),
	!,
	recorda(., -Argument, _),
	fail.
foreach(_, _) :-
	recorded(., Term, Ref), erase(Ref),
	Term = - .

