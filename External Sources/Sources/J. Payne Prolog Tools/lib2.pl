%   File   : LIB2.PL
%   Author : R.A.O'Keefe
%   Updated: 18 February 1984
%   Purpose: A Bottoms-10 version of the VAX "lib" predicate.
%   Needs  : append/3 and memberchk/2

/*  Note: LIB.PL and LIB2.PL are two parts of one module.  They are
    separate because the user must be able to change the 'libdirectory'/1
    table with assert and retract, and therefore that file must not be
    compiled, while this one may be.  If you have an interpreter, or if
    'assert' *is* your compiler, you may merge the two files, as they are
    merged on the VAX.
*/

:- public
	lib/1,			%  load a file
	lib/2,			%  find a file
	note_lib/1,		%  note public predicates of file
	note_lib/2.		%  general case of note_lib/1

:- mode
	lib(+),
	lib(+, -),
	lib_(+),
	lib_(+, -),
	lib_(+, +, -),
	note_lib(+),
	note_lib(+, +),
	note_lib(+, +, +).



%   lib(File)
%   looks in all the likely places for File, and when it has found it,
%   it compiles the file.  In an interpreted Prolog, it should reconsult
%   the file instead.
%   lib(Pred/Arity) uses the clauses left behind by note_lib to compile
%   (or reconsult) the file defining that predicate.

lib(Symbol/Arity) :- !,
	functor(Head, Symbol, Arity),
	(   clause(Head, (Load,Head)), !, call(Load)
	;   true	% it is already defined
	).
lib(File) :-
	lib(File, Found),
	compile(Found).			%  Dec-10 Prolog
%	reconsult(Found).		%  Vax-11 Prolog
	

%   lib(File, Found)
%   looks in all the likely places for File, and when it has
%   found it, it returns the name of the file it Found.

lib(File, Found) :-
	nofileerrors,		%   so 'see' will fail instead of aborting
	seeing(OldFile),	%   'see' redirects the input stream
	(   lib_(File, Guess),	%   GENERATE guesses
	    see(Guess),		%   this succeeds if the guess is right
	    !,			%   and such a file exists.  We want 1 only.
	    seeing(Found),	%   pick up the name after possible normalisation
	    seen,		%   close the file (POSSIBLE BUG!)
	    fileerrors,		%   make see abort instead of failing, again.
	    see(OldFile)	%   make the input stream what it used to be
	;   fileerrors,		%   if the file can't be found, we have to
	    fail		%   fileerrors, but at least input wasn't
	).			%   redirected.


%   lib_(File, Guess)
%   uses the 'libdirectory'/1 table to enumerate likely places.
%   The first thing to try, however, is the file name as given.
%   The entries in 'libdirectory' are of two sorts:
%	atoms containing a question mark <prefix>?<suffix>
%	    The guess is <prefix><File><suffix>.
%	    Any special punctuation must already be in the atom.
%	atoms not containing a question mark (device names)
%	    Two guesses are made: <device>:<File> and <device>:<File>.pl
%   A guess is immediately rejected if it contains two colons, two
%   full stops, or a full stop followed by a colon.

lib_(File, File).
lib_(File, Guess) :-
	name(File, FileName),
	libdirectory(Dir),			% enumerate directories
	name(Dir, DirName),
	lib_(FileName, DirName, FullName),	% construct full name
	lib_(FullName),				% check full name
	name(Guess, FullName).

lib_(FileName, DirName, FullName) :-		%  P?S -> PFS
	append(Prefix, [63|Suffix], DirName),
	!,
	append(FileName, Suffix, L),
	append(Prefix, L, FullName).
lib_(FileName, DirName, FullName) :-		%  D -> D:F
	append(DirName, [58|FileName], FullName).
lib_(FileName, DirName, FullName) :-		%  D -> D:F.pl
	append(FileName, ".pl", Extended),
	append(DirName, [58|Extended], FullName).


%   Check that the string does not contain two colons, two
%   full stops, or a full stop followed by a colon.  Let's face it,
%   THIS PREDICATE IS OPERATING-SYSTEM-SPECIFIC.
%   There ought to be a valid_file_name(Atom) predicate in each
%   Prolog implementation, but there isn't.

lib_(S) :-
	\+  (	append(_, [58|T], S), memberchk(58, T)
	    ;   append(_, [46|T], S),(memberchk(46, T) ; memberchk(58, T))
	    ).



%   note_lib(File, Method)
%   reads the first clause in the File, which should be a public declaration.
%   If it is, it notes for each predicate mentioned in that declaration that
%   the predicate may be defined by calling Method(File).  Method should be
%   'reconsult' or 'compile', 'consult' will NOT do as it has to wipe out the
%   clauses which note_lib creates.  If you have some fancy preprocessor, you
%   can name it as the method to be used, provided it acts like reconsult
%   rather than consult.
%   note_lib(File)
%   supplies a default Method to note_lib/2, the default being compile for
%   those Prologs which have it, and reconsult for the others.

note_lib(File) :-
	note_lib(File, compile).	%  Dec-10 Prolog
%	note_lib(File, reconsult).	%  Vax-11 Prolog

note_lib(File, Method) :-
	nofileerrors,
	see(File),
	read(FirstClause),
	seen,
	fileerrors,
	FirstClause = (:- public Exports),
	!,
	note_lib(Exports, File, Method).
note_lib(File, Method) :-
	fileerrors,
	write('! note_lib: '), write(File),
	write(' is missing or lacks a :- public declaration.'),
	nl.

note_lib((A,B), File, Method) :- !,
	note_lib(A, File, Method),
	note_lib(B, File, Method).
note_lib(Symbol/Arity, File, Method) :-
	functor(Head, Symbol, Arity),
	Load =.. [Method,File],
	(   clause(Head, _)		%  it's already defined
	;   assert((Head :- Load, Head))
	),  !.


