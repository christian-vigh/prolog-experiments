%   File   : TOPLEV.PL
%   Author : Richard A. O'Keefe
%   Updated: 31 August 1984
%   Purpose: Prolog "top level"

/*  Although the actual code presented here is entirely my own, I have
    read the top level code for Dec-10 Prolog, PDP-11 Prolog, C Prolog,
    Prolog-X, and NIP, so the ideas are those of David Warren, Fernando
    Pereira, and Lawrence Byrd.  This top level is not exactly the same
    as any of the ones I've cited; in particular, you can enter clauses
    from command level just by typing them.  The part of the code which
    is most original is the way I handle nested reconsults; I believe
    it to do pretty much what Dec-10 Prolog does, but it is far from
    clear what this "should" mean.  Frankly, I think we need something
    other than consult and reconsult.  This top level is just in the
    interests of Prolog *users*; anyone wanting to claim "Dec-10 Prolog
    compatibility" may freely use this file as a basis for their toplevel.

    [31 August 1984; RAOK] Added toplevel_hook and break_hook interfaces.
    The user-defined predicate break_hook will be called:
	break_hook(enter, break)	-- at the beginning of a break
	break_hook(leave, break)	-- at the end of a break
	break_hook(enter, consult(File))-- just after the file is opened
	break_hook(leave, consult(File))-- just before the file is closed
	break_hook(enter, reconsult(File))
	break_hook(leave, reconsult(File))
    A distinction is made between the predicate not existing, and failing.
    If it exists but fails, the file will be closed, a warning will be
    printed, and the file will not be read.  break_hook can be used to reset
    counters, print additional statistics, to change operator declarations
    according to file extensions, and so on.

    The user-defined predicate toplevel_hook(Goal, Vars) will be called just
    before the goal is executed.  The Vars is the name=variable list which
    read/2 returns.  If this is defined and fails the goal is not executed,
    and '! rejected by toplevel_hook' displayed.

    User intervention in output is available via the portray/1 predicate
    (maybe this should change to print_hook?) and in input via C-Prolog's
    term_expansion/2 predicate (this should be expand_term_hook).

    Dec-10 Prolog does not have break_hook or toplevel_hook.  They are for
    experimentation and do not form part of the proposed standard (yet).

    [2 September 1984] Added portable_uses/1.  The idea is that
	:- uses([Files...]).
    should load precisely those files in the list which are not already
    loaded.  There is a table current_file(F) which says what files are
    loaded, and all these commands update it.

    A general note: I want lib(F) to be acceptable as a file name.  To
    hack that this really needs to be integrated with util:{lib,lib2}.pl.
    But to do a proper job of it, so that "type lib(F)" and so on work,
    lib(_) has to be there in the basic system, and it isn't.  Bear in
    mind that I would like people to write :- uses lib(lists) rather than
    the OS-dependent :- uses '/usr/lib/prolog/lists'.
*/

:- public
	portable_break/0,
	portable_consult/1,
	portable_reconsult/1,
	portable_uses/1.

:- mode
	bind_command_variables(+),
	call_break_hook(+, +),
	call_toplevel_hook(+, +),
	check_command_head(+, +, +),
	do_command(+, +, +),
	portable_break,
	portable_consult(+),
	portable_reconsult(+),
	portable_uses(+),
	process_file(+),
	process_file(+, +),
	process_file(+, +, +),
	process_files(+, +),
	user_is_happy_with(+).



%   'break' starts up a new top level.  It is not possible to express
%   this exactly in standard Prolog, as it is supposed to set up a new
%   level with debugging off.  Furthermore, all errors which do not
%   destroy the Prolog system itself are supposed to return to the
%   process_file/3 loop.  The same is true of 'consult' and 'reconsult';
%   none of these predicates is to be tracable itself, and the computations
%   set up by them are to be in a non-debugging state initially.  In most
%   implementations it should be straightforward to make the error handler
%   unwind the stack to the topmost call of do_command (which will not be
%   removed by TRO except when it is a genuine recursion, which doesn't hurt)
%   and make it look as if that call had simply failed.  It should also be
%   straightforward to save and restore the debugging state at the beginning
%   and end of process_file/2.

portable_break :-
	seeing(OldSee),
	telling(OldTell),
	see(user),
	tell(user),
	process_file(-1, '| ?- ', break),
	see(OldSee),
	tell(OldTell).



%   load each of the Files, adding their contents to the existing
%   definitions, if any.

portable_consult(Files) :-
	seeing(OldSee),
	telling(OldTell),
	tell(user),		% for error messages
	process_files(Files, +),
	see(OldSee),
	tell(OldTell).



%   load each of the files, replacing the current definitions, if any.
%   It is not 100% clear what nested reconsults should mean.  The
%   intent of the code in check_command_head is that a definition at
%   nesting level N wipes out a definition at any lower level, and that
%   after level N is finished its definitions are demoted to level N-1
%   so that reconsult(a), reconsult(a) works.  While I've expressed
%   this in Prolog using the recorded data base, all that is needed is
%   one byte in each predicate record to say what level it was defined
%   at and a list of the predicates defined in the current level.  If
%   these operations make it into the "standard", it will be part of
%   their definition that they do NOT alter either the recorded data
%   base or the clause data base except as commanded.

portable_reconsult(Files) :-
	seeing(OldSee),
	telling(OldTell),
	tell(user),		% for error messages
	process_files(Files, -),
	see(OldSee),
	tell(OldTell).



%   load the files which haven't already been loaded.  When they are loaded,
%   use reconsult.  NB: the current_file table is an ordinary predicate which
%   the user can change.  The user should be able to list but not change it.

portable_uses(Files) :-
	seeing(OldSee),
	telling(OldTell),
	tell(user),
	process_files(Files, =),
	see(OldSee),
	tell(OldTell).



%   Note the juggling with fileerrors.  What we actually want to do is
%   to check whether the File name can be opened for input; there is a
%   C Prolog predicate (exists) and a Dec-10 library predicate (file_exists)
%   to do this, but this should work in either and illustrates the use of
%   [no]fileerrors.  It has the unfortunate side effect that if you were
%   using nofileerrors mode it will be reset to fileerrors, but then you're
%   not supposed to set nofileerrors mode except transiently like this.

process_files(Files, _) :-
	var(Files),
	!,
	write('! var argument to (re)consult'), nl.
process_files([], _) :- !.
process_files([-File|Files], Flag) :- !,
	process_files(File, -),
	process_files(Files, Flag).
process_files(File, Flag) :-
	atom(File),
	nofileerrors,
	see(File),
	fileerrors,
	!,
	process_file(Flag, File),
	seen.
process_files(File, _) :-
	write('! bad argument '), write(File),
	write(' to (re)consult'), nl.


process_file(+, File) :- !,
	process_file(0, '| ', consult(File)).
process_file(=, File) :-
	current_file(File), !.
process_file(_, File) :-	% - and unloaded =
	(   recorded(process_files, depth(M), _),
	    integer(M)
	;   M = 0
	),  !,
	N is M+1,
	recorda(process_files, depth(N), DepthRef),
	process_file(N, '| ', reconsult(File)),
	(   recorded(process_files, defn(P,A,N), R),
	    erase(R),
	    M > 0,		% discard completely on return to top
	    recorda(process_files, defn(P,A,M), _)
	;   erase(DepthRef)
	).



%   Note that this loop uses read/2, not read/1.  read/2 is built into
%   C Prolog, and is a library predicate (RDTOK.PL+READ.PL) in Dec-10 Prolog.
%   expand_term is responsible for converting grammar rules to clauses.

process_file(Level, Prompt, WhatAmIDoing) :-
	(   call_break_hook(enter, WhatAmIDoing)
	;   process_file(WhatAmIDoing),
	    repeat,
		prompt(OldPrompt, Prompt),
		read(RawCommand, Variables),
		prompt(_, OldPrompt),
		expand_term(RawCommand, Command),
		do_command(Command, Variables, Level),
		Command = end_of_file,
	    (   call_break_hook(leave, WhatAmIDoing)
	    ;   seen
	    )
	),
	!.


%   process_file(Action) notes that the file has been loaded.  Note that
%   this has to be done after call_break_hook has been called, in case that
%   decides not to load the file after all, but before we load anything, in
%   case it loads some other files which "use" this one.  That is to say,
%   File fred:				File jim:
%	:- portable_uses(jim).		    :- portable_uses(fred).
%	rest of fred.			    rest of jim.
%   is perfectly acceptable, and unlike :- reconsult, perfectly safe.
%   If you consult, reconsult, or "uses" either of these files, you will get
%   the other as well.  The code is a little contorted because the Dec-10
%   Prolog compiler doesn't understand if-then-else or negation.

process_file(Action) :-
	(   arg(1, Action, FileName),
	    atom(FileName),
	    (   current_file(FileName)
	    ;   assertz(current_file(FileName))
	    )
	;   true
	), !.


%   call_break_hook succeeds iff break_hook fails, and prints a warning
%   message, and closes the file.
call_break_hook(Port, Action) :-
	current_predicate(break_hook, break_hook(Port,Action)),
	\+ call(break_hook(Port, Action)),
	write('! '), write(break_hook(Port, Action)),
	write(' failed.'), nl, seen, !.


do_command(Command, Variables, _) :-
	var(Command),
	!,
	bind_command_variables(Variables),
	write('! variable '), write(Command),
	write(' at top level'), nl.
do_command(end_of_file, _, _).
do_command((:- Command), _, _) :-
	functor(Command, ., 2), !,
	process_files(Command, +).
do_command((:- Command), Variables, _) :-
	(   call_toplevel_hook(Command, Variables)
	;   call(Command)
	;   bind_command_variables(Variables),
	    write('! Failed command: '),
	    write(Command), nl
	),
	!.
do_command((?- Question), [], -1) :-
	(   call_toplevel_hook(Question, [])
	;   call(Question), nl, write(yes)
	;   nl, write(no)
	),  !,
	nl.
do_command((?- Question), Variables, -1) :- !,
	sort(Variables, Solution),	% can use keysort in C Prolog
	(   call_toplevel_hook(Question, Solution)
	;   call(Question),
	    user_is_happy_with(Solution),
	    nl, write(yes)
	;   nl, write(no)
	),  !,
	nl.
do_command((?- Question), Variables, Level) :- !,
	do_command((:- Question), Variables, Level).
do_command((Head:-Body), Variables, Level) :- !,
	check_command_head(Head, Variables, Level),
	assertz((Head :- Body)).
do_command(Question, Variables, -1) :- !,
	do_command((?- Question), Variables, -1).
do_command(Head, Variables, Level) :-
	check_command_head(Head, Variables, Level),
	assertz(Head).


%   call_toplevel_hook *succeeds* iff toplevel_hook is defined and *fails*.
%   It prints a warning message.

call_toplevel_hook(Goal, Variables) :-
	current_predicate(toplevel_hook, toplevel_hook(Goal,Variables)),
	\+ toplevel_hook(Goal, Variables),
	!,
	bind_command_variables(Variables),
	write('! toplevel_hook rejected '), write(Goal), nl.


bind_command_variables([]).
bind_command_variables([X=X|Variables]) :-
	bind_command_variables(Variables).


%   All the hacking around in the second clause could be expressed quite
%   simply in another language: if the predicate was previously undefined,
%   set its definition level to Level and add it to the set of predicates
%   defined in this nest, or if it was previously defined but not at this
%   Level abolish the old definition, set its definition level to Level,
%   and add it to the set of predicates defined in this nest.

check_command_head(Head, Variables, _) :-
	(   var(Head), bind_command_variables(Variables)
	;   atomic(Head), \+atom(Head)
	),  !,
	write('! bad clause head: '), write(Head), nl.
check_command_head(Head, _, Level) :-
	Level > 0,
	functor(Head, P, A),
	(   recorded(process_files, defn(P,A,Level), _)
	;   recorded(process_files, defn(P,A,_), Ref), erase(Ref), fail
	;   recorda( process_files, defn(P,A,Level), _),
	    abolish(P, A)
	),  !.
check_command_head(_, _, _).


user_is_happy_with([Var=Val]) :- !,
	write(Var), write(' = '), print(Val),
	write(' ? '), ttyflush,
	ttyget0(C),
	(   C = 31				% end of line
	;   C = 26, seen			% end of file
	;   C = 0'y, ttyskip(31)		% y(es)
	;   C = 0't, trace, ttyskip(31), fail	% t(race)
	;   ttyskip(31), fail			% others
	).
user_is_happy_with([Var=Val|Variables]) :-
	write(Var), write(' = '), print(Val), put(0',), nl,
	user_is_happy_with(Variables).


