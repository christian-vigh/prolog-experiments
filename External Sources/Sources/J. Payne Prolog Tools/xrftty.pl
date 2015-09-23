%   File   : XRFTTY.PL
%   Author : Dave Bowen
%   Updated: 19 September 1984
%   Purpose: Terminal interaction for XREF

:- public
	member/2.				% this is Called

:- mode
	actionchar(+, +, -),
	append(?, ?, ?),
	get_crf(-, -),
	get_globals_file(-),
	get_title(-),
	get_update_globals(?),
	get_width(-),
	member(?, +),
	readtonl(-),
	readtonl0(-),
	readln(+, -),
	see_chek(+),
	tell_chek(+),
	warn(+, +),
	writepred(+, +),
	writes(+),
	yesno(+, ?).


				/* Get name for cross-refs file: instantiate
				   both File and Chars */

get_crf(File, Chars) :- 			%  Name specified in DEF file?
	recorded('$define', cross_ref_file(File), Ref),
	erase(Ref), !,				%  Yes, remove from database
	name(File, Chars).
get_crf(File, Chars) :-
	repeat,
	    ttynl, display('Filename for Cross-Reference listing: '), ttyflush, 
	    readtonl(Chars),
	!,
	name(File, Chars),			%  Read file name from terminal
	check_filename(File).			%  Check name is appropriate

				/* Get required page width */
get_width(W) :-
 	recorded('$define', width(W), Ref),	% Was it specified in DEF file?
 	erase(Ref), !.
get_width(W) :-					% No, prompt for it.
 	repeat,
	    ttynl, display('Width: '), ttyflush,
	    readtonl(Chars),
	!,
	name(W, Chars),
	check_width(W).

				/* Get title for Cross-Reference listing */
get_title(T) :-
	recorded('$define',title(T),Ref),	% Specified in a DEF file?
	erase(Ref), !.
get_title(T) :-
	ttynl, display('Title: '), ttyflush,	% Not in DEF file, ask for it.
	readtonl0(T).				% Empty line is allowed.

			/* Gets filename (or "no") for import/export lists */

get_globals_file(File) :-			% Specified in DEF file?
	recorded('$define', globals_file(File), Ref),
	erase(Ref), !,
	File \== no.
get_globals_file(File) :-			% No, ask for it.
	yesno('Do you want a listing of imports/exports', yes),
	repeat,
	    ttynl, display('Filename for imports/exports: '), ttyflush, 
	    readtonl(Chars),			% No cut; if we can't write
	    name(File, Chars).			% the file, try another name.

				/* Does the user want us to update the
				   Import/Export lists in all the files? */
get_update_globals(Yes_or_No) :-
	recorded('$define', update_globals(Y_or_N), Ref),
	erase(Ref), !,
	Y_or_N = Yes_or_No.
get_update_globals(Yes_or_No) :-
	yesno('Alter the import/export declarations in your files', Yes_or_No).


% Utilities for input/output 

member(X, [X|_]).
member(X, [_|L]) :-
	member(X, L).


append([], L, L).
append([H|T], L, [H|R]) :-
	append(T, L, R).


yesno(Question, Answer) :-
	repeat,
	    ttynl, display(Question), display('? '), ttyflush,
	    readtonl(Ans),
	    (   Ans = [0'Y|_], !, Answer = yes
	    ;   Ans = [0'N|_], !, Answer = no
	    ;   display('! Answer y(es) or n(o)'), fail
	    ).

			/* Give a warning about a predicate */
warn(Pred, State) :-
	functor(Pred, F, N),
	telling(File), tell(user),
	write('** WARNING: '), write(F/N is State), nl,
	tell(File).


writepred('<user>', 0) :- !,
	write('<user>').
writepred(F, N) :-
	writeq(F/N).


				/* See file or complain if it doesn't exist */
see_chek(File) :-
	(   nofileerrors, see(File), !, fileerrors
	;   fileerrors,
	    display('! Can''t read '), display(File), ttynl,
	    fail
	).

				/* Open file for output or complain */
tell_chek('TTY:') :- !,
	tell(user).
tell_chek(File) :-
	(   nofileerrors, tell(File), !, fileerrors
	;   fileerrors,
	    display('! Can''t write '), display(File), ttynl,
	    fail
	).


% Low level input routines


				/* Read a line, returning character list */
readtonl(Cs) :-
	readln(ignore_blanks,Cs).		% - for reading file names

readtonl0(Cs) :-
	readln(keep_blanks,Cs).			% - for reading text

readln(Flag,Cs) :- 
	get0(C),
	actionchar(Flag,C,Cs).

actionchar(_,31,[]) :- !.			% newline: return []
actionchar(_,26,_) :- !, fail.			% ^Z: fail
actionchar(ignore_blanks,C,Cs) :-		% ignore layout characters
	C=<32, !, 				%  (incl. space) if reqd
	readln(ignore_blanks,Cs).
actionchar(Flag,C,[Cfirst|Crest]) :- !,		% other: construct list
	("a"=<C, C=<"z", !, 			% convert lower to upper case
	    Cfirst is C+"A"-"a"
	;   Cfirst=C),
	readln(Flag,Crest).
