%   File   : PUTSTR.PL
%   Author : 
%   Updated: 24 February 1984
%   Purpose: Write out a string which may be held in another file.

/*  This file forms part of a package for taking large blocks of
    text (such as help information) out of Prolog programs into
    ordinary text files.  This is the bit that prints a string
    regardless of where it lives.  The other part was going to be
    written in Pascal.  It hasn't been written at all yet.
*/

:- public
	putstr/1.

:- mode
	putstr(+),
	copy_chars(+),
	skip_chars(+).


putstr(s(File,Start,Length)) :-
	seeing(Old),
	see(File),
	skip_chars(Start),
	copy_chars(Length),
	seen,
	see(Old).
putstr(Atom) :-
	atomic(Atom),
	write(Atom).


skip_chars(0) :- !.
skip_chars(N) :-
	get0(_),
	M is N-1,
	skip_chars(M).


copy_chars(0) :- !.
copy_chars(N) :-
	get0(C),
	put(C),
	M is N-1,
	copy_chars(M).

