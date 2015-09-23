%   File   : GETFIL.PL
%   Author : Richard A. O'Keefe
%   Updated: 1 June 84
%   Purpose: read a file name from the terminal
%   Updated for NIP   Ken Johnson, 24-6-87

/*  This is for use in tools for analysing Prolog (or other) files.
    getfile(File) prompts Next file: and reads a file name without
    any extraneous punctuation.  The file name is returned as an
    atom.

    The end user should type the file name as a series of characters
    terminated by a newline. Quotes are NOT necessarty EVEN IF the
    file name has a dot in it. The newline is not returned as part
    of the name. The name is returned as a Prolog atom.

    Calling sequence:
	getfile(-Name)
*/


getfile(File) :-
	telling(What),
	tell(user),
	ttyflush,
	write('Next file: '),
	told,
	tell(What),
	get0(Char),
	rest_of_a_line(Char, Chars),
	name(File,Chars).
			

rest_of_a_line(10, []) :- !.	%  <NL>

rest_of_a_line(Ch, [Ch|Rest]) :-
	get0(C2),
	rest_of_a_line(C2, Rest).


