%   File   : STRIO.PL
%   Author : R.A.O'Keefe
%   Updated: 19 October 1984
%   Purpose: Prolog input/output to character strings.
%   Needs  : lib(read), lib(write).

:- public
	read_chars/2,
	read_chars/3,
	display_chars/2,
	print_chars/2,
	write_chars/2,
	writeq_chars/2.

/*  read_chars(Chars, Term[, Dictionary])
    reads a term as read/1-2 would, except that instead of reading
    from a file, a list of character codes is read from, and as
    only one term can be read, there is no need to end with ". ".
    read_string(String...) would have the same effect but would
    read from a byte vector.

    write_chars(Chars, term)
    and display_chars, writeq_chars, print_chars
    all write a term as the corresponding portable_foo would, except
    that instead of writing to a file, they write to a list of
    character codes.  write_string and so on would write to a byte
    vector.

    To hack this on the Dec-10, I do in fact use a file _PLIO_.TMP
    which is deleted after each use.
*/

:- mode
	'output chars'(+, ?),
	'slurp chars'(+, -),
	'valid chars'(+).


read_chars(Chars, Term, Dict) :-
	seeing(OldInput),
	telling(OldOutput),
	(   'valid chars'(Chars), !
	;   tell(user), write('! Bad first argument to '),
	    writeq(read_chars(Chars, Term, Dict)), nl,
	    tell(OldOutput), fail
	),
	tell('_PLIO_.TMP'),
	puts(Chars),
	puts(" . "),		%  note first space in case user had .
	told,
	tell(OldOutput),
	see('_PLIO_.TMP'),
	read(T, D),
	rename('_PLIO_.TMP', []),
	see(OldInput),
	Term = T, Dict = D.


read_chars(Chars, Term) :-
	read_chars(Chars, Term, _).


'valid chars'(X) :-
	var(X), !, fail.
'valid chars'([]).
'valid chars'([Char|Chars]) :-
	integer(Char),
	0 =< Char, Char =< 127,
	'valid chars'(Chars).


display_chars(Chars, Term) :-
	'output chars'(portable_display(Term), Chars).

print_chars(Chars, Term) :-
	'output chars'(portable_print(Term), Chars).

write_chars(Chars, Term) :-
	'output chars'(portable_write(Term), Chars).

writeq_chars(Chars, Term) :-
	'output chars'(portable_writeq(Term), Chars).


'output chars'(Goal, Chars) :-
	telling(OldOutput),
	tell('_PLIO_.TMP'),
	call(Goal),
	told,
	tell(OldOutput),
	seeing(OldInput),
	see('_PLIO_.TMP'),
	get0(C),
	'slurp chars'(C, Chrs),
	rename('_PLIO_.TMP', []),
	Chars = Chrs.


'slurp chars'(26, []) :- !.
'slurp chars'(Char, [Char|Chars]) :-
	get0(C),
	'slurp chars'(C, Chars).


	


