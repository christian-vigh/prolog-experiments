/****h* Modules/Utilities
 ===============================================================================
 *
 * NAME
 *	Utilities - Various utility predicates.
 *
 * FILE
 *	Modules/Utilities.pro
 *
 * CONTENTS
 *	Implements various utility predicates.
 *
 * AUTHOR
 *	Christian Vigh, July 2005.
 *
 ===============================================================================
 ******/



:- module(utilities).

:-	import(list).

:-	export(dup/3).
:-	export(print/1).
:-	export(print/2).
:-	export(subatom/4).

:-	export('/'/2).
:-	export(':'/2).

:- 	discontiguous print/1.

 
:- end_module(utilities).



:-	op(900, fx , print).
:- 	op(200, xfx, '/').
:-	op(200, xfx, ':').

 
:- body(utilities).







/****f* Modules.Utilities/dup
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	dup/3
 *
 * SYNTAX
 *	dup(Atom, Count, Result)
 *
 * PURPOSE
 *	[Result] is unified with an atom resulting from [Count] duplications of [Atom].
 *
 * ARGUMENTS
 *	[Atom] (i) -
 *		Atom to duplicate.
 *	[Count] (i) -
 *		Number of times to duplicate [Atom].
 *	[Result] (o) -
 *		Result of the duplication.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

dup(Atom, Count, _) :-		% Don't allow for negative counts
	Count < 0,
	!, fail.		% The cut is needed, otherwise Prolog would go 
				% through other dup() clauses after the fail

dup(Atom, 0, '').		% A count of zero will yield to an empty atom
dup(Atom, 1, Atom).		% End of recursion

dup(Atom, Count, Result) :-	% Normal case
	NewCount is Count - 1,
	dup(Atom, NewCount, TempResult),
 	atom_concat(Atom, TempResult, Result).

/******/




/****f* Modules.Utilities/print
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	print
 *
 * SYNTAX
 *	print(Expression)
 *
 * PURPOSE
 *	Prints the given expression. [Expression] can be one of the following :
 *
 *	print E1 + E2 / print (E1, E2) -
 *		prints the given expressions with separation. [E1] and [E2] can be
 *		complex expressions.
 *	
 *	print spaces(N) / print space -
 *		Prints [N] spaces ('spaces' version), or one single space ('space'
 *		version).
 *		'spaces' and 'space' can be abbreviated into 'sp'.
 *
 *	print [list] -
 *		Prints the values in [list], without any separation.
 *
 *	print nl(X) / print nl -
 *		Prints the specified number of newlines.
 *
 *	print tab(X) / print tab
 *		Prints the specified number of tabs.
 *
 *	print upper(X) / print lower(X) -
 *		Prints [X] in uppercase/lowercase. 'upper' can be abbreviated into
 *		'up', and 'lower' to 'low'.
 *
 *	print Name:X[/C] / print Name/C[:X] -
 *		Prints 'Name' not exceeding [X] characters in width. If length of
 *		[Name] is less than [X] characters, then extra spaces will be 
 *		inserted. If [C] is specified, then it will be used as the padding
 *		character instead of space.
 *
 *	print header(Name) / print attribute(Name, Value) / 
 *			     print attribute(Indent, Name, Value) -
 *		Formatted output for printing data structures (for debugging
 *		purposes, for example).
 *		print header(Name) prints the header for a structure [Name].
 *		print attribute() prints an attribute of the structure [Name], 
 *		whose value is [Value]. [Indent] specifies the indentation level
 *		for the printing (ie, number of tabs to write before writing the
 *		attribute value).
 *		'attribute' can be abbreviated into 'attr'.
 *
 *	print left(Name, Width) / print left(Name, Width, Pad) -
 *		prints [Name], left justified on [Width] characters. 
 *		If [Name] is less than [Width] characters, then extra spaces will
 *		be added (or the character [Pad]).
 *
 *	print right(Name, Width) / print right(Name, Width, Pad) -
 *		prints [Name], right-justified on [Width] characters.
 *
 *	print center(Name, Width) / print center(Name, Width, Pad) -
 *		prints [Name], centered on [Width] characters.
 *
 *	print dup(Char, Count) -
 *		prints the character [Char] repeated [Count] times.
 *
 *	print count(List) -
 *		Prints the number of elements in [List]. [List] can be an atom,
 *		in that case it will print '1'.
 *
 *	print sorted(List) -
 *		Prints [List] in sorted order.
 *
 *	print length(X) -
 *		Prints the length of atom [X].
 *
 *	print list(X) -
 *		Prints [X] as a list, enclosed in brackets and using commas to 
 *		separate individual elements.
 *
 *	print Expr -
 *		Prints the result of mathematical expression [Expr], after evaluation/
 *
 * ARGUMENTS
 *	[Expression] (i) -
 *		Expression to print.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

%
% print E1 + E2 :
%	allows to print several arguments.
print E1 + E2 :-
	print E1,
	print E2.
print (E1, E2) :-	% Same, enclosed in parentheses
	print E1, 
	print E2.
print [H|T] :-		% Same, as a list.
	print H,
	print T.
print [].

%
% keyword : spaces(N) or sp(N) or sp or space
%	Writes the specified number of space (1, for sp and space).
%
print spaces(N) :-
	dup(' ', N, X),
	write(X).
print sp(N) :-
	print spaces(N).
print space :-
	print spaces(1).
print sp :-
	print spaces(1).
	
%
% keyword : nl(X) or nl -
%	writes the specified number of newlines (1 for nl).
%
print nl :-
	nl.
print nl(X) :-
	X > 0,
	NewX is X - 1,
	print nl,
	print nl(NewX).
print nl(0).

%
% tab(X) or tab -
%	Writes the specified number of tabs (1 for tab).
%
print tab :-
	write('	').
print tab(X) :-
	X > 0,
	NewX is X - 1,
	print tab,
	print tab(NewX).
print tab(0).

%
% upper X, lower X :
%	Sets X to upper-/lowercase.
%
print upper(X) :-
	print up(X).
print up(X) :-
	upper(X, L),
	print L.
print lower(X) :-
	print low(X).
print low(X) :-
	lower(X, L),
	print L.

%
% Name:X -
% 	writes atom Name, not exceeding X characters. If length of atom is less
%	than X, then extra spaces are added.
% Name:X/C or Name/C:X -
%	Same as Name:X, except that padding characters are C's instead of spaces.
%
print Name:X :-
	atom(Name),
	subatom(Name, X, ' ', Result),
	write(Result).
print Name:X/C :-
	atom(Name),
	subatom(Name, X, C, Result),
	write(Result).
print Name/C:X :-
	print Name:X/C.

%
% header, attribute :
%	Formatted output for printing structures.
%
print header(X) :-
	print X + ' : ' + nl,
	atom_length(X, L),
	dup('~', L, S),
	print S + nl.
	
print attribute(Name, Value) :-
	print attr(Name, Value).
print attribute(Indent, Name, Value) :-
	print attr(Indent, Name, Value).

print attr(Name, Value) :-
	print attr(0, Name, Value).

	
print attr(Count, Name, [H|T]) :-
	print tab(Count) + Name + ' : ',
	do_print_attr([H|T]).
	
do_print_attr([H|T]) :-
	print H,
	optional_comma(T),
	do_print_attr(T).
do_print_attr([]).

print attr(Count, Name, Value) :-
	print tab(Count) + Name + ' : ' + value.

optional_comma([]).
optional_comma(List) :-
	print ', '.
optional_comma([], _).
optional_comma(List, C) :-
	print C.
	
%
% justification
%
print left(Name, X) :-
	print left(Name, X, ' ').
print left(Name, X, Char) :-
	atom(Name),
	subatom(Name, X, Char, left, Result),
	write(Result).
print left(Name, X, Char) :-
	number(Name), string_number(SName, Name), string_atom(SName, AName),
	subatom(AName, X, Char, left, Result),
	write(Result).

print right(Name, X) :-
	print right(Name, X, ' ').
print right(Name, X, Char) :-
	atom(Name),
	subatom(Name, X, Char, right, Result),
	write(Result).
print right(Name, X, Char) :-
	number(Name), string_number(SName, Name), string_atom(SName, AName),
	subatom(AName, X, Char, right, Result),
	write(Result).


print center(Name, X) :-
	print center(Name, X, ' ').
print center(Name, X, Char) :-
	atom(Name),
	subatom(Name, X, Char, center, Result),
	write(Result).
print center(Name, X, Char) :-
	number(Name), string_number(SName, Name), string_atom(SName, AName),
	subatom(Name, X, Char, center, Result),
	write(Result).


%
% dup(C, Count) -
%	Write character C Count times.
%
print dup(Char, Count) :-
	dup(Char, Count, X),
	write(X).


%
% count(X) -
%	Writes the number of elements in X.
%
print count(X) :-
	atom(X),
	print 1.
print count(List) :-
	length(List, L),
	print L.


%
% sorted(X) -
% 	writes X as a sorted list.
%
print sorted(X) :-
	atom(X),
	print X.
print sorted(X) :-
	sort(X, List),
	print List.	


%
% length(X) -
%	Writes length of expression x.
print length(X) :-
	atom_length(X, L),
	print L.


%
% list(X), list(X, Sep) -
% 	Prints a list	
%
print list(X) :-
	print list(X, ', ').
print list([], _).
print list([H|T], C) :-
	print H,
	optional_comma(T, C),
	print list(T, C).
print list(Atom) :-
	print Atom.

%
% mathematical expression
%
print Exp :-
	structure(Exp),
	F =.. [is, X, Exp],
	call(F),
	write(X).
	

print Exp :-
	write(Exp).

/******/



/****f* Modules.Utilities/subatom
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	subatom/4, subatom/5
 *
 * SYNTAX
 *	subatom(Atom, Length, Pad, Justify, Result)
 *	subatom(Atom, Length, Pad, Result)
 *
 * PURPOSE
 *	Unifies [Result] with a subtring of [Atom], of length [Length].
 *	If length of [Atom] is shorter than [Length], then [Pad] is used as a 
 *	padding character.
 *
 * ARGUMENTS
 *	[Atom] (i) -
 *		Atom to process.
 *	[Length] (i) -
 *		Length to be set for [Result].
 *	[Pad] (i) -
 *		Padding character.
 *	[Justify] (i) -
 *		Can be either 'left', 'center' or 'right'.
 *	[Result] (o) -
 *		Resulting string/
 *
 * NOTES
 *	subatom/4 calls subatom/5 using 'left' as the value for the [justify]
 *	argument.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

subatom(Atom, Length, Pad, Result) :-
	subatom(Atom, Length, Pad, left, Result).
	
subatom(Atom, Length, Pad, left, Result) :-
	atom_length(Atom, X),
	X =< Length,
	PadCount is Length - X,
	dup(Pad, PadCount, PadChars),
	atom_concat(Atom, PadChars, Result).

subatom(Atom, Length, Pad, right, Result) :-
	atom_length(Atom, X),
	X =< Length,
	PadCount is Length - X,
	dup(Pad, PadCount, PadChars),
	atom_concat(PadChars, Atom, Result).

subatom(Atom, Length, Pad, center, Result) :-
	atom_length(Atom, X),
	X =< Length,
	PadCount is Length - X,
	PadLeft is PadCount // 2,
	PadRight is PadLeft + ( PadCount mod 2),
	dup(Pad, PadLeft, PadLeftChars),
	dup(Pad, PadRight, PadRightChars),
	atom_concat(PadLeftChars, Atom, R1),
	atom_concat(R1, PadRightChars, Result).

subatom(Atom, Length, _, _, Result) :-
	sub_atom(Atom, 1, Length, Result).

/******/

:- end_body(utilities).
 