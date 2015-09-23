%   File   : HELP.PL
%   Author : L. Hardman
%   Updated: 6 April 1984
%   Purpose: Extract predicate names and descriptions from files.



:- public
	help/0,
	help/1,
	help/2.

:- mode
	help(+),
	help(+,+),
	search_help_file(+,+,-),
	search_source_file(+,+),
		look_for_predicate(+,+),
		print_if_not_end_of_comment(+),
	parse_help_line(+,+,-,+,?),
		has_paren(+,-,+,?),
		get_file(-,+,?),
		get_arity(+,?,+,?),
			get_arity(+,+,-,+,?),
	parse_source_line(+,+,+,?),
	read_a_line(-),
	skip_string(+,+,?),
	skip_string(+,+,+,?),
	skip_spaces(+,?).


%#  help
%^  Prints out information on how to use the help facility.
%$

help :-
	write('Type "help(predicate_name)" or "help(predicate_name, arity)"'),nl,
	write('to get help on a particular predicate.'), nl.


%#  help(Predicate)
%^  Prints out the labelled comments for Predicate.
%$

help(Predicate) :-
	help(Predicate, _).

%#  help(Functor, Arity)
%^  Prints out the labelled comments for Functor/Arity.
%$

help(Functor, Arity) :-
	nofileerrors,
	seeing(OldFile),
	(   see('Util:Util.Hlp'),
	    search_help_file(Functor, Arity, File),	% Look for "File"
							% containing "Functor"
	    seen,
	    fileerrors
	;
	    fileerrors,
	    fail
	),
	nofileerrors,
	(   see(File),
	    search_source_file(Functor, Arity),		% Find and print out
	    						% comment
	    seen,
	    fileerrors
	;
	    fileerrors,
	    fail
	),
	see(OldFile).


%   search_help_file(Functor, Arity, File)
%   looks through the input stream line by line.
%   It succeeds when it finds a line with the correct predicate in it
%   and returns the name of the File containing Functor/Arity.

search_help_file(FunctorAtom, Arity, FileAtom) :-
	name(FunctorAtom, Functor),
	repeat,
	read_a_line(Line),
	parse_help_line(Functor, Arity, File, Line, []),
	name(FileAtom, File).


%   search_source_file(Functor, Arity)
%   looks through the input stream line by line.
%   It succeeds after the comments on Functor/Arity have been printed out.

search_source_file(Functor, Arity) :-
	look_for_predicate(Functor, Arity),
	look_for_comment,
	print_comment.

look_for_predicate(FunctorAtom, Arity) :-
	name(FunctorAtom, Functor),
	repeat,
	read_a_line(Line),
	parse_source_line(Functor, Arity, Line, []),
	skip_string(37, 35, Line, Rest_of_line), %  Find "%" followed by "#"
	writef(' %s',[Rest_of_line]), nl.

look_for_comment :-
	repeat,
	read_a_line(Line),
	skip_string(37, 94, Line, Rest_of_line), %  Find "%" followed by "^"
	writef(' %s',[Rest_of_line]), nl.

print_comment :-				 %  Failure driven
	repeat,
	read_a_line(Line),
	print_if_not_end_of_comment(Line).

print_if_not_end_of_comment(Line) :-
	skip_string(37, 36, Line, _),		 %  Find "%" followed by "$"
	!.						%  Finished printing
print_if_not_end_of_comment(Line) :-
	skip_string(37, Line, Rest_of_line),		%  Scrap initial "%"
	writef('%s',[Rest_of_line]), nl,
	!,
	fail.



%   parse_help_line(Pred, Arity, File) has to parse a help line.
%   There are no leading spaces or tabs on one of these lines.
%   The predicate symbol is everything up to the first "(" or
%   layout character.  If there is no "(" the arity is 0.  If
%   there is a "(", the arity is the number of commas up to
%   the matching ")" plus 1.  In either case we then have to
%   skip to the "%", and the File is everything after that.

parse_help_line(Predicate, Arity, File) -->
	has_paren(Predicate, HadParen),
	{   Predicate \== []   },
	get_arity(HadParen, Arity),
	skip_string(37),
	get_file(File).


has_paren([], no, [], []) :- !.
has_paren([], yes) -->
	"(", !.
has_paren([], no) -->
	[C], {C =< 32}, !.
has_paren([C|Cs], HadParen) -->
	[C],
	has_paren(Cs, HadParen).


get_file([C|Cs]) -->
	[C], {C > 32}, !,
	get_file(Cs).
get_file([]) --> [].


get_arity(no, 0) --> !.
get_arity(yes, N) -->
	get_arity(0, 1, N).

get_arity(Depth, SoFar, Arity) -->
	(   "'", !, skip_string(39), get_arity(Depth, SoFar, Arity)
	|   """",!, skip_string(34), get_arity(Depth, SoFar, Arity)
	|   "(", !, {E is Depth+1},  get_arity(E, SoFar, Arity)
	|   ")", {Depth = 0}, !, {Arity = SoFar}
	|   ")", !, {E is Depth-1},  get_arity(E, SoFar, Arity)
	|   ",", {Depth = 0}, !, {Next is SoFar+1}, get_arity(Depth, Next, Arity)
	|   [_], get_arity(Depth, SoFar, Arity)
	).

%   parse_source_line(Pred, Arity) has to parse a
%   line in the source file.

parse_source_line(Predicate, Arity) -->
	skip_string(37,35),		%  Find "%" followed by "$"
	skip_spaces,
	has_paren(Predicate, HadParen),
	{   Predicate \== []   },
	get_arity(HadParen, Arity).


%   read_a_line(Line)
%   reads the next line from the current input stream.
%   If there IS a next line, Line is unified with the characters
%   up to but excluding the closing newline (31) character.
%   If there is NOT a next line, Line is unified with 'end_of_file'.

read_a_line(Line) :-
	get0(Char),
	read_a_line(Char, Line).

read_a_line(26, end_of_file) :- !.
read_a_line(Other, Line) :-
	rest_of_line(Other, Line).

rest_of_line(31, []) :- !.
rest_of_line(Char, [Char|Line]) :-
	get0(NextChar),
	rest_of_line(NextChar, Line).


%   skip_string(Char, Line, Rest_of_line) and
%   skip_string(Char1, Char2, Line, Rest_of_line)
%   both scan the line until they find the relevant characters.

skip_string(C) --> [C], !.
skip_string(C) --> [_], skip_string(C).

skip_string(ThisChar, NextChar) -->
	skip_string(ThisChar),
	[NextChar],!.


%   skip_spaces skip over any number of spaces and tabs.

skip_spaces --> [32], !, skip_spaces.
skip_spaces --> [ 9], !, skip_spaces.
skip_spaces --> [].
