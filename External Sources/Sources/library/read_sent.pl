%   File   : /usr/lib/prolog/read_sent
%   Author : R.A.O'Keefe
%   Updated: 11 November 1983
%   Purpose: to provide a flexible input facility
%   Needs  : memberchk from utils.

/*  The main predicates provided exported by this file are
	read_until(+Delimiters, -Answer)
	read_line(-String)
	trime_blanks(+RawString, -Trimmed)
	read_sentence(-ListOfTokens).
*/

:- public
	case_shift/2,
	chars_to_words/2,
	is_digit/1,
	is_endfile/1,
	is_layout/1,
	is_letter/1,
	is_lower/1,
	is_newline/1,
	is_paren/2,
	is_period/1,
	is_punct/1,
	is_upper/1,
	read_line/1,
	read_snt/1,
	read_until/2,
	trim_blanks/2.

:- mode
	case_shift(+, -),
	chars_to_atom(-, ?, ?),
	chars_to_integer(+, -, ?, ?),
	chars_to_string(+, -, ?, ?),
	chars_to_words(+, -),
	chars_to_words(-, ?, ?),
	chars_to_word(-, ?, ?),
	is_digit(+),
	is_endfile(?),
	is_layout(+),
	is_letter(+),
	is_lower(+),
	is_newline(?),
	is_paren(?, ?),
	is_period(+),
	is_punct(+),
	is_upper(+),
	read_line(-),
	read_sent(-),
	read_until(+, -),
	read_until(+, +, -),
	trim_blanks(+, -),
	trim_blanks_rest_word(+, -),
	trim_blanks_next_word(+, -).



/*  read_until(Delimiters, Answer)
    reads characters from the current input until  a  character  in  the
    Delimiters  string  is  read.  The characters are accumulated in the
    Answer string, and include the closing  delimiter.   Prolog  returns
    end-of-file as ^Z (26)  regardless of the user's assignment (e.g. if
    you use ^D as end of file, Prolog still returns ^Z).  The end of the
    file is always a delimiter.
*/
read_until(Delimiters, [Char|Rest]) :-
	get0(Char),
	read_until(Char, Delimiters, Rest).


read_until(Char, Delimiters, []) :-
	memberchk(Char, [26|Delimiters]), !.
read_until(_, Delimiters, Rest) :-
	read_until(Delimiters, Rest).



%   The following predicates define useful character classes.

is_newline(31).				% the Dec-10 line terminator.
					% UNIX systems: use 10.

is_endfile(26).				% the file terminator ^Z
					% UNIX systems: *still* use 26.

is_layout(Char) :-
	Char =< " ".			% includes tab, newline, ^S, &c

is_lower(Char) :-
	Char >= "a", Char =< "z".	% lower case letter

is_upper(Char) :-
	Char >= "A", Char =< "Z".	% upper case letter

is_letter(Char) :-
	is_lower(Char) | is_upper(Char).

is_digit(Char) :-
	Char >= "0", Char =< "9".	% decimal digit

is_period(Char) :-
	memberchk(Char, ".!?").		% sentence terminator

is_punct(Char) :-
	memberchk(Char, ",;:").		% other punctuation mark

is_paren(Left,Right) :-			% brackets
	memberchk([Left,Right], ["()","[]","{}","<>"]).


/*  trim_blanks(RawInput, Cleaned)
    removes leading and trailing layout characters  from  RawInput,  and
    replaces  internal  groups  of  layout  characters by single spaces.
    Thus trim_blanks(<|TAB TAB a SP ^M ^E b ^Z|>, "a b") would be true.
*/
trim_blanks([Char|Chars], Cleaned) :-
	is_layout(Char), !,
	trim_blanks(Chars, Cleaned).
trim_blanks([Char|Chars], [Char|Cleaned]) :- !,
	trim_blanks_rest_word(Chars, Cleaned).
trim_blanks([], []).


trim_blanks_rest_word([Char|Chars], Cleaned) :-
	is_layout(Char), !,
	trim_blanks_next_word(Chars, Cleaned).
trim_blanks_rest_word([Char|Chars], [Char|Cleaned]) :- !,
	trim_blanks_rest_word(Chars, Cleaned).
trim_blanks_rest_word([], []).


trim_blanks_next_word([Char|Chars], Cleaned) :-
	is_layout(Char), !,
	trim_blanks_next_word(Chars, Cleaned).
trim_blanks_next_word([Char|Chars], [32,Char|Cleaned]) :- !,
	trim_blanks_rest_word(Chars, Cleaned).
trim_blanks_next_word([], []).



/*  chars_to_words(Chars, Words)
    parses a list of characters (read by read_until) into a list of
    tokens, where a token is
	'X' for X a period or other punctuation mark, e.g. ';'
	atom(X) for X a sequence of letters, e.g. atom(the)
	integer(X) for X a sequence of digits, e.g. integer(12)
	apost for '
	aposts for 's
	string(X) for X "..sequence of any.."
    Thus the string "the "Z-80" is on card 12." would be parsed as
	[atom(the),string('Z-80'),atom(is),atom(on),atom(card),
	 integer(12),'.'].
    It is up to the sentence parser to decide what to do with these.
    Note that the final period, if any, is retained.  The parser may
    need it.
*/
chars_to_words(Chars, Words) :-
	chars_to_words(Words, Chars, []).


chars_to_words([Word|Words]) -->
	chars_to_word(Word), !,
	chars_to_words(Words).
chars_to_words([]) --> [].


chars_to_word(Word) -->
	[Char], {is_layout(Char)}, !,
	chars_to_word(Word).
chars_to_word(atom(Word)) -->
	[Char], {is_letter(Char)}, !,
	chars_to_atom(Chars),
	{case_shift([Char|Chars], Name)},
	{name(Word, Name)}.
chars_to_word(integer(Word)) -->
	[Char], {is_digit(Char)}, !,
	{Init is Char-"0"},		% integer value of char
	chars_to_integer(Init, Word).
chars_to_word(aposts) -->
	"'s", !.
chars_to_word(apost) -->
	"'", !.
chars_to_word(string(Word)) -->
	[Quote], {Quote is """"}, !,	% NB Quote is an integer (34)
	chars_to_string(Quote, String),
	{name(Word, String)}.
chars_to_word(Punct) -->
	[Char],
	{name(Punct, [Char])}.


/*  chars_to_atom(Tail)
    reads the remaining characters of a word.  Case conversion  is  left
    to  another  routine.   In this application, a word may only contain
    letters but they may be in either case.  If you want to parse French
    you will have to decide what to do about accents.  I suggest putting
    them after the vowel, and adding a clause
	chars_to_atom([Vowel,Accent|Chars]) -->
		[Vowel],	{accentable_vowel(Vowel)},
		[Accent],	{accent_for(Vowel, Accent)},
		!.
	with the obvious definitions of accentable_vowel and accent_for.
    Note that the Ascii characters ' ` ^ are officially  designated  the
    "accent acute", "accent grave", and "circumflex".  But this file was
    originally written for an English parser and there was no problem.
*/
chars_to_atom([Char|Chars]) -->
	[Char], {is_letter(Char)}, !,
	chars_to_atom(Chars).
chars_to_atom([]) --> [].


/*  case_shift(Mixed, Lower)
    converts all the upper case letters in Mixed to lower  case.   Other
    characters (not necessarily letters!) are left alone.  If you decide
    to accept other characters in words only chars_to_atom has to alter.
*/
case_shift([Upper|Mixed], [Letter|Lower]) :-
	is_upper(Upper),
	Letter is Upper-"A"+"a", !,
	case_shift(Mixed, Lower).
case_shift([Other|Mixed], [Other|Lower]) :-
	case_shift(Mixed, Lower).
case_shift([], []).


/*  chars_to_integer(Init, Final)
    reads the remaining characters of an integer which starts  as  Init.
    NB:  this  parser  does  not  know about negative numbers or radices
    other than 10, as it was written for PDP-11 Prolog.
*/
chars_to_integer(Init, Final) -->
	[Char], {is_digit(Char)}, !,
	{Next is Init*10-"0"+Char},
	chars_to_integer(Next, Final).
chars_to_integer(Final, Final) --> [].


/*  chars_to_string(Quote, String)
    reads the rest of a string which was opened by  a  Quote  character.
    The  string is expected to end with a Quote as well.  If there isn't
    a matching Quote, the attempt to parse the  string  will  FAIL,  and
    thus the whole parse will FAIL.  I would prefer to give some sort of
    error  message and try to recover but that is application dependent.
    Two adjacent Quotes are taken as one, as they are in Prolog itself.
*/
chars_to_string(Quote, [Quote|String]) -->
	[Quote,Quote], !,
	chars_to_string(Quote, String).
chars_to_string(Quote, []) -->
	[Quote], !.
chars_to_string(Quote, [Char|String]) -->
	[Char], !,
	chars_to_string(Quote, String).


/*  read_line(Chars)
    reads characters up to the next newline or the end of the file, and
    returns them in a list, including the newline or end of file.  When
    you want multiple spaces crushed out, and the newline dropped, that
    is most of the time, call trim_blanks on the result.
*/
read_line(Chars) :-
	is_newline(NL),
	read_until([NL], Chars).


/*  read_sent(Words)
    reads characters up to the next period, which may be  several  lines
    distant from the start, skips to the end of that line, and turns the
    result  into  a  list of tokens.  It can happen that the sentence is
    not well formed, if say there is an unmatched double quote.  In that
    case all the characters will still be read, but chars_to_words  will
    fail  and  so  read_sent  will fail.  read_sent will NOT try to read
    another sentence.
*/
read_sent(Words) :-
	read_until("!?.", Chars),
	is_newline(NL),
	read_until([NL], _),		% skip to end of line
	!,
	chars_to_words(Chars, Words),
	!.

