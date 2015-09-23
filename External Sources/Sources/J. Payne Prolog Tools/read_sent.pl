%   File   : /usr/lib/prolog/read_sent
%   Author : R.A.O'Keefe
%   Updated: 11 November 1983
%   Purpose: to provide a flexible input facility
%   Modified for NIP and generalised: Ken Johnson, 24 April 1987

/*  The main predicates provided by this file are
	read_until(+Delimiters, -Answer)
	read_line(-String)
	trim_blanks(+RawString, -Trimmed)
	read_sentence(-ListOfTokens).

	The effect of this version is slightly different from that of
	R O'Keefe's original, and I hope therefore more generally
	useful. The predicate "read_sentence(W)" instantiates W to be a
	list of atoms. For example if you type
		?- read_sentence(W).
	Prolog prompts
		|:
	You type, say
		Show me the way to go home.
	Prolog instantiates W to
		[show,me,the,way,to,go,home,.]
*/

/*  read_sentence(Words)
    reads characters up to the next period, which may be  several  lines
    distant from the start, skips to the end of that line, and turns the
    result  into  a  list of tokens.  It can happen that the sentence is
    not well formed, if say there is an unmatched double quote.  In that
    case all the characters will still be read, but chars_to_words  will
    fail  and  so  read_sentence  will fail.  read_sentence will NOT try to read
    another sentence.
*/

read_sentence(Words) :-
	read_until("!?.", Chars),
	is_newline(NL),
	read_until([NL], _),		% skip to end of line
	!,
	chars_to_words(Chars, Words),
	!.

/*  read_until(Delimiters, Answer)
    reads characters from the current input until  a  character  in  the
    Delimiters  string  is  read.  The characters are accumulated in the
    Answer string, and include the closing  delimiter.   Prolog  returns
    end-of-file as -1. The end of the file is always a delimiter.
*/

read_until(Delimiters, [Char|Rest]) :-
	get0(Char),
	read_until(Char, Delimiters, Rest).


read_until(Char, Delimiters, []) :-
	'readsent:memberchk'(Char, [-1|Delimiters]), !.
read_until(_, Delimiters, Rest) :-
	read_until(Delimiters, Rest).


%   The following predicates define useful character classes.

is_endfile(-1).

is_newline(10).

is_layout(Char) :-
	Char =< " ".			% includes tab, newline, ^S, &c

is_lower(Char) :-
	Char >= "a", Char =< "z".	% lower case letter

is_upper(Char) :-
	Char >= "A", Char =< "Z".	% upper case letter

is_letter(Char) :-
	D is Char\/32,			% D is Char forced to lower case
	D >= "a", D =< "z".		% D is a lower case letter.

is_letter(39).				% Apostrophe (Ken Johnson)
is_letter(45).				% Hyphen (Ken Johnson)

is_digit(Char) :-
	Char >= "0", Char =< "9".	% decimal digit

is_period(Char) :-
	'readsent:memberchk'(Char, ".!?").		% sentence terminator

is_punct(Char) :-
	'readsent:memberchk'(Char, ",;:").		% other punctuation mark

is_paren(Left,Right) :-			% brackets
	'readsent:memberchk'([Left,Right], ["()","[]","{}","<>"]).


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
    tokens, 
*/

chars_to_words(Chars,Words) :- 
        chars_to_words(Words,Chars,[]).

chars_to_words([Word|Words],A,B) :- 
        chars_to_word(Word,A,C),  !,
        chars_to_words(Words,C,B).

chars_to_words([],A,A).


chars_to_word(Word,A,B) :- 
        'C'(A,Char,C),
        is_layout(Char),  !,
        chars_to_word(Word,C,B).

chars_to_word(Word,A,B) :- 
        'C'(A,Char,C),
        is_letter(Char),  !,
        chars_to_atom(Chars,C,B),
        case_shift([Char|Chars],Name),
        name(Word,Name).

chars_to_word(Word,A,B) :- 
        'C'(A,Char,C),
        is_digit(Char),  !,
        Init is Char-48,
        chars_to_integer(Init,Word,C,B).

chars_to_word(Word,A,B) :- 
        'C'(A,Quote,C),
        Quote is 34,  !,
        chars_to_string(Quote,String,C,B),
        name(Word,String).

chars_to_word(Punct,A,B) :- 
        'C'(A,Char,B),
        name(Punct,[Char]).

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


chars_to_atom([Char|Chars],A,B) :- 
        'C'(A,Char,C),
        is_letter(Char),  !,
        chars_to_atom(Chars,C,B).

chars_to_atom([],A,A).

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

chars_to_integer(Init,Final,A,B) :- 
        'C'(A,Char,C),
        is_digit(Char),  !,
        Next is Init*10-48+Char,
        chars_to_integer(Next,Final,C,B).

chars_to_integer(Final,Final,A,A).

/*  chars_to_string(Quote, String)
    reads the rest of a string which was opened by  a  Quote  character.
    The  string is expected to end with a Quote as well.  If there isn't
    a matching Quote, the attempt to parse the  string  will  FAIL,  and
    thus the whole parse will FAIL.  I would prefer to give some sort of
    error  message and try to recover but that is application dependent.
    Two adjacent Quotes are taken as one, as they are in Prolog itself.
*/

chars_to_string(Quote,[Quote|String],A,B) :- 
        'C'(A,Quote,C),
        'C'(C,Quote,D),  !,
        chars_to_string(Quote,String,D,B).

chars_to_string(Quote,[],A,B) :- 
        'C'(A,Quote,B),  !.

chars_to_string(Quote,[Char|String],A,B) :- 
        'C'(A,Char,C),  !,
        chars_to_string(Quote,String,C,B).

/*  read_line(Chars)
    reads characters up to the next newline or the end of the file, and
    returns them in a list, including the newline or end of file.  When
    you want multiple spaces crushed out, and the newline dropped, that
    is most of the time, call trim_blanks on the result.
*/
read_line(Chars) :-
	is_newline(NL),
	read_until([NL], Chars).


% Utility

'readsent:memberchk'(A,[A|_]) :-
	!.

'readsent:memberchk'(A, [_|B]) :-
	'readsent:memberchk'(A,B).


