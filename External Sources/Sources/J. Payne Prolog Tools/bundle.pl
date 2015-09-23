%   File   : BUNDLE.PL
%   Author : R.A.O'Keefe
%   Updated: 22 September 1984
%   Purpose: Bundle and Unbundle files.
%   Needs  : append/3 and reverse/2 from LISTUT.PL (lib(lists)).

/*  This file defines two commands:

	<output> bundle [<label1> = <in1>, ..., <labelK> = <inK>].

	unbundle <input>.

    The former writes, for each of the <inI> files in turn,
	%?BEGIN <labelI> -- <inI>
	{contents of file <inI>}
	%?ENDOF <labelI> -- <inI>
    There is no special mark at the beginning or end of the file.

    The latter reads the <input>, and writes every %?BEGIN..%?ENDOF
    section to a file whose name is the given label.

    If you omit a <label>=, BUNDLE will generate one for you.  In
    any case, it will derive a label from what you give it rather
    than taking it as it stands.  Labels end up as FORTRAN 6-letter
    variable names, with - turned to 8 and _ turned to 9.  These
    seem to be acceptable file names on most file systems.

    The code below does assume that the end of file character is 26.
    In some Prologs this is -1.  That's the only use made of 26.
    The program accepts either ^_ (31, Dec-10 Prolog's mapping of
    CRLF) or ^J (LF) as end of line in input and generates whatever
    nl generates in output.  You should only have to change this if
    your Prolog uses CR (change 31 to 13).

    If your Prolog doesn't do TRO you will have a tough time with
    'bundle', as it recurses all over the place.  If asked nicely I
    might do something about that, but 'repeat' loops are alien to
    Prolog as the 'unbundle' mess shows.  C Prolog should be able to  
    unbundle things all right, but not to bundle them, but you would
    be better advised to write a shell script to bundle things on UNIX.

    It is recommended that you include files called UNIX, TOPS10,
    TOPS20, VMS, or whatever, as appropriate, which can be run as
    commands by the operating system of that name, to rename the files
    to a good place.  E.g. if this file were part of a bundle, we might
    have
	UNIX   = mv BUNDLE /usr/lib/prolog/bundle.pl
	       : rm TOPS10 UNIX
	TOPS10 = rename UTIL:BUNDLE.PL=BUNDLE.
	       : delete TOPS10.,UNIX.
    Even if someone has an operating system not in your list, one of
    these files might be a good place to start editing.
*/

:- public
	bundle/1,
	bundle/2,
	help/0,
	unbundle/1.

:- op(100, xfx, bundle).
:- op(100,  fx, bundle).
:- op(100,  fx, unbundle).

:- mode
	bundle(+),
	bundle(+, +),
	bundle1(+, +),
	bundle2(+, +),
	copy_to_bundle(+, +),
	get_one_line(-),
	get_one_line(+, -),
	keep_letters(+, +, -),
	unbundle(+),
	unbundle1(+),
	unbundle2(+, -),
	unbundle3(+, -),
	write_one_line(+).



help :-
	write('Output bundle Files wraps up a list of files into a bundle
	and writes them to Output.  Files is a list of Label=File pairs.
unbundle File unwraps a bundle of files into its components.
').


Output bundle Files :-
	tell(Output),
	bundle Files,
	told.

bundle [].
bundle [Label=File|Files] :- !,
	bundle1(File, Label),
	bundle Files.
bundle [File|Files] :-
	bundle1(File, File),
	bundle Files.


bundle1(File, Label) :-
	atom(Label),
	name(Label, Chars),
	reverse(Chars, Rev),
	(   append(Hrev, [0':|_], Rev)		%  Tops-10
	;   append(Hrev, [0'>|_], Rev)		%  Tops-20
	;   append(Hrev, [0'/|_], Rev)		%  UNIX
	;   append(Hrev, [0']|_], Rev)		%  VMS
	;   Hrev = Rev				%  no device/directory
	),  !,
	reverse(Hrev, Head),
	keep_letters(Head, 6, Letters),
	name(ShortLabel, Letters),
	ShortLabel \== '',
	bundle2(File, ShortLabel).
bundle1(File, Label) :-
	telling(Bundle),
	tell(user),
	write('Can''t generate a label from '), write(Label),
	write(', '), write(File),
	write(' not written to '), write(Bundle), nl,
	tell(Bundle).


keep_letters([Char|Chars], N, [Keep|Keeps]) :-
	N > 0, M is N-1,
	(   0'A =< Char, Char =< 0'Z, Keep = Char
	;   0'a =< Char, Char =< 0'z, Keep is Char-32
	;   0'0 =< Char, Char =< 0'9, Keep = Char
	;   0'- == Char,	      Keep = 0'8	% for VMS
	;   0'_ == Char,	      Keep = 0'9	% for VMS
	),  !,
	keep_letters(Chars, M, Keeps).
keep_letters(_, _, []).


bundle2(File, Label) :-
	nofileerrors,
	see(File),
	!,
	fileerrors,
	write('%?BEGIN '), write(Label), write(' -- '), write(File), nl,
	get0(C),
	copy_to_bundle(C, yes),
	seen,
	write('%?ENDOF '), write(Label), write(' -- '), write(File), nl.
bundle2(File, Label) :-
	fileerrors,
	telling(Bundle),
	tell(user),
	write('Can''t open '), write(File), write(', '),
	write(Label), write(' not written to '), write(Bundle), nl,
	tell(Bundle).


%   copy_to_bundle(Char, Flag)
%   loops around reading characters and writing them to the bundle file.
%   The Flag indicates whether the last character written was a newline.
%   This is so that we can be sure that the %?BEGIN and %?ENDOF marks
%   always start at the beginning of a line.  Which in turn means that
%   we can read a bundle file a line at a time without worrying about
%   end of file in the middle of a line.  Char is the next character to
%   be copied.

copy_to_bundle(26, yes) :- !.		% 26 => -1 in new standard
copy_to_bundle(26, no) :- !,
	nl.
copy_to_bundle(31, _) :- !,		% NB: we don't copy a 31 or 10
	nl, get0(C),			% literally, this is to convert
	copy_to_bundle(C, yes).		% between line terminators if
copy_to_bundle(10, _) :- !,		% that is necessary.  You ought
	nl, get0(C),			% to be able to run this in any
	copy_to_bundle(C, yes).		% current version of Dec-10 or
copy_to_bundle(D, Flag) :-		% C Prolog WITH NO CHANGES.
	put(D), get0(C),
	copy_to_bundle(C, Flag).



/*  unbundle is a wee bit tricky.
    I'm afraid I do most of my character I/O in Prolog thinking in terms
    of Finite-State-Automata rather than in terms of logic.  If files
    were lists I could use DCGs...
    The FSM I have in mind here works on a stream of lines, and has 3
    states.
	state 1: end of file => enter state 3
		 input is %?ENDOF ... => error message, return to state 1
		 input is %?BEGIN ... => open file, enter state 2
	         input is something else => forget it and return to state 1
	state 2: end of file => close output, print error message, state 3
	  	 input is %?ENDOF ... => close output, enter state 1
		 input is %?BEGIN ... => close output, print error message,
				  	return to new state 2
		 input is something else => print it, return to state 2
	state 3: halt
    At least, that's what I had in mind when I started.  It dawned on me
    when I was half way through that Prologs without TRO and GC would have
    a hard time with N,000 characters on the stack, so I had to hack it
    into 'repeat' (ugh, chunder) form.  TRO and GC are VITAL for practical
    programming, VITAL!  Sorry about the mess, before I hacked all the
    repeats in here I thought I understood it, now I know I don't.  But it
    seems to work.
*/

unbundle File :-
	see(File),
	repeat,
	    get_one_line(Line),
	    unbundle1(Line),		% succeeds at end of file
	!,
	seen.	


unbundle1(end_of_file) :- !.
unbundle1(Line) :-
	append("%?BEGIN ", Rest, Line),
	!,
	unbundle2(Rest, Last),
	Last = end_of_file.
unbundle1(Line) :-
	append("%?ENDOF ", Rest, Line),
	write('? Unexpected '), write_one_line(Line),
	!, fail.


unbundle2(Rest, Last) :-
	nofileerrors,
	append(LabelChars, [0' ,0'-,0'-,0' |_], Rest),
	name(Label, LabelChars), 
	atom(Label),
	tell(Label),
	fileerrors,
	!,
	repeat,
	    get_one_line(Line),
	    unbundle3(Line, Last),
	!.
unbundle2(Rest, Rest) :-
	fileerrors.


unbundle3(end_of_file, end_of_file) :- !,
	told,
	write('? unexpected end of file'), nl.
unbundle3(Line, Last) :-
	append("%?BEGIN ", Rest, Line),
	!,
	told,
	write('? Unexpected '), write_one_line(Line),
	unbundle2(Rest, Last).
unbundle3(Line, Line) :-
	append("%?ENDOF ", _, Line),
	told,
	!.
unbundle3(Line, _) :-
	write_one_line(Line),
	fail.


get_one_line(Line) :-
	get0(C),
	(   C = 26, Line = end_of_file
	;   get_one_line(C, Line)
	),  !.

get_one_line(31, []) :- !.
get_one_line(10, []) :- !.
get_one_line(C, [C|Cs]) :-
	get0(D),
	get_one_line(D, Cs).


write_one_line([]) :-
	nl.
write_one_line([C|Cs]) :-
	put(C),
	write_one_line(Cs).

