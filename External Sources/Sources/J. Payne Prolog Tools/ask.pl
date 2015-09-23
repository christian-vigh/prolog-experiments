%   File   : ASK.PL
%   Author : R.A.O'Keefe
%   Updated: 25 November 1983
%   Purpose: ask questions that have a one-character answer.
%   Hacked for NIP: Ken Johnson 25-4-87
%   Conversion note: end of line is 10 in NIP

% Example: 		?- ask('cancel',A).
% Prolog types		cancel?
% You type		y
% Prolog instantiates A to Ascii code for y
% The instantiation is the same even if there are characters after your 'y'
% e.g. if you answer 'yes' or 'yggdrasil'.

%			?- ask( 'cancel', 'n', A).
% is like the above except that
% (a) Prolog types	cancel [n] ?
% and (b) if the user replies with a carriage return only
% then Prolog instantiates A to n (the default reply specified)

%			?- yesno('cancel').
% prints		cancel?
% insists on an answer beginning y or n
% succeeds if the answer began with y, fails if it began with n

%   The method of redirecting input-output in Dec-10 Prolog, C-Prolog,
%   PDP-11 Prolog, and PopLog is rather clumsy.  The following
%   version of "call" manages to ensure that input and output are
%   redirected to 'user' while Goal is running, and restored to their
%   original values while it is not, even if Goal should backtrack or
%   fail.  This is no mean achievement, I can tell you.  To avoid the
%   need for this nonsense, Dec-10 Prolog has a number of commands
%   ttyX that do X to 'user'.  There isn't any ttywrite, but display
%   comes close.  However, this file has to run under C-Prolog as well,
%   where display writes on the current output stream, and the ttyX
%   predicates are not primitives, but have to do their own switching.


talk_to_user_while(Goal) :-
	seeing(Seeing),
	telling(Telling),
	(   see(user), tell(user)		%  CALL port
	;   see(Seeing), tell(Telling), fail	%  FAIL port
	),
	call(Goal),
	(   see(Seeing), tell(Telling)		%  EXIT port
	;   see(user), tell(user), fail		%  REDO port
	).

%   ask_default_character(Spec, Char)
%   lets the programmer specify the default character in whatever way
%   s/he finds convenient, either as an integer, as a string, or as a
%   Prolog atom.  The case of the character is preserved.

ask_default_character(Spec, Spec) :-
	integer(Spec), !.
ask_default_character([Spec|_], Spec) :- !.
ask_default_character(Spec, Char) :-
	atom(Spec),
	name(Spec, [Char|_]).

%   ask(Question, Answer)
%   displays the Question on the  terminal  and  reads  a  one-character
%   answer  from the terminal.  But because you normally have to type "X
%   <CR>" to get the computer to attend to you, it skips to the  end  of
%   the  line.   All the juggling with see and tell is to make sure that
%   i/o is done to the terminal even if your program is doing  something
%   else.   The  character  returned  will  have Ascii code in the range
%   33..126 (that is, it won't be a space or a control character).

ask(Question, Answer) :-
	talk_to_user_while(ask__1(Question, Answer)).


ask__1(Question, Answer) :-
	write(Question),
	write('? '),
	ttyflush,
	get0(Char),
	ask__1(Char, Question, Answer).
	

ask__1(Char, _, Answer) :-
	Char > 32, Char < 127, !,
	skip(10),
	Answer = Char.
ask__1(10, Question, Answer) :- !,
	ask__1(Question, Answer).
ask__1(_, Question, Answer) :-
	skip(10),
	ask__1(Question, Answer).



%   ask(Question, Default, Answer)
%   is like ask(Question, Answer) except thast if the user types a newline
%   the Default will be taken as the Answer.

ask(Question, Default, Answer) :-
	ask_default_character(Default, DefChar),
	talk_to_user_while(ask__2(Question, DefChar, Answer)).


ask__2(Question, Default, Answer) :-
	write(Question),
	write(' ['),
	put(Default),
	write(']? '),
	ttyflush,
	get0(Char),
	ask__2(Char, Question, Default, Answer).
	

ask__2(Char, _, _, Answer) :-
	Char > 32, Char < 127, !,
	skip(10),
	Answer = Char.
ask__2(10, _, Default, Answer) :- !,
	Answer = Default.
ask__2(_, Question, Default, Answer) :-
	skip(10),
	ask__2(Question, Default, Answer).


%   yesno(Question)
%   asks the question, and succeeds if the answer is y or Y, fails if
%   the answer is n or N, and repeats the question if it is anything else.

yesno(Question) :-
	talk_to_user_while(yesno__1(Question)).


yesno__1(Question) :-
	write(Question),
	write('? '),
	ttyflush,
	get0(Char),
	Answer is Char\/32,	%   force lower case
	(   Char = 10		%   end of line
	;   skip(10)		%   skip if it isn't
	),  !,
	yesno__1(Answer, Question).

yesno__1(121/*y*/, _) :- !.
yesno__1(110/*n*/, _) :- !, fail.
yesno__1(_, Question) :-
	write('Please answer Yes or No.'), nl,
	yesno__1(Question).



%   yesno(Question, Default)
%   is like yesno(Question), except that if the user types a newline
%   without a Y or N the default will be used.  It should of course
%   be y or n itself.

yesno(Question, Default) :-
	ask_default_character(Default, DefChar),
	talk_to_user_while(yesno__2(Question, DefChar)).

yesno__2(Question, Default) :-
	write(Question),
	write(' ['),
	put(Default),
	write(']? '),
	ttyflush,
	get0(Char),
	(   Char = 10, Answer = Default		% end of line
	;   skip(10), Answer is Char\/32	% skip to eol
	),  !,
	yesno__2(Answer, Question, Default).

yesno__2(121/*y*/, _, _) :- !.
yesno__2(110/*n*/, _, _) :- !, fail.
yesno__2(_, Question, Default) :-
	write('Please answer Yes or No.'), nl,
	yesno__2(Question, Default).


