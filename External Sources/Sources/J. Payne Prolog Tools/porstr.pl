%   File   : PORSTR.PL
%   Author : R.A.O'Keefe
%   Updated: 30 August 1984
%   Purpose: Portray lists of characters as strings.


%   portray(S) will write a complete list of character codes as "text"
%   and a list of character codes with a variable or $VAR tail as "text|var".
%   This is potentially confusing, as "text|var" is a possible string, but
%   in the context of tracing/debugging it will generally be clear to the
%   human reader, for whom print/1 output is exclusively intended.  For one
%   reason and another, I consider it better for "" to be displayed as [],
%   so we need the clause for nil as well.

portray([]) :- !,
	write([]).
portray(String) :-
	is_a_string(String),
	!,
	put(34), portray_string(String), put(34).


is_a_string(Var) :-
	var(Var), !.
is_a_string([]) :- !.
is_a_string([Char|Chars]) :-
	integer(Char),
	( Char >= 31, Char < 127 ; Char = 9 ),
	!,
	is_a_string(Chars).
is_a_string('$VAR'(N)).


portray_string(Var) :-
	var(Var),
	!,
	put(124), write(Var).
portray_string([Char|Chars]) :- !,
	put(Char),
	portray_string(Chars).
portray_string([]) :- !.
portray_string('$VAR'(N)) :-
	put(124), write('$VAR'(N)).
