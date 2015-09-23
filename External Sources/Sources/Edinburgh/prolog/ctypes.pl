%   File   : CTYPES.PL
%   Author : Richard A. O'Keefe
%   Updated: 9 September 1984
%   Purpose: Character classification

/*  The predicates to_lower, to_upper, is_alnum, is_alpha, is_cntrl,
    is_digit, is_graph, is_lower, is_upper, is_print, is_punct, and
    is_space are taken directly from the April 84 draft of the C
    standard.  The remaining ones are of my own invention, but are
    reasonably useful.  If you want to make your programs portable
    between different operating systems, use is_endline and is_endfile
    instead of the literal constants 31 and 26 or 10 and -1.
*/
:- public
	is_alnum/1,
	is_alpha/1,
	is_cntrl/1,
	is_digit/1,
	is_endfile/1,
	is_graph/1,
	is_lower/1,
	is_newline/1,
	is_newpage/1,
	is_paren/1,
	is_period/1,
	is_print/1,
	is_punct/1,
	is_quote/1,
	is_space/1,
	is_upper/1,
	to_lower/2,
	to_upper/2.

:- mode
	'_ul_hack'(+, +, +, +, ?).


is_alnum(C) :-
	is_alpha(C).
is_alnum(C) :-
	is_digit(C).


is_alpha(C) :-
	is_lower(C).
is_alpha(C) :-
	is_upper(C).
is_alpha(0'_).


is_cntrl(127).
is_cntrl(C) :-
	between(0, 31, C).


is_digit(C) :-
	between(0'0, 0'9, C).


is_endfile(26).			% is -1 in the draft standard


is_graph(C) :-
	between(33, 126, C).


is_lower(C) :-
	between(0'a, 0'z, C).


is_newline(31).			% may be 10 or 13 (O/S-dependent)


is_newpage(12).			% may fail (O/S-dependent)


is_paren(0'(, 0')).
is_paren(0'[, 0']).
is_paren(0'{, 0'}).
is_paren(0'<, 0'>).		% should this be in?


is_period(0'.).			% a period is anything that ends a sentence
is_period(0'?).			% (also known as a period).  . is a full stop.
is_period(0'!).


is_print(C) :-
	between(32, 126, C).


is_punct(C) :-			% between space and digits
	between(33, 47, C).
is_punct(C) :-			% between digits and uppers
	between(58, 64, C).
is_punct(C) :-			% between uppers and lowers
	between(91, 96, C), C \== 0'_.
is_punct(C) :-			% between lowers and delete
	between(123, 126, C).


is_quote(0'').
is_quote(0'").
is_quote(0'`).


is_space(32).			% ` `
is_space(31).			% `\n` in Dec-10 Prolog
is_space( 9).			% `\t`
is_space(10).			% -maybe- `\n`
is_space(11).			% `\v`
is_space(12).			% `\f`
is_space(13).			% `\r`


is_upper(C) :-
	between(0'A, 0'Z, C).


%   to_lower and to_upper are complicated by the fact that the
%   Dec-10 compiler doesn't handle if->then;else.  _ul_hack would not
%   be necessary otherwise.

to_lower(U, L) :-
	between(0, 127, U),
	'_ul_hack'(U, 0'A, 0'Z, 32, L).


to_upper(L, U) :-
	between(0, 127, L),
	'_ul_hack'(L, 0'a, 0'z, -32, U).


'_ul_hack'(X, A, Z, D, Y) :-
	A =< X, X =< Z, !,
	Y is X+D.
'_ul_hack'(X, _, _, _, Y).


