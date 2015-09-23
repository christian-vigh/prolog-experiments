%   File   : RDTOK.PL
%   Author : R.A.O'Keefe
%   Updated: 18 November 1982
%   Purpose: Read tokens up to the next "." <layout>

/*  This file is an interface to the Prolog system routine $read_tokens.
    For some reason which I agreed to at the time but now forget, the
    tokeniser is only accessible from compiled code.  Now that the plsys(_)
    mask exists for nasty internal things, it could really go there.
    However, the structures returned by the tokeniser are unpleasant enough
    that an extra interface is worth having.

	read_tokens(TokenList, VarList)

    returns a list of Tokens and a list of Variables, where the elements
    of the VarList have the form Atom=Variable.  For example, reading
    the term above might result in ['TokenList'=_1, 'VarList'=_2].  The
    elements of the TokenList take the form
	var(Var,Name)	-- the name is retained for error messages
	integer(Int)	-- 18-bit integers or xwd(_,_) pairs
	atom(Name)	-- most atoms
	string(ChList)	-- for string constants "..."
	Punct		-- for special punctuation ( ) [ ] { } , | 
*/

:- public
	read_tokens/2.

:-  mode
%	clean_neg(+, +, -),
	clean_pos(+, +, -),
	clean_tok_list(+, -),
	clean_var_list(+, -),
	read_tokens(-, -),
	sixbit_to_ascii(+, -).


read_tokens(Toks, Vars) :-
	'$read_tokens'(RawToks, RawVars),
	clean_var_list(RawVars, Vars), !,
	clean_tok_list(RawToks, Toks).

clean_var_list([var(Name,Var,_)|Rest], [Atom=Var|More]) :-
	sixbit_to_ascii(Name, VarName),
	name(Atom, VarName), !,
	clean_var_list(Rest, More).
clean_var_list([], []).

sixbit_to_ascii([Three|Rest], Ascii) :-
	A is (Three>>12)/\63,
	B is (Three>> 6)/\63,
	C is (Three    )/\63,
	one_sixbit_char(A, Ascii, A1),
	one_sixbit_char(B, A1, A2),
	one_sixbit_char(C, A2, More), !,
	sixbit_to_ascii(Rest, More).
sixbit_to_ascii([], []).

one_sixbit_char(0, X, X) :- !.
one_sixbit_char(A, [H|T], T) :- A < 27, !, H is 96+A.
one_sixbit_char(63,[95|T], T) :- !.
one_sixbit_char(Z, [H|T], T) :- Z > 36, !, H is 28+Z.
one_sixbit_char(D, [H|T], T) :- H is 21+D.

%clean_tok_list([atom(-),xwd(Hi,Lo)|Rest], [integer(Int)|More]) :-
%	clean_neg(Hi, Lo, Int), !,
%	clean_tok_list(Rest, More).
clean_tok_list([xwd(Hi,Lo)|Rest], [integer(Int)|More]) :-
	clean_pos(Hi, Lo, Int), !,
	clean_tok_list(Rest, More).
clean_tok_list([var(Var,Name)|Rest], [var(Var,Atom)|More]) :-
	sixbit_to_ascii(Name, VarName),
	name(Atom, VarName), !,
	clean_tok_list(Rest, More).
clean_tok_list([Other|Rest], [Other|More]) :- !,
	clean_tok_list(Rest, More).
clean_tok_list([], []).

%   reading a negative number is complicated by the fact that the token
%   reader will accept 36-bit numbers.  Unfortunately, the interpreter
%   will not (X is xwd(_,_) truncates).  On the whole, the simplest
%   thing to do seems to be to represent the result as an integer where
%   we can, otherwise to represent it as xwd(-,-) or -xwd(-,-).  Then at
%   least eval(-,-) in LONG will do the right thing.

clean_pos(0,  N, N) :- !(N) =< 8'377777, !.
clean_pos(-1, N, N) :- !(N) >  8'377777, !.
clean_pos(Hi,Lo, xwd(Hi,Lo)).

%clean_neg(0,  N, M) :- !(N) =< 8'377777+1, !, M is -N.
%clean_neg(-1, N, M) :- !(N) >= 8'377777,   !, M is -N.
%clean_neg(Hi,Lo, -xwd(Hi,Lo)).
