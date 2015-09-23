From @USC-ISID,@Ucl-Cs:okeefe.r.a.@Ercc.AC.UK Sun Sep  4 19:55:10 1983
Date:       Monday, 5-Sep-83  01:39:43-BST
From: RICHARD HPS (on ERCC DEC-10) <okeefe.r.a.@edxa>
Subject:    Prolog parser
Message-Id: <8308050054.26023@ANL-MCS.ARPA> 
Received: FROM UCL-CS BY USC-ISID.ARPA WITH TCP ; 4 Sep 83 17:53:08 PDT
Received: from USC-ISID.ARPA 
Via:  Ercc.AC.UK; to 44b.Ucl-Cs.AC.UK  over Sercnet with NIFTP; 
	  5 Sep 83 1:42 BST
To: overbeek <overbeek%anl-mcs.arpa@ucl-cs>
Reply-To:   okeefe.r.a. <okeefe.r.a.%edxa@ucl-cs>
Status: R

--------
     If you are getting into Prolog implementation, I am surprised
that you do not subscribe to the Arpanet mailing lists
Prolog-Request and Prolog-Hackers (mail to Prolog@Su-Score to
join), or to the UseNet newgroup net.lang.prolog .  There is a
fair bit of Prolog utility code which is available via FTP in the
directory <PROLOG> at Su-Score (I think Anonymous/Guest works),
and which was posted to net.sources over UseNet.  Fernando Pereira
(Pereira@SRI-AI) collected this stuff (mostly from Edinburgh) and
made it generally available.  It includes a Prolog parser.  Just
to be helpful, I'm including the parser in this message.

     Essentially, the Prolog parser is just the good old read/1
predicate.  What the compiler actually uses differs in two ways;
it gets a list of variables (with some extra information about
number of occurrences which this version of read/2 suppresses),
and it constructs terms as e.g.  f![g![var(X)],h![var(X)]] instead
of f(g(X),h(X)).  The reason for that is that the current compiler
is based on the old out-of-line compiler, which didn't have much
space (we then had to run jobs overnight if we wanted more than
50k) so didn't want to create functor blocks.  It is wildly
irrelevant nowadays.

     If you haven't already got a copy, you might find the
"Proceedings LOGIC PROGRAMMING WORKSHOP '83" interesting.  To
quote from the Foreword: "To obtain a copy of the proceedings
delivered by air mail send 1700 Escudos or equivalent, by personal
check[sic] or otherwise, to
	Logic Programming Workshop '83
	Nucleo de Inteligencia Artificial
	Universidade Nova de Lisboa
	2825 Quinta da Torre
	Portugal
".  There are two papers about Prolog compilers, a paper about the
Japanese PSI machine, a paper about the University of Maryland's
PRISM and some other interesting things.  1700 Escudos is only
about 11 pounds, I think.

     Are you going to produce a Prolog-like system, or are you
going for a REAL logic programming language?  I would very much
like to see an efficient implementation of Seif Haridi's language,
which is based on natural deduction.  If you give it Horn clauses,
it is almost identical to Prolog, but you can use all the logical
connectives (including negation, via X=>false) and both quantifiers.

     Anyway, here is the parser.  It relies on a tokeniser, which
returns a list of all the tokens up to the next ".<layout>".  That
tokeniser is buried deep inside the Dec-10 system, and was brought
out in a hacky sort of way at my request (I wanted to produce a
frame-based system, and wanted quite a different syntax from
Prolog.  The frame parser worked, but the rest of the system never
did) and is ONLY available from COMPILED code.  There are two
files: RDTOK.PL is an interface to the Dec-10 tokeniser, READ.PL
is the parser proper.  current_op(Priority,Type,Atom) is a Dec-10
primitive.

     Re your closing salutation: what are the good tidings?

{{{{{{{{RDTOK.PL
%   File: RdTok		Author: R.A.O'Keefe	    Updated: 18 November 82

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
}}}}}}}}RDTOK.PL
{{{{{{{{READ.PL
/*  File: Read.Ppl	Author: D.H.D.W.	Updated: 2 August 1983

    Modified by Alan Mycroft to regularise the functor modes.
    This is both easier to understand (there are no more '?'s),
    and also fixes bugs concerning the curious interaction of cut with
    the state of parameter instantiation.

    I don't cite myself as author of this file yet because I don't
    understand it yet.  When I do understand it I will reorganise it and
    rename things and maybe add things, and I shall then put my name on
    it.  (I=R.A.O'Keefe)

    Since this file doesn't provide "metaread", it is considerably
    simplified.  The token list format has been changed somewhat, see
    the comments in the RDTOK file.

    I have added the rule X(...) -> apply(X,[...]) for Alan Mycroft.

*/

:- public
	read/2.

:- mode
	after_prefix_op(+, +, +, +, +, -, -),
	ambigop(+, -, -, -, -, -),
	cant_follow_expr(+, -),
	expect(+, +, -),
	exprtl(+, +, +, +, -, -),
	exprtl0(+, +, +, -, -),
	infixop(+, -, -, -),
	postfixop(+, -, -),
	prefixop(+, -, -),
	prefix_is_atom(+, +),
	read(?, ?),
	read(+, +, -, -),
	read(+, +, +, -, -),
	read_args(+, -, -),
	read_list(+, -, -),
	syntax_error(+),
	syntax_error(+, +).


%   read(?Answer, ?Variables)
%   reads a term from the current input stream and unifies it with
%   Answer.  Variables is bound to a list of [Atom=Variable] pairs.

read(Answer, Variables) :-
	repeat,
	    read_tokens(Tokens, Variables),
	    (   read(Tokens, 1200, Term, Leftover), all_read(Leftover)
	    |   syntax_error(Tokens)
	    ),
	!,
	Answer = Term.


%   all_read(+Tokens)
%   checks that there are no unparsed tokens left over.

all_read([]) :- !.
all_read(S) :-
	syntax_error([operator,expected,after,expression], S).


%   expect(Token, TokensIn, TokensOut)
%   reads the next token, checking that it is the one expected, and
%   giving an error message if it is not.  It is used to look for
%   right brackets of various sorts, as they're all we can be sure of.

expect(Token, [Token|Rest], Rest) :- !.
expect(Token, S0, _) :-
	syntax_error([Token,or,operator,expected], S0).


%   I want to experiment with having the operator information held as
%   ordinary Prolog facts.  For the moment the following predicates
%   remain as interfaces to current_op.
%   prefixop(O -> Self, Rarg)
%   postfixop(O -> Larg, Self)
%   infixop(O -> Larg, Self, Rarg)


prefixop(Op, Prec, Prec) :-
	current_op(Prec, fy, Op), !.
prefixop(Op, Prec, Less) :-
	current_op(Prec, fx, Op), !,
	Less is Prec-1.


postfixop(Op, Prec, Prec) :-
	current_op(Prec, yf, Op), !.
postfixop(Op, Less, Prec) :-
	current_op(Prec, xf, Op), !, Less is Prec-1.


infixop(Op, Less, Prec, Less) :-
	current_op(Prec, xfx, Op), !, Less is Prec-1.
infixop(Op, Less, Prec, Prec) :-
	current_op(Prec, xfy, Op), !, Less is Prec-1.
infixop(Op, Prec, Prec, Less) :-
	current_op(Prec, yfx, Op), !, Less is Prec-1.


ambigop(F, L1, O1, R1, L2, O2) :-
	postfixop(F, L2, O2),
	infixop(F, L1, O1, R1), !.


%   read(+TokenList, +Precedence, -Term, -LeftOver)
%   parses a Token List in a context of given Precedence,
%   returning a Term and the unread Left Over tokens.

read([Token|RestTokens], Precedence, Term, LeftOver) :-
	read(Token, RestTokens, Precedence, Term, LeftOver).
read([], _, _, _) :-
	syntax_error([expression,expected], []).


%   read(+Token, +RestTokens, +Precedence, -Term, -LeftOver)

read(var(Variable,_), ['('|S1], Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_args(S2, RestArgs, S3), !,
	exprtl0(S3, apply(Variable,[Arg1|RestArgs]), Precedence, Answer, S).

read(var(Variable,_), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Variable, Precedence, Answer, S).

read(atom(-), [integer(Integer)|S1], Precedence, Answer, S) :-
	Negative is -Integer, !,
	exprtl0(S1, Negative, Precedence, Answer, S).

read(atom(Functor), ['('|S1], Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_args(S2, RestArgs, S3),
	Term =.. [Functor,Arg1|RestArgs], !,
	exprtl0(S3, Term, Precedence, Answer, S).

read(atom(Functor), S0, Precedence, Answer, S) :-
	prefixop(Functor, Prec, Right), !,
	after_prefix_op(Functor, Prec, Right, S0, Precedence, Answer, S).

read(atom(Atom), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Atom, Precedence, Answer, S).

read(integer(Integer), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Integer, Precedence, Answer, S).

read('[', [']'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, [], Precedence, Answer, S).

read('[', S1, Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_list(S2, RestArgs, S3), !,
	exprtl0(S3, [Arg1|RestArgs], Precedence, Answer, S).

read('(', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).

read(' (', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).

read('{', ['}'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, '{}', Precedence, Answer, S).

read('{', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect('}', S2, S3), !,
	exprtl0(S3, '{}'(Term), Precedence, Answer, S).

read(string(List), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, List, Precedence, Answer, S).

read(Token, S0, _, _, _) :-
	syntax_error([Token,cannot,start,an,expression], S0).


%   read_args(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ')' and returns a list of terms.

read_args([','|S1], [Term|Rest], S) :- !,
	read(S1, 999, Term, S2), !,
	read_args(S2, Rest, S).
read_args([')'|S], [], S) :- !.
read_args(S, _, _) :-
	syntax_error([', or )',expected,in,arguments], S).


%   read_list(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ['|' expr(999)] ']' and returns a list of terms.

read_list([','|S1], [Term|Rest], S) :- !,
	read(S1, 999, Term, S2), !,
	read_list(S2, Rest, S).
read_list(['|'|S1], Rest, S) :- !,
	read(S1, 999, Rest, S2), !,
	expect(']', S2, S).
read_list([']'|S], [], S) :- !.
read_list(S, _, _) :-
	syntax_error([', | or ]',expected,in,list], S).


%   after_prefix_op(+Op, +Prec, +ArgPrec, +Rest, +Precedence, -Ans, -LeftOver)

after_prefix_op(Op, Oprec, Aprec, S0, Precedence, _, _) :-
	Precedence < Oprec, !,
	syntax_error([prefix,operator,Op,in,context,
		with,precedence,Precedence], S0).

after_prefix_op(Op, Oprec, Aprec, S0, Precedence, Answer, S) :-
	peepop(S0, S1),
	prefix_is_atom(S1, Oprec), % can't cut but would like to
	exprtl(S1, Oprec, Op, Precedence, Answer, S).

after_prefix_op(Op, Oprec, Aprec, S1, Precedence, Answer, S) :-
	read(S1, Aprec, Arg, S2),
	Term =.. [Op,Arg], !,
	exprtl(S2, Oprec, Term, Precedence, Answer, S).


%   The next clause fixes a bug concerning "mop dop(1,2)" where
%   mop is monadic and dop dyadic with higher Prolog priority.

peepop([atom(F),'('|S1], [atom(F),'('|S1]) :- !.
peepop([atom(F)|S1], [infixop(F,L,P,R)|S1]) :- infixop(F, L, P, R).
peepop([atom(F)|S1], [postfixop(F,L,P)|S1]) :- postfixop(F, L, P).
peepop(S0, S0).


%   prefix_is_atom(+TokenList, +Precedence)
%   is true when the right context TokenList of a prefix operator
%   of result precedence Precedence forces it to be treated as an
%   atom, e.g. (- = X), p(-), [+], and so on.

prefix_is_atom([Token|_], Precedence) :-
	prefix_is_atom(Token, Precedence).

prefix_is_atom(infixop(_,L,_,_), P) :- L >= P.
prefix_is_atom(postfixop(_,L,_), P) :- L >= P.
prefix_is_atom(')', _).
prefix_is_atom(']', _).
prefix_is_atom('}', _).
prefix_is_atom('|', P) :- 1100 >= P.
prefix_is_atom(',', P) :- 1000 >= P.
prefix_is_atom([],  _).


%   exprtl0(+Tokens, +Term, +Prec, -Answer, -LeftOver)
%   is called by read/4 after it has read a primary (the Term).
%   It checks for following postfix or infix operators.

exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	ambigop(F, L1, O1, R1, L2, O2), !,
	(   exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S)
	|   exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S)
	).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	infixop(F, L1, O1, R1), !,
	exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	postfixop(F, L2, O2), !,
	exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S).

exprtl0([','|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1000, !,
	read(S1, 1000, Next, S2), !,
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).

exprtl0(['|'|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1100, !,
	read(S1, 1100, Next, S2), !,
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).

exprtl0([Thing|S1], _, _, _, _) :-
	cant_follow_expr(Thing, Culprit), !,
	syntax_error([Culprit,follows,expression], [Thing|S1]).

exprtl0(S, Term, _, Term, S).


cant_follow_expr(atom(_),	atom).
cant_follow_expr(var(_,_),	variable).
cant_follow_expr(integer(_),	integer).
cant_follow_expr(string(_),	string).
cant_follow_expr(' (',		bracket).
cant_follow_expr('(',		bracket).
cant_follow_expr('[',		bracket).
cant_follow_expr('{',		bracket).



exprtl([infixop(F,L,O,R)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L, !,
	read(S1, R, Other, S2),
	Expr =.. [F,Term,Other], /*!,*/
	exprtl(S2, O, Expr, Precedence, Answer, S).

exprtl([postfixop(F,L,O)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L, !,
	Expr =.. [F,Term],
	peepop(S1, S2),
	exprtl(S2, O, Expr, Precedence, Answer, S).

exprtl([','|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1000, C < 1000, !,
	read(S1, 1000, Next, S2), /*!,*/
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).

exprtl(['|'|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1100, C < 1100, !,
	read(S1, 1100, Next, S2), /*!,*/
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).

exprtl(S, _, Term, _, Term, S).


%   This business of syntax errors is tricky.  When an error is detected,
%   we have to write out a message.  We also have to note how far it was
%   to the end of the input, and for this we are obliged to use the data-
%   base.  Then we fail all the way back to read(), and that prints the
%   input list with a marker where the error was noticed.  If subgoal_of
%   were available in compiled code we could use that to find the input
%   list without hacking the data base.  The really hairy thing is that
%   the original code noted a possible error and backtracked on, so that
%   what looked at first sight like an error sometimes turned out to be
%   a wrong decision by the parser.  This version of the parser makes
%   fewer wrong decisions, and my goal was to get it to do no backtracking
%   at all.  This goal has not yet been met, and it will still occasionally
%   report an error message and then decide that it is happy with the input
%   after all.  Sorry about that.  (2 Aug) Spurious error messages @i<seem>
%   to be a thing of the past.  However, you may get more than one message
%   for the same unacceptable input.

syntax_error(Message, List) :-
	ttynl, display('**'),
	display_list(Message),
	length(List, Length),
	recorda(syntax_error, length(Length), _), !,
	fail.

display_list([Head|Tail]) :-
	ttyput(32),
	display_token(Head), !,
	display_list(Tail).
display_list([]) :-
	ttynl.

syntax_error(List) :-
	recorded(syntax_error, length(AfterError), Ref),
	erase(Ref),
	length(List, Length),
	BeforeError is Length-AfterError,
	display_list(List, BeforeError), !,
	fail.

display_list(X, 0) :-
	display('<<here>> '), !,
	display_list(X, 99999).
display_list([Head|Tail], BeforeError) :-
	display_token(Head),
	ttyput(32),
	Left is BeforeError-1, !,
	display_list(Tail, Left).
display_list([], _) :-
	ttynl.

display_token(atom(X))	  :- !,	display(X).
display_token(var(V,X))	  :- !,	display(X).
display_token(integer(X)) :- !,	display(X).
display_token(string(X))  :- !,	display(X).
display_token(X)	  :-	display(X).

}}}}}}}}READ.PL

--------


