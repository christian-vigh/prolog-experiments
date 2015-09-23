/* XRF.PL :  Prolog Cross-Reference Program 

						Dave Bowen
						Updated: 1 June 1983
	This is the main module of XREF.

*/

% Compiler declarations %

 :- public go/0.			% User entry.
 :- public xrf/1, indirect/1.		% These are public for internal Call.

 :- op(1050,xfy,->).
 :- op(500,fy,@).

 :- mode caller(+,+,+,+).
 :- mode crecord(+,+).
 :- mode definition(+,+).
 :- mode do_file(+,+).
 :- mode exhaust(+). 
 :- mode getfrom(+).
 :- mode goal(?,-). 
 :- mode goal0(+,-).
 :- mode has_ext(+,+).
 :- mode head(+,+,-,-).  
 :- mode indirect(+).    
 :- mode mark_interpreted(+).
 :- mode process(+,+).   
 :- mode reply(+).  
 :- mode widen(?,-).
 :- mode xrf(+).

/******************************************************************************

Data

   $caller(Called_Predicate, Calling_Functor, Calling_Arity, Where_Defined)
			  Held on key: Called_Predicate.
			  $caller(P,F,N,I) means P is called by F/N in file I.
   $defn(File, Predicate)
			  Held on key: Predicate.
			  $defn(I,P) means P is defined in I.
   $file(File)
			  Held on key: $file(_).
			  $file(F) means F is a file.
   Predicate
			  Held on key: $pred
			  pred(G,M) means G/M was defined OR used.

******************************************************************************/

				/* Top level */
go :-   'LC',
	repeat,
	    ttynl, display('Next file: '), ttyflush,
	    readtonl(String),
	    reply(String), !.

				/* Check for termination of input: just 
				   <newline> typed? */
reply([]) :- !,
	(collect ; true).	% Yes: go and start output phase
reply(String) :-
	getfrom(String), !,	% Something input: what is it?
	fail.			% Go back to repeat

				/* Prolog, indirect or definition file? */
getfrom([64|S]) :- !,
	do_file(indirect,S).		% Indirect file introduced by '@'
getfrom([42|S]) :- !,
	do_file(load_file,S).		% Definition file introduced by '*'
getfrom(S) :- 
	has_ext(S,".CCL"), !,		% Indirect file indicated by ".CCL"?
	do_file(indirect,S).
getfrom(S) :- 
	has_ext(S,".DEF"), !,		% Definition file indicated by ".DEF"?
	do_file(load_file,S).
getfrom(S) :-				% Must be Prolog file for cross ref
	do_file(xrf,S).

				/* Takes two character lists as arguments, and
				   tests whether the 1st ends with the 2nd */
has_ext(Ext,Ext) :- !.
has_ext([_|L],Ext) :- has_ext(L,Ext).


				/* Open up the file, call processing procedure,
				   and close file again. Calls indirect(File)
				   load_file(File) or xrf(File). */
do_file(Predicate,String) :-
	name(File,String),		% Convert char list to atom (File)
	seeing(Old_file),		% Save currently open file
	see_chek(File),			% Open required file (may fail)
	P =.. [Predicate,File],		% Construct call to reqd procedure
	call(P),			% Call it
	seen,				% Close file
	see(Old_file).			% Re-open original file
do_file(_,_).				% Always succeed

				/* Get file names from indirect file */
indirect(F) :- 
	readtonl(S),			% Read a line, fail at end_of_file(^Z)
	( (S="")			% Ignore blank lines
	;   display('File: '),		% (S is char list)
	    writes(S), ttynl,		% Echo on terminal
	    getfrom(S)			% Process (may be indirect or def)
	), !,
	indirect(F).			% Loop to get next line
indirect(F) :-
	ttynl,				% Tell user when indirect file finished
	display('Indirect file '),
	display(F),
	display(' processed'), ttynl.

				/* Cross reference processing for a particular
				   file F */
xrf(F) :-
	recordz('$file'(_),'$file'(F),_),	% record F under $file
	exhaust(F).				% go through F term by term

				/* Process each clause, T, in file F */
exhaust(F) :- 
	repeat,				% Iterate till end_of_file
	    read(T),			% Read a clause
	    expand_term(T,T1),		% Pre-translation of grammar rules
	    process(T1,F),		% Cross-ref processing
	T=(end_of_file), !.

				/* Process clause (1st arg). 2nd arg is file */

process((P:-Q),I) :- !, 		% Non-unit clause
	head(P,I,F,N), !,
	goal(Q,G),			% Process successive goals by 
	caller(G,F,N,I).		%   backtracking.
process((:-G),_) :- !, call(G).		% goal clause, call it.
process((?-G),_) :- !, call(G).		% question, ditto
process(end_of_file,_) :- !.		% eof so succeed and exit repeat loop
process(P,I) :- head(P,I,_,_).		% unit clause

				/* Record the fact that P is a predicate & that
				   it is defined in file I. Return principal
				   functor of P (F) & its arity (N). */
head(P,I,F,N) :-
	functor(P,F,N),			% P has name F and arity N
	functor(G,F,N),			% G is most general term F/N
	definition(G,I).		% Record F/N is pred & defined in I

				/* Fail if goal is a variable. */
goal(G,_) :- var(G), !, fail.
goal(G,G1) :- goal0(G,G1).

				/* Returns most general term having the
				   principal functor & arity of each goal in 
				   the clause (successively on backtracking).
				   Ignores system predicates. */
goal0((G,_),G1) :-
   goal(G,G1).
goal0((_,G),G1) :- !,
   goal(G,G1).
goal0((G;_),G1) :-
   goal(G,G1).
goal0((_;G),G1) :- !,
   goal(G,G1).
goal0(G1,G2) :- 
   recorded(G1,'$applies'(G1,P),_),
   widen(P,P1),
   goal(P1,G2),
   mark_interpreted(G2).
goal0(G,_) :- 
   recorded(G,'$system',_), !,
   fail.
goal0(G,G).

				/* Record that P is a predicate and that it is
				   defined in file I */
definition(P,I) :-
	recorded(P,'$system',_), !,	% But not if P is a system predicate
	warn(P,'already defined as a system predicate'), fail.
definition(P,I) :-
	crecord('$pred',P),
	crecord(P,'$defn'(I,P)).

				/* Record that P is a predicate called by
				   F/N in file I */
caller(P,F,N,I) :- 
	functor(P,Pf,Pn),
	functor(P1,Pf,Pn),
	crecord('$pred',P1),
	crecord(P1,'$caller'(P1,F,N,I)).

				/* Record that P is called by the user or
				   outside its file of definition, and hence
				   must be public. */
mark_interpreted(P) :- 
	caller(P,'<user>',0,undefined).

				/* Record term Q on key P unless already
				   recorded. */
crecord(P,Q) :- recorded(P,Q,_), !.
crecord(P,Q) :- recordz(P,Q,_), !.


				/* Increase arity of predicate by specified
				   amount */
widen(A+_,_) :-
	var(A), !, fail.	% NB also covers variable as first arg
widen(A+Offset,A1) :- !,
	functor(A,F,N1),
	N2 is N1+Offset,
	functor(A1,F,N2).
widen(A,A).
