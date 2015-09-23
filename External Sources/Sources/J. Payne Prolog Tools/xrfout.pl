%   File   : XRFOUT.PL
%   Author : Dave Bowen
%   Updated: 22 August 1984
%   Purpose: Output module for XREF.

:- public			% for setof/3
	erase_file_record/1,
	erase_export_record/2,
	erase_import_record/2.
:- mode
	charlength(+, -),
	do_imports(+),
	do_imports(+, +),
	do_output(+),
	do_publics(+),
	do_publics(+, +),
	erase_file_record(-),
	erase_export_record(+, -),
	erase_import_record(+, -),
	export_records(+, -),
	f_back(+, +, -, +),
	f_front(+, +, +, +),
	f_output(+, +),
	f_write(+, +, -, +),
	f_writepred(+, +, +, -, +),
	import_records(+, -),
	makeroom(+, +, -, +),
	number_length(+, -),
	out_globals,
	writes(+),
	write_callers(+, +, -, +),
	write_centre(+, +).


				/* O/P cross-ref listing in reqd format.*/
do_output(Dbase) :-
	get_crf(File, Chars),		%  Get filename for cross-ref listing
	tell_chek(File), !,		%  Get another if can't 'tell' it
	get_title(Title),
	get_width(Width),
	write_centre('**********************************', Width), nl,
	write_centre('* PROLOG CROSS REFERENCE LISTING *', Width), nl,
	write_centre('**********************************', Width), nl,
	nl,
	write_centre(Title, Width), nl,
	nl, nl,
	write('PREDICATE               FILE          CALLED BY'), nl,
	nl, nl,
	f_output(Dbase, Width), !,	%  Output cross-reference list
	told,
	nl, write('Cross-reference listing written to '), write(File), nl,
	out_globals.			%  Output import/export lists if reqd


				/* formatted output of cross refs */
f_output(Dbase, Width) :-
	member(e(F,N,f(I,Cs)), Dbase),
	f_front(F, N, I, Width),
	f_back(Cs, 38, _, Width),
	fail.
f_output(_, _).

				/* Write predicate and file where defined */
f_front(F, N, I, W) :-
	nl, nl,
	f_writepred(F, N, 0, C1, W),
	pos(C1, 24),
	f_write(I, 24, C2, W),
	pos(C2, 38).

				/* Either write callers or 'NOT CALLED' */
f_back([], C1, C2, W) :-
	f_write('NOT CALLED', C1, C2, W).
f_back([H|T], C1, C2, W) :-
	sort([H|T], Sorted),
	write_callers(Sorted, C1, C2, W).

				/* List callers of the predicate */
write_callers([], C, C, _) :- !.
write_callers([c(F,N)|Zs], C1, C4, W) :-
	f_writepred(F, N, C1, C2, W),
	put(32),
	C3 is C2+1,
	write_callers(Zs, C3, C4, W).



				/* L is the length in chars of X */
charlength(X, L) :-
	atomic(X), !,			% Is X an atom or integer?
	name(X, Chars),
	length(Chars, L).
charlength(F/N, L) :-			% Is it of the form F/N
	atom(F), integer(N), !,		% with F/N a plausible functor?
	name(F, S1),	length(S1, L1),
	name(N, S2),	length(S2, L2),
	L is L1+L2+1.
charlength(X, L) :-			% Must be a list (string)
	length(X, L).			% is this used at all?
   
				/* Write predicate remembering format info */
f_writepred('<user>', 0, C1, C2, W) :- !,
	makeroom(6, C1, C2, W),
	write('<user>').
f_writepred(F, N, C1, C2, W) :-
	charlength(F/N, L),
	makeroom(L, C1, C2, W),
	write(F), put(47), write(N).

				/* Write atom ditto */
f_write(X, C1, C2, W) :-
	charlength(X, L),
	makeroom(L, C1, C2, W),
	write(X).

				/* Make sure there is room to write L */ 
makeroom(L, C1, C2, W) :-
	(   L+C1 < W, C2 is C1+L
	;   C1 =< 38, C2 is C1+L
	;   nl, tab(38), C2 is L+38
	),  !.


				/* Write X in the centre of current line */
write_centre(X, W) :-
	charlength(X, L),
	Space is (W-L)/2,
	tab(Space),
	writes(X).

				/* Move from column From to To */
pos(From, To) :-
	(   To > From, Space is To-From
	;   nl, Space = To
	),  !,
	tab(Space).

				/* Write out list of chars (string), or atom */
writes([]) :- !.
writes([C|Cs]) :- !,
	put(C),
	writes(Cs).
writes(X) :-
	write(X).


out_globals :-
	setof(File, erase_file_record(File), Files),
	(   get_globals_file(Globals),	% if we are to write a globals file
	    tell_chek(Globals),		% and we can open it
	    !,				% commit to that file
	    (   member(file(FileName,Exports,Imports), Files),
		write('%   FILE:  '), write(FileName), nl, nl,
		do_publics(Exports),	% write the exported predicates
		do_imports(Imports),	% and imported predicates
		nl, fail		% for each File we've read
	    ;   true			% this'd be "forall" but the Dec-10
	    ),				% compiler doesn't understand that
	    told,			% close the Globals file
	    write('Globals listing written to '), write(Globals), nl
	;   true			% do nothing if no file wanted
	),
	(   get_update_globals(yes),	% if we are to update the declarations
	    !,				% in each source file
	    (   member(file(FileName,Exports,Imports), Files),
		update_declarations(FileName, Exports, Imports),
		fail
	    ;   true
	    ),
	    write('Source files updated.'), nl
	;   true
	).


/*  erase_file_record(File)
    enumerates triples file(FileName,Exports,Imports) where FileName is the
    name of a file that XREF has looked at, Exports is a list of
    export(Functor,Arity) pairs naming predicates exported from that File,
    and imports is a list of import(Functor,Arity,FromFile) triples naming
    predicates imported from other files and indicating which.

    It has the side effect of erasing all this information from the data
    base, which only matters if you're using XREF inside something else.
*/
erase_file_record(file(FileName,Exports,Imports)) :-
	recorded('$file'(_), '$file'(FileName), Ref),
	erase(Ref),
	export_records(FileName, Exports),
	import_records(FileName, Imports).


export_records(FileName, Exports) :-
	setof(Export, erase_export_record(FileName,Export), Exports), !.
export_records(_, []).


erase_export_record(FileName, export(Symbol,Arity)) :-
	recorded(FileName, '$entry'(FileName, Pred), Ref),
	erase(Ref),
	functor(Pred, Symbol, Arity).


import_records(FileName, Imports) :-
	setof(Import, erase_import_record(FileName,Import), Imports), !.
import_records(_, []).


erase_import_record(FileName, import(Symbol,Arity,FromFile)) :-
	recorded(FileName, '$ext'(FileName, Pred), Ref),
	erase(Ref),
	functor(Pred, Symbol, Arity),
	defn_file(Pred, FromFile).


				/* Output public decls (exports) for a file */
do_publics([]) :- !.
do_publics(L) :-
	do_publics(L, ':- public').

do_publics([], L) :- !,	
	put(46), nl, nl.
do_publics([export(F,N)|L], Atom) :-
	write(Atom), nl,
	put(9), writeq(F/N),
	do_publics(L, (',')).

				/* Output import decls for a file */
do_imports([]) :- !.
do_imports(L) :-
	do_imports(L, '%- import').

do_imports([], _) :- !,
	put(46), nl, nl.
do_imports([import(F,N,I)|L], Atom) :-
	write(Atom), nl,
	put(37), put(9), writeq(F/N),
	charlength(F/N, W), Space is 32-W,
	tab(Space), write('from '), write(I),
	do_imports(L, (',')).

