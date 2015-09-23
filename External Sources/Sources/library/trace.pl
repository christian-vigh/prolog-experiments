/*
This is the routine described in Prolog Digest V1 #13:

Trace - July 8, 1983  written by Russ Abbott and Alan Foonberg

This is a trace program similar to that provided by the PROLOG system.  A
term's execution is traced step-by-step by typing "trace(term).", where term
is that term to be traced.  At each step of the trace, you have an option to
enter a different trace mode.  The list of modes can be obtained by answering
this question with a '?'.  A typical trace line looks like this:

	2.   (3)   enter    b(Var, constant)

The number before the period indicates the count, the number in parenthesis
is the depth, the following phrase indicates the port, and the rest is the
current term being traced.  To obtain the full trace as a parameter, type
"trace(term, Full_Trace)."  Full_Trace will contain a list of the trace lines,
which can be printed out formatted by typing
 "trace$_write_term(trace_list(Full_Trace)."  A 'true trace' is also provided
by adding a third parameter to the trace call, and this can be printed in a
manner similar to that used to print the full trace.  The true trace contains
only those calls that succeeded which led to the satsification of the main
goal.  If you have any questions or suggestions, please send network mail to 
foonberg@aerospace or abbott@aerospace.

(remember that dollar signs can be eliminated).

*/
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
:-(op(1100, xfy, [or])).
:-(op(1000, xfy, [and])).
 
function$is_built_in_with$numero$arguments(abolish, 2).
function$is_built_in_with$numero$arguments(abort, 0).
function$is_built_in_with$numero$arguments(acos, 1).
function$is_built_in_with$numero$arguments(arg, 3).
function$is_built_in_with$numero$arguments(asin, 1).
function$is_built_in_with$numero$arguments(assert, 1).
function$is_built_in_with$numero$arguments(assert, 2).
function$is_built_in_with$numero$arguments(asserta, 1).
function$is_built_in_with$numero$arguments(asserta, 2).
function$is_built_in_with$numero$arguments(assertz, 1).
function$is_built_in_with$numero$arguments(assertz, 2).
function$is_built_in_with$numero$arguments(atan, 1).
function$is_built_in_with$numero$arguments(atom, 1).
function$is_built_in_with$numero$arguments(atomic, 1).
function$is_built_in_with$numero$arguments(break, 0).
function$is_built_in_with$numero$arguments(clause, 2).
function$is_built_in_with$numero$arguments(clause, 3).
function$is_built_in_with$numero$arguments(close, 1).
function$is_built_in_with$numero$arguments(compare, 3).
function$is_built_in_with$numero$arguments(consult, 1).
function$is_built_in_with$numero$arguments(cos, 1).
function$is_built_in_with$numero$arguments(current_atom, 1).
function$is_built_in_with$numero$arguments(current_functor, 2).
function$is_built_in_with$numero$arguments(current_predicate, 2).
function$is_built_in_with$numero$arguments(debug, 0).
function$is_built_in_with$numero$arguments(debuging, 0).
function$is_built_in_with$numero$arguments(display, 1).
function$is_built_in_with$numero$arguments(erase, 1).
function$is_built_in_with$numero$arguments(erased, 1).
function$is_built_in_with$numero$arguments($excess_vars,4).
function$is_built_in_with$numero$arguments(exists, 1).
function$is_built_in_with$numero$arguments(exp, 1).
function$is_built_in_with$numero$arguments(expand_term, 2).
function$is_built_in_with$numero$arguments(fail, 0).
function$is_built_in_with$numero$arguments(fileerrors, 0).
function$is_built_in_with$numero$arguments(floor, 1).
function$is_built_in_with$numero$arguments(functor, 3).
function$is_built_in_with$numero$arguments(get, 1).
function$is_built_in_with$numero$arguments(get0, 1).
function$is_built_in_with$numero$arguments(halt, 0).
function$is_built_in_with$numero$arguments(instance, 2).
function$is_built_in_with$numero$arguments(integer, 1).
function$is_built_in_with$numero$arguments(is, 2).
function$is_built_in_with$numero$arguments(keysort, 2).
function$is_built_in_with$numero$arguments(leash, 1).
function$is_built_in_with$numero$arguments(listing, 0).
function$is_built_in_with$numero$arguments(listing, 1).
function$is_built_in_with$numero$arguments(log, 1).
function$is_built_in_with$numero$arguments(log10, 1).
function$is_built_in_with$numero$arguments(mod, 1).
function$is_built_in_with$numero$arguments(name, 2).
function$is_built_in_with$numero$arguments(nl, 0).
function$is_built_in_with$numero$arguments(nodebug, 0).
function$is_built_in_with$numero$arguments(nofileerrors, 0).
function$is_built_in_with$numero$arguments(nonvar, 1).
function$is_built_in_with$numero$arguments(nospy,1).
function$is_built_in_with$numero$arguments(numero, 1).
function$is_built_in_with$numero$arguments(op, 3).
function$is_built_in_with$numero$arguments(print, 1).
function$is_built_in_with$numero$arguments(print, 2).
function$is_built_in_with$numero$arguments(process_terms, 0).
function$is_built_in_with$numero$arguments(prompt, 2).
function$is_built_in_with$numero$arguments(put, 1).
function$is_built_in_with$numero$arguments(read, 1).
function$is_built_in_with$numero$arguments($reap, 2).
function$is_built_in_with$numero$arguments(reconsult, 1).
function$is_built_in_with$numero$arguments(recorda, 3).
function$is_built_in_with$numero$arguments(recorded, 3).
function$is_built_in_with$numero$arguments(recordz, 3).
function$is_built_in_with$numero$arguments(rename, 2).
function$is_built_in_with$numero$arguments(repeat, 0).
function$is_built_in_with$numero$arguments(retract, 1).
function$is_built_in_with$numero$arguments(save, 1).
function$is_built_in_with$numero$arguments(see, 1).
function$is_built_in_with$numero$arguments(seeing, 1).
function$is_built_in_with$numero$arguments(seen, 1).
function$is_built_in_with$numero$arguments(sin, 1).
function$is_built_in_with$numero$arguments(skip, 1).
function$is_built_in_with$numero$arguments(sort, 2).
function$is_built_in_with$numero$arguments(spy, 1).
function$is_built_in_with$numero$arguments(sqrt, 1).
function$is_built_in_with$numero$arguments(tab, 1).
function$is_built_in_with$numero$arguments($tag,1).
function$is_built_in_with$numero$arguments(tan, 1).
function$is_built_in_with$numero$arguments(trace, 0).
function$is_built_in_with$numero$arguments(tell, 1).
function$is_built_in_with$numero$arguments(telling, 1).
function$is_built_in_with$numero$arguments(told, 1).
function$is_built_in_with$numero$arguments(trace, 1).
function$is_built_in_with$numero$arguments(true, 0).
function$is_built_in_with$numero$arguments(var, 1).
function$is_built_in_with$numero$arguments(write, 1).
function$is_built_in_with$numero$arguments(writeq, 1).
function$is_built_in_with$numero$arguments('!', 0).
function$is_built_in_with$numero$arguments('=..', 2).
function$is_built_in_with$numero$arguments('+', 2).
function$is_built_in_with$numero$arguments('-', 1).
function$is_built_in_with$numero$arguments('-', 2).
function$is_built_in_with$numero$arguments('*', 2).
function$is_built_in_with$numero$arguments('^', 2).
function$is_built_in_with$numero$arguments('//', 2).
function$is_built_in_with$numero$arguments('/', 2).
function$is_built_in_with$numero$arguments('<', 2).
function$is_built_in_with$numero$arguments('\+',1).
function$is_built_in_with$numero$arguments('=<', 2).
function$is_built_in_with$numero$arguments('>', 2).
function$is_built_in_with$numero$arguments('>=', 2).
function$is_built_in_with$numero$arguments('+.', 2).
function$is_built_in_with$numero$arguments('==', 2).
function$is_built_in_with$numero$arguments('\==', 2).
function$is_built_in_with$numero$arguments('=', 2).
function$is_built_in_with$numero$arguments('=:=', 2).
function$is_built_in_with$numero$arguments('\==', 2).
function$is_built_in_with$numero$arguments('@<', 2).
function$is_built_in_with$numero$arguments('@=<', 2).
function$is_built_in_with$numero$arguments('@>', 2).
function$is_built_in_with$numero$arguments('@>=', 2).

and(A, B) :-
	A,
	B.

list$appended_to$list$yields$list([], List, List).
list$appended_to$list$yields$list([Old_First | Old_Rest], Other_List,
					[Old_First | Rest_of_New_List]) :-
	list$appended_to$list$yields$list(Old_Rest, Other_List,
							Rest_of_New_List).

conjuncts$are_converted_to$list(Term, List) :-
	Term =.. [Functor, Term_1, Term_2],
	element$is_a_member_of$list(Functor, [',', and]),
	conjuncts$are_converted_to$list(Term_1, List_1),
	conjuncts$are_converted_to$list(Term_2, List_2),
	list$appended_to$list$yields$list(List_1, List_2, List).
conjuncts$are_converted_to$list(Term, [Term]).

copy_full_to_true :-
	repeat,
	full_trace([Count, Depth, Port, Term]),
	assert(true_trace([Count, Depth, Port, Term])),
	Port = 'starting',
	!.

dec_count :-
	count(Count),
	retract(count(Count)),
	New_Count is Count - 1,
	assert(count(New_Count)),
	!.

dec_depth :-
	depth(Depth),
	retract(depth(Depth)),
	New_Depth is Depth - 1,
	assert(depth(New_Depth)),
	!.

term$is_used_for_a_call(Term) :-
	depth(Depth),
	Depth_Plus_1 is Depth + 1,
	inc_count,
	count(Count_Plus_1),
	inc_depth,
	$message_print$count$$depth$$port$$term(Count_Plus_1, Depth, 'call    ', Term),
	asserta(stacked_count(Count_Plus_1, Depth_Plus_1)),
	!.

term$is_used_for_a_failed(Term) :-
	depth(Depth),
	stacked_count(New_Count, Depth),
	retract(stacked_count(New_Count, Depth)),
	dec_depth,
	New_Depth is Depth -1,
	$message_print$count$$depth$$port$$term(New_Count, New_Depth, 'failed  ', Term),
	!.

term$is_used_for_a_no_more(Term) :-
	depth(Depth),
	stacked_count(New_Count, Depth),
	retract(stacked_count(New_Count, Depth)),
	dec_depth,
	New_Depth is Depth -1,
	$message_print$count$$depth$$port$$term(New_Count, New_Depth, 'no more ', Term),
	!.

count$is_stacked_for$depth(Count, Depth) :-
	stacked_count(Count, Depth),
	!.

term$is_used_for_an_exit(Term) :-
	depth(Depth),
	stacked_count(New_Count, Depth),
	dec_depth,
	depth(New_Depth),
	$message_print$count$$depth$$port$$term(New_Count, New_Depth, 'exit    ', Term),
	!.

term$is_used_for_an_out_of(Term) :-
	depth(Depth),
	stacked_count(New_Count, Depth),
	Depth_Minus_1 is Depth - 1,
	$message_print$count$$depth$$port$$term(New_Count, Depth_Minus_1, 'out of  ', Term),
	!.
	    
list$is_edited_to_produce$list(Old_List, New_List) :-
	files$are_written_to_with$list(user, [new_line, 'Current list: ', Old_List]),
	files$are_written_to_with$list(user, [new_line(2), tabulate,
		'Options: delete from list', tabulate, '- d', new_line]),
	files$are_written_to_with$list(user, [tabulate(2), ' add to list', tabulate(2), '- a',
								new_line]),
	files$are_written_to_with$list(user, [tabulate(2), ' replace list', tabulate(2), '- r',
								new_line]),
	files$are_written_to_with$list(user, [tabulate(2), ' exit', tabulate(3), '- e',
							new_line(2)]), 
	repeat,
	files$are_written_to_with$list(user, ['Enter option: ']), 
	character$is_inputted(Option_Letter),
	element$is_a_member_of$list(Option_Letter, [d, a, r, e]),
	!,
	(
	 (Option_Letter = e,
	  New_List = Old_List);
	 (files$are_written_to_with$list(user, ['Enter list: ']),
	  seeing(Old_Input_Stream_3),
	  see(user),
	  read(List_Members),
	  see(Old_Input_Stream_3),
	  conjuncts$are_converted_to$list(List_Members, Real_List),
	  (
	   (Option_Letter = d,
	    set$minus$set$yields$set(Old_List, Real_List, New_List));
	   (Option_Letter = a,
	    set$is_unioned_with$set$to_produce$set(Old_List, Real_List, New_List));
	   (Option_Letter = r,
	    New_List = Real_List)
	  )
	 )
	),
	!.

eliminate_out_ofs :-
	repeat,
	true_trace([Count, Depth, Port, Term]),
	$process_out_of$count$$depth$$port$$term(Count, Depth, Port, Term),
	Port = 'starting',
	!.

eliminate_no_mores :-
	repeat,
	true_trace([Count, Depth, Port, Term]),
	$process_no_more$$count$$depth$$port$$term(Count, Depth, Port, Term),
	Port = 'starting',
	!.

expression$is_evaluated_as$value(Expression, Value) :-
	Value is Expression,
	!.

character$is_inputted(Char) :-
	mode$is_replaced_by$mode(_, Char),
	!.

mode$is_replaced_by$mode(Mode_1, Mode_2) :-
	seeing(Old_Input_Stream),
	see(user),
	get0(Char),
	see(Old_Input_Stream),
	mode$is_replaced_via$mode$by$mode(Mode_1, Char, Mode_2),
	!.

mode$is_replaced_via$mode$by$mode(Mode, 10, c) :-				% 10 is a return.
	var(Mode),
	!.
mode$is_replaced_via$mode$by$mode(Mode, 10, Mode) :-				% 10 is a return.
	nonvar(Mode),
	!.
mode$is_replaced_via$mode$by$mode(Mode_1, Char, Mode_3) :-
	word$is_spelled_with$list(Mode_2, [Char]),
	mode$is_replaced_by$mode(Mode_2, Mode_3),
	!.

inc_count :-
	count(Count),
	retract(count(Count)),
	New_Count is Count + 1,
	assert(count(New_Count)),
	!.

inc_depth :-
	depth(Depth),
	retract(depth(Depth)),
	New_Depth is Depth + 1,
	assert(depth(New_Depth)),
	!.

list$is_a_list_of_ASCII_codes([]).
list$is_a_list_of_ASCII_codes([Element | List]) :-
	0 @=< Element,
	Element @=< 255,
	list$is_a_list_of_ASCII_codes(List).

make_true_trace :-
	eliminate_out_ofs,
	!,
	term$is_retracted(marked_for_deletion(X,Y)),
	eliminate_no_mores,
	!.

element$is_a_member_of$list(Element, [Element | Rest_of_List]).
element$is_a_member_of$list(Element, [First_Element | Rest_of_List]) :-
	element$is_a_member_of$list(Element, Rest_of_List).

element$is_a_member_wo_instantiation_of$list(Variable, [Element | Rest_of_List]) :-
	Variable == Element.
element$is_a_member_wo_instantiation_of$list(Element, [First_Element | Rest_of_List]) :-
	element$is_a_member_wo_instantiation_of$list(Element, Rest_of_List).

$message_print$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	term$is_retracted(real_count(Real_Count)),
	term$is_retracted(real_depth(Real_Depth)),
	assert(real_count(Count)),
	assert(real_depth(Depth)),
	asserta(full_trace([Count, Depth, Port, Term])),
	asserta(true_trace([Count, Depth, Port, Term])),
	$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term),
	!.

$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	functor(Term, Functor, _),
	invisible(Invisible_Terms),
	element$is_a_member_of$list(Functor, Invisible_Terms),
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(c),
	files$are_written_to_with$list_it(Count, Depth, Port, Term),
	mode_enquire,
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(d),
	search_depth(Search_Depth),
	Depth >= Search_Depth,
	files$are_written_to_with$list_it(Count, Depth, Port, Term),
	mode_enquire,
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(d),
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(j),
	return_count(Count),
	element$is_a_member_of$list(Port, ['exit    ', 'failed  ']),
	retract(return_count(Count)),
	files$are_written_to_with$list_it(Count, Depth, Port, Term),
	mode_enquire,
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(j),
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(l),
	functor(Term, Functor, _),
	terms(Terms),
	element$is_a_member_of$list(Functor, Terms),
	files$are_written_to_with$list_it(Count, Depth, Port, Term),
	mode_enquire,
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(l),
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(n),
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(r),
	files$are_written_to_with$list_it(Count, Depth, Port, Term),
	files(Files),
	(element$is_a_member_of$list(user, Files) ->
		files$are_written_to_with$list(user, [new_line]);
		true),
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(s),
	return_count(Count),
	retract(return_count(Count)),
	files$are_written_to_with$list_it(Count, Depth, Port, Term),
	mode_enquire,
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(s),
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(u),
	return_depth(Depth),
	retract(return_depth(Depth)),
	files$are_written_to_with$list_it(Count, Depth, Port, Term),
	mode_enquire,
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(u),
	!.
$mode_check$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	mode(X),
	files$are_written_to_with$list(user, [X,
		' is not a valid mode.  Type ''?'' for help or try again ']),
	mode_enquire,
	!.

mode_enquire :-
	files$are_written_to_with$list(user, [' --> ']),
	mode$is_replaced_by$mode(Mode_Char_In, Mode_Char_Out),
	(
	 (element$is_a_member_of$list(Mode_Char_Out, [h, '?']),
	  files$are_written_to_with$list(user, [new_line, 'Available modes:', new_line]),
	  files$are_written_to_with$list(user, [tabulate, 'a   abort', tabulate, 
				'abort the whole thing', new_line]),
	  files$are_written_to_with$list(user, [tabulate, 'c   creep', tabulate,
					'trace every step', new_line]),
	  files$are_written_to_with$list(user, [tabulate, 'd   depth', tabulate,
			'resume tracing at specific depth', new_line]),
	  files$are_written_to_with$list(user, [tabulate, 'e   edit', tabulate, 
				'edit one of the following lists', new_line]),
	  files$are_written_to_with$list(user, [tabulate(2), 'output files', new_line,
								tabulate(2),
			'predicates to search for', new_line, tabulate(2),
					'predicates not to print', new_line]),
	  files$are_written_to_with$list(user, [tabulate, 'h   help', tabulate, 
					'print this help message', new_line]),
	  files$are_written_to_with$list(user, [tabulate, 'j   jump', tabulate, 
		'resume tracing upon exiting or failing at same count',
								new_line]),
	  files$are_written_to_with$list(user, [tabulate, 'l   leap', tabulate,
		'resume tracing upon seeing a term in the list of terms', 
								new_line]),
	  files$are_written_to_with$list(user, [tabulate, 'n   notrace', tabulate, 
					'terminate tracing', new_line]),
	  files$are_written_to_with$list(user, [tabulate, 'r   run ', tabulate, 
			'trace without prompting for mode', new_line]),
	  files$are_written_to_with$list(user, [tabulate, 's   skip', tabulate, 
				'resume tracing at same count', new_line]),
	  files$are_written_to_with$list(user, [tabulate, 'u   up  ', tabulate, 
			'resume tracing at the next higher depth', new_line]),
	  files$are_written_to_with$list(user, [tabulate, '?   help', tabulate, 
				'print this help message', new_line(2)]),
	  files$are_written_to_with$list(user, ['Enter desired trace mode']),
	  mode_enquire
	 );
	 mode$becomes_the_new_mode(Mode_Char_Out)
	),
	!.

mode$becomes_the_new_mode(a) :-
	abort.
mode$becomes_the_new_mode(d) :-
	search_depth(Depth),
	files$are_written_to_with$list(user, ['At what depth do you want to resume tracing? ']),
	seeing(Old_Input_Stream),
	see(user),
	read(New_Depth),
	see(Old_Input_Stream),
	retract(search_depth(Depth)),
	assert(search_depth(New_Depth)),
	mode(Old_Mode),
	retract(mode(Old_Mode)),
	assert(mode(d)),
	!.
mode$becomes_the_new_mode(e) :-
	files$are_written_to_with$list(user, [new_line, 'Which list would you like to edit? ', 
							new_line, tabulate,
		'f = files, p = predicates, i = invisible predicates: ']),
	seeing(Old_Input_Stream),
	see(user),
	get0(Option),
	word$is_spelled_with$list(List, [Option]),
	get0(_),
	see(Old_Input_Stream),
	((List = f) ->
		(files(Old_Files),
		 list$is_edited_to_produce$list(Old_Files, New_Files),
		 retract(files(Old_Files)),
		 assert(files(New_Files)));
		((List = p) ->
			(terms(Old_Predicates),
			 list$is_edited_to_produce$list(Old_Predicates, New_Predicates),
			 retract(terms(Old_Predicates)),
			 assert(terms(New_Predicates)));
			((List = i) ->
				(invisible(Old_List),
				 list$is_edited_to_produce$list(Old_List, New_List),
				 retract(invisible(Old_List)),
				 assert(invisible(New_List)));
				 !))),
	files$are_written_to_with$list(user, ['Enter mode in which to conutinue tracing: ']),
	mode_enquire,
	!.
mode$becomes_the_new_mode(u) :-
	real_depth(Depth),
	Depth_Minus_1 is Depth-1,
	assert(return_depth(Depth_Minus_1)),
	mode(Old_Mode),
	retract(mode(Old_Mode)),
	assert(mode(u)),
	!.
mode$becomes_the_new_mode(Mode) :-
	element$is_a_member_of$list(Mode, [j,s]),
	mode(Old_Mode),
	retract(mode(Old_Mode)),
	assert(mode(Mode)),
	real_count(Count),
	assert(return_count(Count)),
	!.
mode$becomes_the_new_mode(Mode) :-
	mode(Old_Mode),
	retract(mode(Old_Mode)),
	assert(mode(Mode)),
	!.

or(A, B) :-
	call(A);
	call(B).

$process_out_of$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	((Port = 'out of  ',
	  not marked_for_deletion(_, _),
	  assert(marked_for_deletion(Count, Depth)));
	 (Port = 'call    ',
	  marked_for_deletion(Count, Depth),
	  retract(marked_for_deletion(Count, Depth)));
	 (marked_for_deletion(X,Y),
	  retract(true_trace([Count, Depth, Port, Term])));
	 true),
	!.	
	
$process_no_more$$count$$depth$$port$$term(Count, Depth, Port, Term) :-
	((not marked_for_deletion(_, _),
	  (Port = 'no more ';
	   Port = 'failed  ';
	   Port = 'no match'),
	  assert(marked_for_deletion(Count, Depth)),
	  retract(true_trace([Count, Depth, Port, Term])));
	 (Port = 'call    ',
	  marked_for_deletion(Count, Depth),
	  retract(true_trace([Count, Depth, Port, Term])),
	  retract(marked_for_deletion(Count, Depth)));
	 (marked_for_deletion(X,Y),
	  retract(true_trace([Count, Depth, Port, Term])));
	 true),
	!.

list$becomes_the_full_trace([]) :-
	not full_trace(_),
	!.
list$becomes_the_full_trace(Full_Trace) :-
	full_trace(Last_Line),
	retract(full_trace(Last_Line)),
	list$becomes_the_full_trace(All_But_One),
	list$appended_to$list$yields$list(All_But_One, [Last_Line], Full_Trace),
	!.

list$becomes_the_true_trace([]) :-
	not true_trace(_),
	!.
list$becomes_the_true_trace(True_Trace) :-
	true_trace(Last_Line),
	retract(true_trace(Last_Line)),
	list$becomes_the_true_trace(All_But_One),
	list$appended_to$list$yields$list(All_But_One, [Last_Line], True_Trace),
	!.

term$is_retracted(Term) :-
	clause(Term, true, Reference),
	erase(Reference),
	fail.
term$is_retracted(Head) :-
	clause(Head, Body, Reference),
	erase(Reference),
	fail.
term$is_retracted(_).
	
set$minus$set$yields$set(Set_1, Set_2, Set_1_Minus_Set_2) :-
	set_of(	X,	(element$is_a_member_of$list(X, Set_1),
			 not element$is_a_member_wo_instantiation_of$list(X, Set_2)
			),
		Set_1_Minus_Set_2),
	!.

element$satisfies$predicate$and_it_goes_in$set(Variable, Predicate, Resulting_Set) :-
	setof(Variable, Predicate, Resulting_Set).
element$satisfies$predicate$and_it_goes_in$set(Variable, Predicate, []).

word$is_spelled_with$list('', []) :-
	!.
word$is_spelled_with$list(Word, List) :- 
	(atomic(Word);
	 list$is_a_list_of_ASCII_codes(List)),
	name(Word, List),
	!.
word$is_spelled_with$list(Word, List) :- 
	files$are_written_to_with$list(user, ['Error in call to word$is_spelled_with$list: Word = ', Word,
			new_line, tabulate(2), 'List = ', List, new_line]),
	!.

term$is_traced_to_produce$full_trace$and$true_trace(Term, Full_Trace,
							True_Trace) :-
	term$is_traced_to_produce$full_trace(Term, Full_Trace),
	!,
	make_true_trace,
	!,
	list$becomes_the_true_trace(True_Trace),
	!.

term$is_traced_to_produce$full_trace(Term, Full_Trace) :-
	term$is_traced(Term),
	!,
	list$becomes_the_full_trace(Full_Trace),
	!.

term$is_traced(Term) :-
	term$is_retracted(count(Count)),
	term$is_retracted(depth(Depth)),
	term$is_retracted(files(Files)),
	term$is_retracted(full_trace(Full_Trace)),
	term$is_retracted(invisible(Terms)),
	term$is_retracted(mode(Mode)),	
	term$is_retracted(return_count(Return_Count)),
	term$is_retracted(return_depth(Return_Depth)),
	term$is_retracted(real_count(Real_Count)),
	term$is_retracted(real_depth(Real_Depth)),
	term$is_retracted(search_depth(Search_Depth)),
	term$is_retracted(stacked_count(Count, Depth)),
	term$is_retracted(true_trace(True_Trace)),
	assert(count(0)),
	assert(depth(1)),
	assert(invisible([])),
	assert(mode(c)),
	assert(terms([])),
	assert(files([user])),
	assert(return_count(1)),
	assert(return_depth(1)),
	assert(search_depth(1)),
	$message_print$count$$depth$$port$$term(0, 1, 'starting', Term),
	!,
	(term$is_whyed(Term); true),
	!.

set$is_unioned_with$set$to_produce$set([], [], []).
set$is_unioned_with$set$to_produce$set([], [B_1 | B_Rest], C) :-
	set$is_unioned_with$set$to_produce$set([], B_Rest, Rest),
	((element$is_a_member_of$list(B_1, Rest), C = Rest);
		C = [B_1 | Rest]).
set$is_unioned_with$set$to_produce$set([A_1 | A_Rest], B, A_Union_B) :-
	set$is_unioned_with$set$to_produce$set(A_Rest, B, A_Rest_Union_B),
	((element$is_a_member_of$list(A_1, A_Rest_Union_B), A_Union_B = A_Rest_Union_B);
		A_Union_B = [A_1 | A_Rest_Union_B]).
		
/*************************************************************
	    c,d are inputs     c',d' are outputs
			
Message	assert	stacked_count	retract	count	depth	print
--------------------------------------------------------------
call	(c',d')	  --		  --	c'=c+1	d'=d+1	c',d
enter	  --	(cl,d)		  --	c'=c	d'=d	cl,d-1
outof	  --	(cl,d)		  --	c'=c	d'=d	cl,d-1
failed	  --	(cl,d)		(cl,d)	c'=c	d'=d-1	cl,d'
exit	  --	(cl,d)	 	 --	c'=c	d'=d-1	cl,d'
nomatch	  --	(cl,d)		(cl,d)	c'=c	d'=d-1	cl,d'
nomore	  --	(cl,d)		(cl,d)	c'=c	d'=d-1	cl,d'
*************************************************************/

term$is_whyed(Term) :-
	Term = (Term_1 -> Term_2; Term_3),
	!,
	(term$is_whyed(Term_1) ->
		term$is_whyed(Term_2);
		term$is_whyed(Term_3)).
term$is_whyed(Term) :-
	Term = (Term_1 -> Term_2),
	!,
	(term$is_whyed(Term_1) ->
		term$is_whyed(Term_2)).
term$is_whyed(Term) :-
	(
	 Term = (Term_1, Term_2) ;
	 Term = (Term_1 and Term_2)
	),
	!,
	term$is_whyed(Term_1),
	term$is_whyed(Term_2).
term$is_whyed(Term) :-
	(
	 Term = (Term_1; Term_2) ;
	 Term = (Term_1 or Term_2)
	),
	!,
	(
	 term$is_whyed(Term_1) ;
	 term$is_whyed(Term_2)
	).
term$is_whyed(Term) :-
	count(Count),
	functor(Term, Functor, _),
	function$is_built_in_with$numero$arguments(Functor, _),
	!,
	term$is_used_for_a_call(Term),
	(
	 (Term,
	  (
	   term$is_used_for_an_exit(Term)
	   ;
	   (inc_depth,
	    term$is_used_for_an_out_of(Term),
	    fail)))
	 ;
	 (term$is_used_for_a_failed(Term),
	  !,
	  fail)).
term$is_whyed(Term) :-
	count(Count),
	(Term = call(Body);
	 Term = $user_call(Body)),
	!,
	term$is_used_for_a_call(Term),
	(
	 (term$is_whyed(Body),
	  (
	   term$is_used_for_an_exit(Term)
	   ;
	   (inc_depth,
	    term$is_used_for_an_out_of(Term),
	    fail)))
	 ;
	 (term$is_used_for_a_failed(Term),
	  !,
	  fail)).
term$is_whyed(Term) :-
	not clause(Term, _),
	!,
	term$is_used_for_a_call(Term),
	depth(Depth),
	stacked_count(New_Count, Depth),
	retract(stacked_count(New_Count, Depth)),
	dec_depth,
	depth(New_Depth),
	$message_print$count$$depth$$port$$term(New_Count, New_Depth, 'no match', Term),
	!,
	fail.
term$is_whyed(Term) :-
	!,	% assumes clause(Term, _) succeeds
	term$is_used_for_a_call(Term),
	(
	 (clause(Term, Body),
	  depth(Depth),
	  expression$is_evaluated_as$value(Depth - 1, Depth_Minus_1),
	  count$is_stacked_for$depth(Count, Depth),
	  $message_print$count$$depth$$port$$term(Count, Depth_Minus_1, 'enter   ', Term),
	  (
	   (term$is_whyed(Body),
	    (
	     term$is_used_for_an_exit(Term)
	     ;
	     (inc_depth,
	      fail)))
	   ;
	   (term$is_used_for_an_out_of(Term),
	    fail)))
	  ;
	  (term$is_used_for_a_no_more(Term),
	   !,
	   fail)).

files$are_written_to_with$list([], Output_List) :-
	!.
files$are_written_to_with$list([File | File_List], Output_List) :-
	files$are_written_to_with$list(File, Output_List),
	files$are_written_to_with$list(File_List, Output_List),
	!.
files$are_written_to_with$list(user, Output_List) :-
	telling(Old_File),
	tell(user),
	files$are_written_to_with$list_list(Output_List),
	tell(Old_File),
	!.
files$are_written_to_with$list(File, Output_List) :-
	atom(File),
	telling(Old_File),
	tell(File),
	files$are_written_to_with$list_list(Output_List),
	nl,
	tell(Old_File),
	!.

files$are_written_to_with$list_it(Count, Depth, Port, Term) :-
	files(Files),
	files$are_written_to_with$list(Files, [trace_line([Count, Depth, Port, Term])]).

files$are_written_to_with$list_list(Output_List) :-
	var(Output_List),
	write(Output_List),
	!.
files$are_written_to_with$list_list([Term | Terms]) :-
	var(Term),
	write(Term),
	!,
	files$are_written_to_with$list_list(Terms),
	!.
files$are_written_to_with$list_list([]) :- !.	% Stop at the end of the list of terms.
files$are_written_to_with$list_list([Term | Terms]) :-
	files$are_written_to_with$list_term(Term),
	!,
	files$are_written_to_with$list_list(Terms),
	!.

files$are_written_to_with$list_term(trace_line([Count, Depth, Port, Term])) :-
	files$are_written_to_with$list_list([Count, '.  (', Depth, ')  ', Port, '  ', Term]).

files$are_written_to_with$list_term(trace_list([Term | Terms])) :-
	files$are_written_to_with$list_term(new_line),
	files$are_written_to_with$list_term(trace_line(Term)),
	files$are_written_to_with$list_term(trace_list(Terms)),
	!.
files$are_written_to_with$list_term(trace_list([])) :-
	files$are_written_to_with$list_term(new_line),
	!.

files$are_written_to_with$list_list(Term) :-
	write(Term),
	!.

files$are_written_to_with$list_term(new_line) :-
	files$are_written_to_with$list_term(new_line(1)).
files$are_written_to_with$list_term(new_line(0)) :- !.
files$are_written_to_with$list_term(new_line(N)) :-
	put("
"),
	M is N-1,
	files$are_written_to_with$list_term(new_line(M)).
files$are_written_to_with$list_term(space) :-
	files$are_written_to_with$list_term(space(1)), !.
files$are_written_to_with$list_term(space(0)) :- !.
files$are_written_to_with$list_term(space(N)) :-
	put(" "),
	M is N-1,
	files$are_written_to_with$list_term(space(M)).
files$are_written_to_with$list_term(tabulate) :-
	files$are_written_to_with$list_term(tabulate(1)).
files$are_written_to_with$list_term(tabulate(0)) :- !.
files$are_written_to_with$list_term(tabulate(N)) :-
	N > 0,
	put("	"),
	M is N-1,
	files$are_written_to_with$list_term(tabulate(M)).
files$are_written_to_with$list_term(Term) :-	% If don't recognize the term, just write it.
	write(Term), !.
