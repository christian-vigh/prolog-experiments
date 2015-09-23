/****h* Modules/list
 ===============================================================================
 *
 * NAME
 *	list - List-manipulation predicates.
 *
 * FILE
 *	Modules/list.pro
 *
 * CONTENTS
 *	Contains a set of list-manipulation predicates.
 *
 * AUTHOR
 *	Christian Vigh, July 2005.
 *
 ===============================================================================
 ******/

 
:- module(list).
:-	export([append/3]).
:-	export([belongs/2]).
:-	export([countoccurrences/2]).
:-	export([countoccurrences/3]).
:-	export([difference/3]).
:-	export([drop/3]).
:-	export([duplicate/3]).
:-	export([duplicateatom/3]).
:-	export([equal/2]).
:- 	export([explode/2]).
:-	export([filtermin/3]).
:-	export([filtermax/3]).
:- 	export([flatten/2]).
:-	export([implode/2]).
:-	export([insert/4]).
:-	export([intersection/3]).
:-	export([last/2]).
:-	export([left/3]).
:-	export([length/2]).
:-	export([lengthoflongest/2]).
:-	export([lengthofshortest/2]).
:-	export([lower/2]).
:-	export([middle/4]).
:-	export([nummax/3]).
:-	export([nummax_of_list/3]).
:-	export([nummin/3]).
:-	export([nummin_of_list/3]).
:- 	export([partition/4]).
:-	export([removedups/2]).
:-	export([removeoccurrences/3]).
:-	export([repeated/2]).
:-	export([reverse/2]).
:-	export([right/3]).
:-	export([rotate/3]).
:-	export([rotateleft/3]).
:-	export([rotateright/3]).
:-	export([set/2]).
:-	export([slice/3]).
:-	export([strmax/3]).
:-	export([strmax_of_list/3]).
:-	export([strmin/3]).
:-	export([strmin_of_list/3]).
:-	export([take/3]).
:-	export([union/3]).
:-	export([upper/2]).
:-	export([wholelength/2]).
:- end_module(list).
 
 :- body(list).



/****f* Modules.List/append
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	append/3
 *
 * SYNTAX
 *	append(List1, List2, Result)
 *
 * PURPOSE
 *	concatenates [List1] and [List2] to unify [Result].
 *
 * ARGUMENTS
 *	[List1], [List2] (i,o) -
 *		Lists to bre concatenated.
 *	[Result] (i,o) -
 *		Result list.
 *
 * NOTES
 *		The predicate is constructed in such a way that any argument
 *		can be an unbound variable.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

append([], X, X).
append([A|X], Y, [A|Z]) :-
 	append(X, Y, Z).
append(X, Y, Z) :-
 	Z = [X, Y].

/******/




/****f* Modules.List/belongs
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	belongs/2
 *
 * SYNTAX
 *	belongs(Element, List)
 *
 * PURPOSE
 *	Satisfies if [Element] belongs to [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to be searched for inclusion of [Element].
 *	[Element] (o) -
 *		Element whose presence is to be searched in [List].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

belongs(Element, [Element|Tail]) :-
	!.
belongs(Element, [_|Tail]) :-
	belongs(Element, Tail).

/******/



/****f* Modules.List/countoccurrences
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	countoccurrences/2
 *
 * SYNTAX
 *	countoccurrences(List, Result)
 *
 * PURPOSE
 *	Unifies [Result] with a list of 2-elements list, where the first element of
 *	the pair is an element of [List], and the second element is the number of
 *	times it appears within[List]. Individual elements appear in sorted order
 *	in [Result].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to process.
 *	[Result] (o) -
 *		Resulting list.
 *
 * EXAMPLE
 *	countoccurrences([a,b,c,a,b,b,b,d,f], Result).
 *	Result = [ [a, 2], [b, 4], [c, 1], [d, 1], [f, 1] ]
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

countoccurrences([], []).
countoccurrences(List, Result) :-
	sort(List, NewList),
	docountoccurrences(NewList, 1, Result).

docountoccurrences([X, X |Tail], Counter, Result) :-
	NewCounter is Counter + 1,
	docountoccurrences([X | Tail], NewCounter, Result).
docountoccurrences([X, Y|Tail], Counter, Result) :-
	X \== Y,
	docountoccurrences([Y | Tail], 1, TempResult),
	append([[X, Counter]], TempResult, Result).
docountoccurrences([Atom], Counter, Result) :-
	atom(Atom),
	Result = [ [Atom, Counter] ].

/******/


	
	
/****f* Modules.List/countoccurrences (3 parameters)
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	countoccurrences/3
 *
 * SYNTAX
 *	countoccurrences(Atom, List, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the number of times that [Atom] has been found in [List].
 *
 * ARGUMENTS
 *	[Atom] (i,o) -
 *		Element to search for.
 *	[List] (i) -
 *		List to be searched.
 *	[Result] (i,o) -
 *		Number of times[Atom] has been found in [List].
 *
 * NOTES
 *	countoccurrences has been written so that [Atom] and [Result] can be unbound :
 *		countoccurrences(a, [a,b,c], X) -> X = 1
 *		countoccurrences(X, [a,b,a], 2) -> X = a
 *		countoccurrences(a, [a,b,a], 1) -> no
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

countoccurrences(Atom, [], 0).
countoccurrences(Atom, [Atom|Tail], Result) :-
	countoccurrences(Atom, Tail, TempResult),
	Result is TempResult + 1.
countoccurrences(Atom, [AnyElement|Tail], Result) :-
	Atom \== AnyElement,	% This test is important otherwise prolog would backtrack through this rule
	countoccurrences(Atom, Tail, Result).

/******/




/****f* Modules.List/difference
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	difference/3
 *
 * SYNTAX
 *	difference(List1, List2, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the difference of [List1] and [List2], ie with all 
 *	the elements :
 *	- contained in [List1] but not in [List2] and
 *	- contained in [List2] but not in [List1]
 *
 * ARGUMENTS
 *	[List1], [List2] (i) -
 *		Lists on which to apply the difference.
 *	[Result] (o) -
 *		Resulting list.
 *
 * EXAMPLE
 *	difference([a,b,c], [c,d,e]) -> [a,b,d,e]
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

difference(List1, List2, Result) :-
	removedups(List1, X1), 
	removedups(List2, X2),
	dodifference(X1, X2, R1),
	dodifference(X2, X1, R2),
	union(R1, R2, Result).
	
dodifference([], _, []).	
dodifference([H|T], List2, Result) :-
	not(belongs(List2, H)),
	dodifference(T, List2, TempList),
	append([H], TempList, Result).
dodifference([H|T], List2, Result) :-
	belongs(List2, H),
	dodifference(T, List2, Result).

/******/




/****f* Modules.List/drop
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	drop/3
 *
 * SYNTAX
 *	drop(List, Count, Result)
 *
 * PURPOSE
 *	Unifies [Result] with [List], after removing [Count] elements from [List] head.
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to be processed.
 *	[Count] (i) -
 *		Number of elements to remove from beginning of [List].
 *	[Result] (o) -
 *		Unified to [List] withouth the [Count] first elements.
 *
 * NOTES
 *	Calling drop with a [Count] of zero unifies [Result] with [List].
 *	Calling drop with an empty list unifies [Result] with an empty list.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

drop(List, 0, List).
drop([], _, []).
drop([_|Tail], Index, NewList) :-
	Index > 0,
	NewIndex is Index - 1,
	drop(Tail, NewIndex, NewList).

/******/




/****f* Modules.List/duplicate
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	duplicate/3
 *
 * SYNTAX
 *	duplicate(List, Count, Result)
 *
 * PURPOSE
 *	Duplicates [Count] times the elements of [List] and unifies [Result] with
 *	the resulting list. Duplicate elements are put in an adjacent way.
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose elements are to be duplicated.
 *	[Count] (i) -
 *		Number of duplications to perform on each element of list.
 *	[Result] (o) -
 *		Resulting list.
 *
 * NOTES
 *	Duplicating an empty list unifies [Result] with an empty list.
 *	Calling duplicate with a duplication [Count] less than or equal to zero
 *	unifies [Result] with [List].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

duplicate([], _, []).
duplicate(List, Count, List) :-
	Count =< 0.
duplicate([Head|Tail], Count, Result) :-
	duplicateatom(Head, Count, DuplicatedElement),
	duplicate(Tail, Count, TempList),
	append(DuplicatedElement, TempList, Result).
	
duplicateatom(_, 0, _).
duplicateatom(Atom, 1, [Atom, Atom]).
duplicateatom(Atom, Count, Result) :-
	Count > 0,
	NewCount is Count - 1,
	duplicateatom(Atom, NewCount, NewResult),
	append([Atom], NewResult, Result).

/******/




/****f* Modules.List/equal
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	equal/3
 *
 * SYNTAX
 *	equal(List1, List2)
 *
 * PURPOSE
 *	Succeeds if [List1] has the same content as [List2].
 *
 * ARGUMENTS
 *	[List1], [List2] (i) -
 *		Lists to be compared.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

equal([], []).
equal([H|T1], [H|T2]) :-
	equal(T1, T2).

/******/


	

/****f* Modules.List/explode
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	explode/2
 *
 * SYNTAX
 *	explode(Atom, List)
 *
 * PURPOSE
 *	Sets [List] to the list of characters of [Atom].
 *
 * ARGUMENTS
 *	[Atom] (i) -
 *		Atom to split.
 *	[List] (o) -
 *		List built from the individual characters of [Atom].
 *
 * NOTES
 *	This predicate has been written because the standard atom_list predicate
 *	 does not work (it does not return a list of character codes).
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

explode(Atom, []) :-
	atom_length(Atom, L),
	L =:= 0.
explode(Atom, Result) :-
	sub_atom(Atom, 1, 1, C),
	string_term(Term, C),		% Needed because Atom('2', 1, 1) returns '2', not 2
	sub_atom(Atom, 2, _, R),
	explode(R, TempResult),
	string_term(Term, V),
	append([V], TempResult, Result).	

/******/




/****f* Modules.List/filtermin, filtermax
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	filtermin/3, filtermax/3
 *
 * SYNTAX
 *
 * PURPOSE
 *	filtermin unifies [Result] with all values in [List] greater than or equal
 *	to [Min].
 *	filtermax unifies [Result] with all values in [List] less than or equal 
 *	to [Max].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to be filtered.
 *	[Min] (i) -
 *		For filtermin, minimum value to keep.
 *	[Max] (i) -
 *		For filtermax, maximum value to keep.
 *	[Result] (o) -
 *		Resulting list.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

filtermin([], _, []).
filtermin([H|T], Min, Result) :-
	H < Min,
	filtermin(T, Min, Result).
filtermin([H|T], Min, Result) :-
	filtermin(T, Min, TempResult),
	append([H], TempResult, Result).

filtermax([], _, []).
filtermax([H|T], Max, Result) :-
	H > Max,
	filtermax(T, Max, Result).
filtermax([H|T], Max, Result) :-
	filtermax(T, Max, TempResult),
	append([H], TempResult, Result).

/******/




/****f* Modules.List/flatten
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	flatten/2
 *
 * SYNTAX
 *	flatten(List, Result)
 *
 * PURPOSE
 *	Flattens [List] into [Result]. Every sublist in [List] is expanded.
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to be flattened.
 *	[Result] (o) -
 *		Unified to a list corresponding to [List], with all sublists
 *		expanded.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

flatten([], []).

flatten(X, [X]) :-
	not(is_list(X)).

flatten([H|T], List) :-
	flatten(H, L1),
	flatten(T, L2),
	append(L1, L2, List).

/******/


	

/****f* Modules.List/implode
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	implode/2
 *
 * SYNTAX
 *	implode(List, Result)
 *
 * PURPOSE
 *	Concatenates all elements from [List] into a single atom, [Result].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose elements are to be concatenated.
 *	[Result] (o) -
 *		Resulting atom.
 *
 * NOTES
 *	This predicate can be viewed as the counterpart of explode/2.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

implode(List, Result) :-
	atomlist_concat(List, Result).

/******/	




/****f* Modules.List/insert
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	insert/4
 *
 * SYNTAX
 *	insert(List, Position, Sublist, Newlist)
 *
 * PURPOSE
 *	Unifies [Newlist] with a list formed with [Sublist] inserted at [Position]
 *	within [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List where [Sublist] is to be inserted.
 *	[Position] (i) -
 *		Index of the element in [List] before which [Sublist] is to be
 *		inserted. The elements are numbered starting from 1.
 *	[Sublist] (i) -
 *		List to insert.
 *	[Result] (o) -
 *		Resulting list.
 *
 * NOTES
 *	Inserting an empty list will unify [Result] with [List].
 *	Inserting a [Sublist] into an empty list will unify [Result] with [Sublist].
 *	If [Position] is less than 1, then [Sublist] is simply prepended to [List].
 *	If [Position] if greater than length of [List], then [Sublist] is simply
 *	appended to [List].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

insert(List, _, [], List).
insert([], _, X, X).

insert(List, Position, Sublist, Newlist) :-
	Position < 1,
	append(Sublist, List, Newlist).

insert(List, Position, Sublist, Newlist) :-
 	length(List, L),
 	Position > L,
 	append(List, Sublist, Newlist).
 	
insert(List, Position, Sublist, Newlist) :-
 	Until is Position - 1,
 	left(List, Until, LeftPart),
 	drop(List, Until, RightPart),
 	append(LeftPart, Sublist, X),
 	append(X, RightPart, Newlist).

/******/


 

/****f* Modules.List/intersection
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	intersection/3
 *
 * SYNTAX
 *	intersection(List1, List2, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the intersection of [List1] and [List2], ie all
 *	elements present in both [List1] and [List2].
 *
 * ARGUMENTS
 *	[List1], [List2] (i) -
 *		Lists to be intersected.
 *	[Result] (o) -
 *		Intersection of [List1] and [List2].
 *
 * NOTES
 *	intersection([d,a,b,c],[x,y,a]) -> [a].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

intersection(List1, List2, Result) :-
	removedups(List1, X1), 
	removedups(List2, X2),
	dointersection(X1, X2, R1),
	dointersection(X2, X1, R2),
	union(R1, R2, Result).
	
dointersection([], _, []).	
dointersection([H|T], List2, Result) :-
	belongs(List2, H),
	dointersection(T, List2, TempList),
	append([H], TempList, Result).
dointersection([H|T], List2, Result) :-
	not(belongs(List2, H)),
	dointersection(T, List2, Result).

/******/





/****f* Modules.List/last
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	last/2
 *
 * SYNTAX
 *	last(List, Element)
 *
 * PURPOSE
 *	Unifies [Element] with the last element of [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose last element is to be extracted.
 *	[Element] (o) -
 *		Last element of [List].
 *
 * NOTES
 *	The last element of an empty list is the empty list.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

last([],[]).
last(X, X) :-
	length(X, L), L = 1, 
	!.
last([_|Tail], X) :-
	last(Tail, X).

/******/





/****f* Modules.List/left
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	left/3
 *
 * SYNTAX
 *	left(List, Count, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the [Count] first elements taken from [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose first elements are to be taken.
 *	[Count] (i) -
 *		Number of elements to take.
 *	[Result] (o) -
 *		Unified with the [Count] first elements taken from [List].
 *
 * NOTES
 *	Calling left on an empty list unifies [Result] with an empty list.
 *	Calling left with a [Count] of zero unifies [Result] with an empty list.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

left([], _, []).		% Any empty list returns the empty list.
				% (useful also when Count is greater than
				% list length).
left(X, 0, X) :-		% We reach here the last element to extract
	atom(X).
left(X, 0, []).			% For other case, stop when we have extracted
				% Count elements
left([Head|Tail], Count, Result) :-
 	Count > 0,
 	NewCount is Count - 1,
 	left(Tail, NewCount, NewResult),
 	append([Head], NewResult, Result).

/******/




/****f* Modules.List/length
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	length/2
 *
 * SYNTAX
 *	length( List, Result )
 *
 * PURPOSE
 *	Unifies [Result] with the number of elements in [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose number of elements is to be counted.
 *	[Result] (o) -
 *		Unified to the number of elements of [List].
 *
 * NOTES
 *	The length of an empty list is 0. 
 *	The length of an atom is 1.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

length([], L) :-
	L = 0, !.
length([_|Tail], L) :-
	length(Tail, TailLength),
	L is TailLength + 1, !.
length(X, L) :-
	atom(X),
	L = 1, !.

/******/




/****f* Modules.List/lengthofshortest, lengthoflongest
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	lengthoflongest/2, lengthofshortest/2
 *
 * SYNTAX
 *	lengthoflongest(List, Result)
 *	lengthofshortest(List, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the length of the shortest (lengthofshortest) or
 *	longest (lengthoflongest) atom in [List].
 *
 * ARGUMENTS
 *	[List] (i,o) -
 *		List to be searched for.
 *	[Result] (i,o) -
 *		List of the shortest (lengthofshortest) or longest (lengthoflongest)
 *		atom found in [List].
 *
 * NOTES
 *	The predicate is written so that X can be bound or not ; for
 *	example :
 *		lengthoflongest([a,ab], X) 
 *	will unify X to 2 (length of longest element, 'ab') ; whereas
 *		lengthoflongest([a,ab], 1)
 *	will fail.	
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

lengthoflongest([], 0).
lengthoflongest([H|T], Result) :-
	lengthoflongest(T, TempResult),
	atom_length(H, X),
	lengthofcurrentlongest(X, TempResult, Result).

%
% We have to handle either an bound or unbound Result argument. It is not
% possible in this case to use constructs such as :
%	X > Result,
%	Result is X.
% (since Result can be bound or not). For this reason, we use unification to
% retain the longest atom seen so far. This is the job of lengthofcurrent, 
% which unifies Result with max(CurrentAtomLength, PreviousMax).
%
lengthofcurrentlongest(X, Y, X) :-
	X > Y, !.
lengthofcurrentlongest(X, Y, Y) :- !.
	

%
% length of shortest element.
%
lengthofshortest([], 0).
lengthofshortest(List, Result) :-
	dolengthofshortest(List, Result).
	
dolengthofshortest([], 32767).
dolengthofshortest([H|T], Result) :-
	dolengthofshortest(T, TempResult),
	atom_length(H, X),
	dolengthofcurrentshortest(X, TempResult, Result).

dolengthofcurrentshortest(X, Y, X) :-
	X < Y, !.
dolengthofcurrentshortest(X, Y, Y) :- !.

/******/




/****f* Modules.List/lower
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	lower/2
 *
 * SYNTAX
 *	lower(List, Result)
 *
 * PURPOSE
 *	Unifies [Result] with all the lowercased elements of [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose elements are to be lowercased.
 *	[Result] (o) -
 *		Resulting (lowercased) list.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

lower([], []).
lower([H|T], Result) :-
	atom_uplow(H, X),
	lower(T, TempResult),
	append([X], TempResult, Result).
lower(Atom, Result) :-
	atom_uplow(Atom, Result).

/******/




/****f* Modules.List/middle
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	middle/4
 *
 * SYNTAX
 *	middle(List, Start, Count, Result)
 *
 * PURPOSE
 *	Unifies [Result] with [Count] elements of [List] taken at position [Start].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to be extracted.
 *	[Start] (i) -
 *		Index of the first element to take from [List].
 *	[Count] (i) -
 *		Number of elements to take from [List].
 *	[Result] (o) -
 *		Unified with a sublist of [Count] elements taken from [List], 
 *		starting at position [Start].
 *
 * NOTES
 *	The first element of a list is numbered 1.
 *	Calling middle with an empty list unifies [Result] with the empty list.
 *	If [Start] or [Count] are less than 1, or if [Start] is greater than the
 *	number of elements in [List], then [Result] is unified with the empty list.
 *	If [Start] + [Count] - 1 are greater than the length of [List], then [Result]
 *	is unified with all the elements of [List], starting at position [Start].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

 middle([], _, _, []).
 
 middle(List, Start, Count, []) :-
 	Start < 1.
 middle(List, Start, Count, []) :-
 	Count < 1.
 middle(List, Start, Count, []) :-
 	length(List, L),
 	Start > L.
 	
 middle(List, Start, Count, NewList) :-
 	NewStart is Start - 1,
 	drop(List, NewStart, X),
 	left(X, Count, NewList).

/******/




/****f* Modules.List/nummin, nummax
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	nummin/3, nummax/3
 *
 * SYNTAX
 *	nummin(X, Y, R), nummax(X, Y,R)
 *
 * PURPOSE
 *	Unifies [R] with the minimum/maximum value of [X] and [Y].
 *
 * ARGUMENTS
 *	[X], [Y] (i) -
 *		Values to be compared.
 *	[R] (o) -
 *		Unified with the minimum/maximum value of [X] and [Y].
 *
 * NOTES
 *	The comparison is performed numerically.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

nummin(X, Y, Y) :-		% Minimum value of two elements
	Y =< X, !.
nummin(X, Y, X).

nummax(X, Y, Y) :-		% Maximum value of two elements
	Y >= X, !.
nummax(X, Y, X).

/******/




/****f* Modules.List/nummin_of_list, nummin_of_max
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	nummin_of_list/2, nummax_of_list/2
 *
 * SYNTAX
 *	nummin_of_list(List, Result), 
 * 	nummax_of_list(List, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the minimum/maximum value that can be found in [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose minimum/maximum value is to be searched.
 *	[Result] (o) -
 *		Unified with the minimum/maximum value found in [List].
 *
 * NOTES
 *	Comparisons are performed numerically.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

nummin_of_list([X], X).
nummin_of_list([H|T], X) :-
	nummin_of_list(T, XBis), nummin(H, XBis, X).
	
nummax_of_list([X], X).
nummax_of_list([H|T], X) :-
	nummax_of_list(T, XBis), nummax(H, XBis, X).	

/******/



	
/****f* Modules.List/partition
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	partition/4
 *
 * SYNTAX
 *	partition(List, Pivot, ListBelow, ListAbove)
 *
 * PURPOSE
 *	Partition/4 processes all elements from [List]. It adds in [ListBelow] the
 *	elements that are less than the value [Pivot], and in [ListAbove] the
 *	elements thar are greater than or equal to [Pivot].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to be processed.
 *	[Pivot] (i) -
 *		Pivot value.
 *	[ListBelow] (o) -
 *		Unified to all the elements of [List] that are less than [Pivot].
 *	[ListAbove] (o) -
 *		Unified to all the elements of [List] that are greater or equal 
 *		to [Pivot].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

partition([], _, [], []).
partition([H|T], Pivot, ListBelow, ListAbove) :-
	H < Pivot,
	partition(T, Pivot, TempBelow, ListAbove),
	append([H], TempBelow, ListBelow).
partition([H|T], Pivot, ListBelow, ListAbove) :-
	partition(T, Pivot, ListBelow, TempAbove),
	append([H], TempAbove, ListAbove).

/******/




/****f* Modules.List/removedups
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	removedups/2
 *
 * SYNTAX
 *	removedups(List, Result)
 *
 * PURPOSE
 *	Removes any adjacent elements from [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to be processed.
 *	[Result] (o) -
 *		Resulting list, where all the adjacent elements of [List] have
 *		been removed.
 *
 * NOTES
 *	Calling removedups on an atom or empty list will unify [Result] with the
 *	atom or the empty list.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

removedups([X, X|Tail], Result) :-
	removedups([X|Tail], Result).
	
removedups([X, Y|Tail], Result) :-
	removedups([Y|Tail], Newlist),
	append([X], Newlist, Result).
removedups(X, X).
removedups([], []).

/******/




/****f* Modules.List/removeoccurrences
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	removeoccurrences/3
 *
 * SYNTAX
 *	removeoccurrences(Atom, List, Result).
 *
 * PURPOSE
 *	Unifies [Result] with [List] after having removed everyoccurrence of
 *	[Atom] within [List].
 *
 * ARGUMENTS
 *	[Atom] (i) -
 *		Element to remove.
 *	[List] (i) -
 *		List to be processed.
 *	[Result] (o) -
 *		Resulting list.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

removeoccurrences(Atom, [], []).
removeoccurrences(Atom, [Atom|Tail], Result) :-
	removeoccurrences(Atom, Tail, Result).
removeoccurrences(Atom, [AnyElement|Tail], Result) :-
	Atom \== AnyElement,
	removeoccurrences(Atom, Tail, TempResult),
	append([AnyElement], TempResult, Result).

/******/


	

/****f* Modules.List/repeated
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	repeated/2
 *
 * SYNTAX
 *	repeated(List, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the list of elements from [List] that appear more
 *	than one time.
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to be processed.
 *	[Result] (o) -
 *		Unified to the list of elements that appear more than one time in
 *		[List].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

repeated(List, Result):-
	dorepeated(List, [], TempResult),	% Use an accumulator
	sort(TempResult, Result).
	
dorepeated([], BuildingList, BuildingList).	% All elements processed : Result is bind to BuildingList
dorepeated([H|T], BuildingList, Result) :-	% Add every element présent in T but not yet in BuildingList
	belongs(H, T),
	not(belongs(H, BuildingList)),
	dorepeated(T, [H | BuildingList], Result).
dorepeated([H|T], BuildingList, Result) :-	% Other cases : ignore 1st element of list since not present in tail
	dorepeated(T, BuildingList, Result).

/******/




/****f* Modules.List/reverse
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	reverse/2
 *
 * SYNTAX
 *	reverse(List, Result)
 *
 * PURPOSE
 *	Puts all elements of [List] in reverse order, and unifies resulting list
 *	to [Result].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to be reversed.
 *	[Result] (o) -
 *		Reversed list.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

reverse([], []).
reverse([Head|Tail], Newlist) :-
	reverse(Tail, L1),
	append(	L1, [Head], Newlist).

/******/




/****f* Modules.List/right
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	right/3
 *
 * SYNTAX
 *	right(List, Count, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the [Count] last elements of [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose last elements are to be taken.
 *	[Count] (i) -
 *		Number of elements to take from the right of [List].
 *	[Result] (o) -
 *		Unified with the [Count] last elements of [List].
 *
 * NOTES
 *	Calling right with a [Count] greater than the number of elements of [List]
 *	will unify [Result] with [List].
 *	Calling right with an empty list will unify [Result] with the empty list.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

 right(List, Count, List) :-
 	length(List, L), 
 	Count >= L.
 right([], _, []).
 right(List, Count, NewList) :-
 	length(List, L),
 	NewCount is L - Count,
 	drop(List, NewCount, NewList).

/******/





/****f* Modules.List/rotate, rotateleft, rotateright
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	rotate/3, rotateleft/3, rotateright/3
 *
 * SYNTAX
 *	rotate(List, Count, Result)
 *	rotateleft(List, Count, Result)
 *	rotateright(List, Count, Result)
 *
 * PURPOSE
 *	Rotates [List] by the number of times specified by [Count], and unifies
 *	[Result] with the new list. rotate/3 calls rotateright/3 if [Count] is
 *	negative, and rotateleft/3 if [Count] is positive.
 *	Rotating one time to the left the list [a,b,c,d] gives [b,c,d,a]. 
 *	Rotating it one time to the right gives [d,a,b,c].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to be rotated.
 *	[Count] (i) -
 *		Rotation count.
 *	[Result] (o) -
 *		Unified with the rotated list.
 *
 * NOTES
 *	Calling rotate with a rotation count of zero unifies [Result] with list.
 *	Calling rotate on the empty list unifies [Result] with an empty list.
 *	The number of rotations is adjusted to modulo length of	[List]. 	
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

rotate(List, 0, List).
rotate([], _, []).
rotate(List, Count, Result) :-
	Count < 0,
	NewCount is -Count,
	rotateright(List, NewCount, Result).
rotate(List, Count, Result) :-
	rotateleft(List, Count, Result).

rotate_adjust(List, Count, NewCount) :-		% For big values of rotate count,
	length(List, X),			% adjust it to modulo length of list
	NewCount is Count mod X.	
	
rotateleft(X, 0, X).
rotateleft([], _, []).
rotateleft(List, X, List) :-			% negative rotation counts not allowed
	X < 0, fail.
rotateleft(List, Count, Result) :-
	rotate_adjust(List, Count, X),
	dorotateleft(List, X, Result).
	
dorotateleft(X, 0, X).				% this does the real work
dorotateleft([], _, []).
dorotateleft([H|T], Count, Result) :-
	NewCount is Count - 1,
	append(T, [H], Newlist),
	dorotateleft(Newlist, NewCount, Result).

rotateright(X, 0, X).
rotateright([], _, []).
rotateright(List, X, List) :-
	X < 0, fail.
rotateright(List, Count, Result) :-
	rotate_adjust(List, Count, RealCount),
	length(List, ListLength),
	dorotateright(List, ListLength, RealCount, Result).
	
dorotateright(X, _, 0, X).
dorotateright(List, ListLength, Count, Result) :-
	TakeLeft is ListLength - 1,
	right(List, 1, LastElement),
	left(List, TakeLeft, LeftPart),
	append(LastElement, LeftPart, NewList),
	NewCount is Count - 1,
	dorotateright(NewList, ListLength, NewCount, Result).

/******/





/****f* Modules.List/set
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	set/2
 *
 * SYNTAX
 *	set(List, Result)
 *
 * PURPOSE
 *	Transforms [List] into a set, ie all duplicate elements are removed, and
 *	the list is sorted.
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to transform into a set.
 *	[Result] (o) -
 *		Resulting set.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

set([], []).
set(List, Result) :-
	sort(List, Sorted),
	removedups(Sorted, Result).

/******/





/****f* Modules.List/slice
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	slice/3
 *
 * SYNTAX
 *	slice(List, Count, NewList)
 *
 * PURPOSE
 *	slice/3 divides [List] in slices of [Count] elements and sets [NewList]
 *	to this list of slices.
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to process.
 *	[Count] (i) -
 *		Size of a slice.
 *	[NewList] (o) -
 *		List containing the slices.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

slice([], _, [[]]).			% Case of an empty list
slice(List, Count, NewList) :-		% Count must be greater than zero
	Count > 0,
	slice_perform(List, Count, NewList).
	
slice_perform(List, Count, [List]) :-	% Final case : length of list is less than Count
	length(List, Size),
	Size =< Count,
	!.

slice_perform(List, Count, Result) :-	% Normal case
	length(List, Size),
	left(List, Count, Left),
	drop(List, Count, Right),
	slice_perform(Right, Count, TempResult),
	append([Left], TempResult, Result).

/******/





/****f* Modules.List/strmin, strmax
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	strmin/3, strmax/3
 *
 * SYNTAX
 *	strmin(X, Y, R), strmax(X, Y,R)
 *
 * PURPOSE
 *	Unifies [R] with the minimum/maximum value of [X] and [Y].
 *
 * ARGUMENTS
 *	[X], [Y] (i) -
 *		Values to be compared.
 *	[R] (o) -
 *		Unified with the minimum/maximum value of [X] and [Y].
 *
 * NOTES
 *	The comparison is performed lexically.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

strmin(X, Y, Y) :-		% Minimum value of two elements
	Y @=< X, !.
strmin(X, Y, X).

strmax(X, Y, Y) :-		% Maximum value of two elements
	Y @>= X, !.
strmax(X, Y, X).

/******/





/****f* Modules.List/strmin_of_list, strmax_of_list
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	strmin_of_list/2, strmax_of_list/2
 *
 * SYNTAX
 *	strmin_of_list(List, Result), 
 *	strmax_of_list(List, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the minimum/maximum value that can be found in [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose minimum/maximum value is to be searched.
 *	[Result] (o) -
 *		Unified with the minimum/maximum value found in [List].
 *
 * NOTES
 *	Comparisons are performed lexically.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

strmin_of_list([X], X).
strmin_of_list([H|T], X) :-
	strmin_of_list(T, XBis), strmin(H, XBis, X).
	
strmax_of_list([X], X).
strmax_of_list([H|T], X) :-
	strmax_of_list(T, XBis), strmax(H, XBis, X).	

/******/





/****f* Modules.List/take
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	take/3
 *
 * SYNTAX
 *	take(List, Index, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the element of [List] taken at position [Index].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List where to look for element to be taken.
 *	[Index] (i) -
 *		Index of element to extract.
 *	[Result] (o) -
 *		Element at position [Index] taken from list [List].
 *
 * NOTES
 *	The predicate fails if [Index] is less than 1 or greater than length of [List].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
	
take(List, Index, []) :-
	Index < 1.
take(List, Index, []) :-
	length(List, ListLength),
	Index > ListLength.
take([Head|Tail], Index, Result) :-
	Index =:= 1,
	Result = Head, 
	!.	
take([Head|Tail], Index, Result) :-
	NewIndex is Index - 1,
	take(Tail, NewIndex, Result).	

/******/




/****f* Modules.List/union
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	union/3
 *
 * SYNTAX
 *	union(List1, List2, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the union of [List1] and [List2].
 *
 * ARGUMENTS
 *	[List1], [List2] (i) - 
 *		Lists on which to apply the union.
 *	[Result] (o) -
 *		Resulting list. Result is sorted, and all duplicate elements
 *		are removed.
 *
 * EXAMPLE
 *	union ([a,d,b,d], [b,c,d]) -> [a,b,c,d]
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

union(List1, List2, Result) :-
	append(List1, List2, X1),
	sort(X1, X2),
	removedups(X2, Result).

/******/




/****f* Modules.List/upper
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	upper/2
 *
 * SYNTAX
 *	upper(List, Result)
 *
 * PURPOSE
 *	Unifies [Result] with all the uppercased elements of [List].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose elements are to be uppercased.
 *	[Result] (o) -
 *		Resulting (uppercased) list.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

upper([], []).
upper([H|T], Result) :-
	atom_uplow(X, H),
	upper(T, TempResult),
	append([X], TempResult, Result).
upper(Atom, Result) :-
	atom_uplow(Result, Atom).

/******/




/****f* Modules.List/wholelength
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	wholelength/2
 *
 * SYNTAX
 *	wholelength( List, Result )
 *
 * PURPOSE
 *	Unifies [Result] with the number of elements in [List]. All sublists
 *	are expanded.
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List whose number of elements is to be counted.
 *	[Result] (o) -
 *		Unified to the number of elements of [List].
 *
 * NOTES
 *	The length of an empty list is 0. 
 *	The length of an atom is 1.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

wholelength([], L) :-
	L = 0.
wholelength([Head|Tail], L) :-
	wholelength(Head, L1),
	wholelength(Tail, L2),
	L is L1 + L2.
wholelength(X, L) :-
	atom(X),
	L = 1.

/******/

 :- end_body(list).