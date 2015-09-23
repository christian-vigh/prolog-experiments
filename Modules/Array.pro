/****h* Modules/Array
 ===============================================================================
 *
 * NAME
 *	Array - Array-manipulation predicates.
 *
 * FILE
 *	Modules/Array.pro
 *
 * CONTENTS
 *	Implements array as property bags.
 *
 * USES
 *	Properties.Pro, list.Pro
 *
 * AUTHOR
 *	Christian Vigh, July 2005.
 *
 ===============================================================================
 ******/

:- module(array).

:- 	import(properties).
:-	import(list).

:-	export(array/2).
:-	export(array/3).
:-	export(base/2).
:-	export(delete/1).
:-	export(firstthat/3).
:-	export(firstthat/4).
:-	export(grow/2).
:-	export(grow/3).
:-	export(growto/2).
:-	export(growto/3).
:- 	export(lastthat/3).
:-	export(lastthat/4).
:-	export(merge/3).
:-	export(print/1).
:-	export(printall/1).
:-	export(shrink/2).
:-	export(shrinkto/2).
:-	export(size/2).
:-	export(slice/4).
:-	export(sliceto/4).
:-	export(sortarray/1).

:-	export('#'/2).		% Array indexing (used in ':=')
:-	export(':='/2).		% Assignment
:-	export('=#='/2).	% Test for equality
:-	export('<#>'/2).	% Test for inequality

:- end_module(array).
 

:- body(array).


:- op(900, xfx, #).
:- op(910, xfx, :=).
:- op(900, xfx, =#=).
:- op(900, xfx, <#>).


/****f* Modules.Array/array
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	array/2, array/3
 *
 * SYNTAX
 * 	array(Name, List)
 *	array(Name, Length, InitialValue)
 *
 * PURPOSE
 *	If [List] is a bound variable, array/2 creates the array called [Name].
 *	Otherwise, array/2 unifies [List] with the list of elements in array
 *	[Name]. 
 *	The array/3 predicate creates an array [Name] of size [Length], all
 *	elements being initialized with [InitialValue].
 *
 * ARGUMENTS
 *	* [Name] (i) -
 *		Name of the array to create or whose elements are to be retrieved.
 *	* [List] (i,o) -
 *		If bound : list of elements used to initialize the array.
 *		Otherwise : list that is unified with the contents of array [Name].
 *	* [Length] (i) -
 *		Length of array to create.
 *	* [InitialValue] (o) -
 *		Initial value of array elements, in the array/3 version
 *		of the predicate.
 *
 * NOTES
 *	Arrays are created as property bags ; in order to preserve the
 *	name space, should other kind of objects be managed through
 *	property bags, the string "arraybag:" is prepended to [Name].
 *		
 *	The property bag for an array holds the following properties:
 *	- base 
 *		base of the array, 1 by default (not used in the current
 *		implementation).
 *	- length
 *		Length of the array.
 *	- element:x 
 *		Holds the element at index "x".
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
 
%
% Version where [List] is not bound :
%	retrieve the elements in array [Name].
%
array(Name, List) :-
	var(List),
	property_get(arraybag:Name, length, Length),
	array_get_elements(arraybag:Name, 0, Length, List),
	!.
	
%
% Version where [List] is bound :
%	Creates an array [Name] and initialize it with [List].
%
array(Name, List) :-
	length(List, L),
	array_create(arraybag:Name, L, 1),
	array_set_elements(arraybag:Name, List, 1).

%
% Version where length of array and initial values are provided.
%
array(Name, Length, InitialValue) :-
	array_create(arraybag:Name, Length, 1),
	duplicateatom(InitialValue, Length, A),
	array_set_elements(arraybag:Name, A, 1).
/******/
	


/****f* Modules.Array/base
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	base/2
 *
 * SYNTAX
 *	base(Name, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the start index of array [Name].
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the array.
 *	[Result] (o) -
 *		Base (starting index) of the array.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

base(Name, Result) :-
	property_get(arraybag:Name, base, Result).

/******/



/****f* Modules.Array/delete
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	delete/1
 *
 * SYNTAX
 *	delete(Name).
 *
 * PURPOSE
 *	Deletes the array [Name].
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the array to be deleted.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
delete(Name) :-
	property_bag_delete(arraybag:Name).

/******/


/****f* Modules.Array/firstthat
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	firstthat/3, firstthat/4
 *
 * SYNTAX
 *	firstthat(Name, Value, Result)
 * 	firstthat(Name, Start, Value, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the index of the first element of array [Name] that 
 *	is equal to [Value].
 *	The firstthat/3 version starts the search with the first element of [Name],
 *	while firstthat/4 starts its search at position [Index].
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the array to be searched.
 *	[Start] (i) -
 *		Starting position for the search.
 *	[Value] (i) -
 *		Value to search for.
 *	[Result] (o) -
 *		Index of the searched element.
 *
 * NOTES
 *	The predicate fails if [Start] or [Index] is invalid, or if [Value] cannot
 *	be found in array [Name].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
firstthat(Name, Value, Result) :-
	firstthat(Name, 1, Value, Result).
	
firstthat(Name, Start, Value, Result) :-
	size(Name, L),
	Start > 0,
	Start =< L,
	firstthat_process(Name, L, Start, Value, Result).

firstthat_process(_, Length, Start, _, _) :-
	Start > Length,
	!, fail.

%
% Internal helper functions
%
firstthat_process(Name, Length, Start, Value, Result) :-
	X := Name#Start,
	Value = X,
	Result is Start.
firstthat_process(Name, Length, Start, Value, Result) :-
	NewStart is Start + 1,
	firstthat_process(Name, Length, NewStart, Value, Result).

/******/



/****f* Modules.Array/grow
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	grow/2, grow/3
 *
 * SYNTAX
 *	grow(Name, Count)
 * 	grow(Name, Count, InitialValue)
 *
 * PURPOSE
 * 	Adds [Count] elements to array [Name].
 *	grow/3 initializes any new element to [InitialValue], whereas
 *	grow/2 initializes any new element to "".
 *
 * ARGUMENTS
 * 	[Name] (i) -
 *		Name of the array to expand.
 *	[Count] (i) -
 *		Number of elements to add to array [Name].
 *	[InitialValue] (i) -
 *		Initial value for new elements.
 *
 * NOTES
 *	grow fails if [Index] is less than 0, and does nothing if [Index] is 0.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
	
grow(Name, Count) :-
	grow(Name, Count, '').

grow(_, 0, _).				% Do nothing if Count is zero
grow(_, Count, _) :-			% Fails if Count is negative
	Count < 0,
	!, fail.	

grow(Name, Count, InitialValue) :-	% Does the real work
	% Create a list with the initial value repeat Count times
	duplicateatom(InitialValue, Count, X),
	% Get the size of array Name
	size(Name, L),
	% Set the new length of the array
	NewLength is L+ Count,
	property_set(arraybag:Name, length, NewLength),
	% Append new list to existing array
	Start is L + 1,
	array_set_elements(arraybag:Name, X, Start).
/******/	

	
	
/****f* Modules.Array/growto
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	growto/2, growto/3
 *
 * SYNTAX
 *	grow(Name, Upto)
 *	grow(Name, Upto, InitialValue)
 *
 * PURPOSE
 *	Expands array [Name] so that it contains [Upto] elements.
 *	growto/3 initializes any new element to [InitialValue], whereas growto/2 
 *	initializes any new element to "".
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the array to expand.
 *	[Upto] (i) -
 *		New array size for [Name].
 *	[InitialValue] (i) -
 *		Initial value for new elements.
 *
 * NOTES
 *	growto fails if [Index] is less than 1, and calls shrinkto if [Index] is 
 *	less than length of array [Name].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */


growto(Name, Index) :-
	growto(Name, Index, '').

growto(_, Index, _) :-			% Fails if Count is negative
	Index < 1,
	!, fail.	

growto(Name, Index, InitialValue) :-	% Index < array size : call shrinkto
	size(Name, S),
	Index < S,
	shrinkto(Name, Index, InitialValue),
	!.
growto(Name, Index, InitialValue) :-	% Normal case
	size(Name, L),
	Count is Index - L,
	grow(Name, Count, InitialValue).
/******/	



/****f* Modules.Array/lastthat
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	lastthat/3, lastthat/4
 *
 * SYNTAX
 *	lastthat(Name, Value, Result)
 *	lastthat(Name, Start, Value, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the index of the last element of array [Name] that
 *	is equal to [Value].
 *	The lastthat/3 version starts the search with the last element of
 *	[Name], while lastthat/4 starts its search at position [Index].
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the array to be searched.
 *	[Start] (i) -
 *		Starting position for the search.
 *	[Value] (i) -
 *		Value to search for.
 *	[Result] (o) -
 *		Index of the searched element.
 *
 * NOTES
 *	The predicate fails if [Start] or [Index] is invalid, or if [Value] cannot
 *	be found in array [Name].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

lastthat(Name, Value, Result) :-
	size(Name, L),
	lastthat(Name, L, Value, Result).
	
lastthat(Name, Start, Value, Result) :-
	size(Name, L),
	Start > 0,
	Start =< L,
	lastthat_process(Name, L, Start, Value, Result).

lastthat_process(_, Length, 0, _, _) :-
	!, fail.

lastthat_process(Name, Length, Start, Value, Result) :-
	X := Name#Start,
	Value = X,
	Result is Start.

%
% Internal helper.
%
lastthat_process(Name, Length, Start, Value, Result) :-
	NewStart is Start - 1,
	lastthat_process(Name, Length, NewStart, Value, Result).

/******/



/****f* Modules.Array/merge
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	merge/3
 *
 * SYNTAX
 *	merge(A, B, Result)
 *
 * PURPOSE
 *	Merges the contents of arrays [A] and [B], creates a third array named 
 *	[Result], containing the result of the merge.
 *
 * ARGUMENTS
 *	[A], [B] (i) -
 *		Arrays to merge together.
 *	[Result] (i) -
 *		Name of the resulting array.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

merge(A, B, X) :-
	array(A, AValues),
	array(B, BValues),
	flatten([AValues, BValues], NewList),
	array(X, NewList).

/******/


	
/****f* Modules.Array/print, printall
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	print/1, printall/1
 *
 * SYNTAX
 *	print(Name)
 *	printall(Name)
 *
 * PURPOSE
 *	print/1 prints the elements of array [Name], as can do write/1 with a list.
 *	printall/1 prints all the attributes and values of array [Name].
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the array to be printed.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

print(Name) :-
	array(Name, List),
	write(List).

printall(Name) :-
	write('Array ['), write(Name), write('] :'), nl,
	property_get(arraybag:Name, base, B),
		write('Base     : '), write(B), nl,
	property_get(arraybag:Name, length, L),
		write('Length   : '), write(L), nl,
        write('Elements : '), 
        print(Name), nl.

/******/




/****f* Modules.Array/slice, sliceto
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	slice/4, sliceto/4
 *
 * SYNTAX
 * 	slice(Name, Start, Count, ResultName)
 *	sliceto(Name, Start, End, ResultName)
 *
 * PURPOSE
 *	Builds an array [ResultName], based on a slice extracted from array [Name].
 *	slice/4 takes a slice of [Count] elements, starting from position [Start].
 *	sliceto/4 takes a slice from positions [Start] to [End].
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the array from which to extract the slice.
 *	[Start] (i) -
 *		Starting position.
 *	[Count] (i) -
 *		Number of elements in the slice.
 *	[End] (i) -
 *		Ending position of the slice.
 *	[ResultName] (i) -
 *		Name of the resulting array.
 *
 * NOTES
 *	if [Count] or [End] is uninstantiated, then the slice will be composed of
 *	all the elements of the array [Name] starting at position [Start], until
 *	the last element of the array.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

sliceto(Name, Start, End, Result) :-
	% Get array size
	size(Name, Length),
	% Start must be positive
	Start > 0,
	% Unify End with length of array if End is a variable
	sliceto_adjust(Length, Start, End),
	% Check that End is not greater than array size
	End > 0, End =< Length,
	% Compute total number of elements in the slice
	Count is End - Start + 1,
	% Perform the slicing
	slice(Name, Start, Count, Result).	

%
% sliceto_adjust -
%	Sets End to the size of the array if End is a variable.
%
sliceto_adjust(Length, Start, End) :-
	var(End),
	End is Length,
	!.
sliceto_adjust(Length, Start, End).


slice(Name, Start, Count, Result) :-
	% Get the size of the array
	size(Name, Length),
	% Check that Start is a valid number
	Start > 0, 
	% Set Count to the number of remaining elements if Count is 
	% a variable
	slice_adjust(Length, Start, Count),
	% Check that specified count is valid
	Count > 0,
	% Compute the ending position of the slice in array Name
	End is Start + Count - 1,
	% Check that the ending position is valid
	End > 0, End =< Length,
	% Get elements of the array
	array(Name, Elements),
	% Extract the slice
	middle(Elements, Start, Count, Slice),
	% and build the array Result
	array(Result, Slice).

%
% slice_adjust -
%	Sets Count to the number of remaining elements of the array
%	if Count is a variable.
%
slice_adjust(Length, Start, Count) :-
	var(Count),
	Count is Length - Start + 1,
	!.

slice_adjust(Length, Start, Count).

/******/



/****f* Modules.Array/shrink
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	shrink/2
 *
 * SYNTAX
 *	shrink(Name, Count).
 *
 * PURPOSE
 *	Removes [Count] elements from array [Name].
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the array to expand.
 *	[Count] (i) -
 *		Number of elements to remove from array [Name].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

shrink(Name, Count) :-
	shrink(Name, Count, '').

shrink(_, 0, _).			% Do nothing if Count is zero
shrink(Name, Count, _) :-		% Fails if Count is negative
	Count < 0,
	!.	
shrink(Name, Count, _) :-		% Fails if Count is > length(Array)
	size(Name, L),
	Count >= L,
	!.
	
shrink(Name, Count, InitialValue) :-
	% Get the size of array Name
	size(Name, L),
	% Set the new length of the array
	NewLength is L - Count,
	property_set(arraybag:Name, length, NewLength),
	% Append new list to existing array
	Start is L + 1,
	array_delete_elements(arraybag:Name, Start, Count).

/******/


	

/****f* Modules.Array/shrinkto
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	shrinkto/2, shrinkto/3
 *
 * SYNTAX
 *	shrink(Name, Upto)
 *	shrink(Name, Upto, InitialValue)
 *
 * PURPOSE
 *	Expands array [Name] so that it contains [Upto] elements.
 *	shrinkto/3 initializes any new element to [InitialValue], whereas
 *	shrinkto/2 initializes any new element to "".
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the array to expand.
 *	[Upto] (i) -
 *		New array size for [Name].
 *	[InitialValue] (i) -
 *		Initial value for new elements.
 *
 * NOTES
 *	shrinkto fails if [Index] is less than 1, and calls shrinkto if [Index]
 *	is less than length of array [Name].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
	
shrinkto(Name, Index) :-
	shrinkto(Name, Index, '').

shrinkto(_, Index, _) :-		% Fails if Count is negative
	Index < 1,
	!.	
shrinkto(Name, Index, InitialValue) :-	% Index >= array size : call shrinkto
	size(Name, S),
	Index >= S,
	!.
shrinkto(Name, Index, InitialValue) :-	% Normal case
	size(Name, L),
	Count is L - Index,
	shrink(Name, Count, InitialValue).

/******/


	

/****f* Modules.Array/size
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	size/2
 *
 * SYNTAX
 *	size(Name, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the length of array [Name].
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the array.
 *	[Result] (o) -
 *		Length of the array.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

size(Name, Result) :-
	property_get(arraybag:Name, length, Result).

/******/


	

/****f* Modules.Array/sortarray
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	sortarray/1
 *
 * SYNTAX
 *	sortarray(Name)
 *
 * PURPOSE
 *	Sorts the elements of array [Name].
 *
 * ARGUMENTS
 *	[Name] (i,o) -
 *		Array to sort.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
	
sortarray(Name) :-
	array(Name, Values),
	sort(Values, SortedValues),
	delete(Name),
	array(Name, SortedValues).
/******/



/****f* Modules.Array/operator :=
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	:=/2
 *
 * SYNTAX
 *	X := Y
 *
 * PURPOSE
 *	The ":=" operator is used to perform assignments back and forth between
 *	atoms and elements of an array.
 *	[X] and [Y] can either be an atom, a list or an expression of the form
 *	"a#x" where "a" is an array name, and "x" the index of an element in 
 *	array "a". This allows for assignments such as :
 *		
 *	X := myarray#1. 	% Unifies X with element 1 of array "myarray"
 *	myarray#2 := hello.	% Sets element 2 of array "myarray" to "hello"
 *	a#1 := b#2.		% Set element 1 of array "a" to element 2
 *				% of array "b"
 *
 * ARGUMENTS
 *	An atom, list or expression of the form : array:index.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

%
% X := array:index.
%
X := Name # Index :-
	% X must be a variable
	var(X),
	% preprocess Index
	array_preprocess_index(Name, Index, NewIndex),
	% Check that Index is in the allowed range
	array_inbounds(arraybag:Name, NewIndex),
	% Get element
	property_get(arraybag:Name, element:NewIndex, Y),
	% Unify
	X = Y.

%
% array1:index1 := array2:index2.
%
Name1 # Index1 := Name2 # Index2 :-
	array_preprocess_index(Name, Index1, NewIndex1),
	array_inbounds(arraybag:Name1, NewIndex1),
	array_preprocess_index(Name, Index2, NewIndex2),
	array_inbounds(arraybag:Name2, NewIndex2),
	property_get(arraybag:Name2, element:NewIndex2, X),
	property_set(arraybag:Name1, element:NewIndex1, X).

%
% array:index = X.
%
Name # Index := X :-
	array_preprocess_index(Name, Index, NewIndex),
	array_inbounds(arraybag:Name, NewIndex),
	property_set(arraybag:Name, element:NewIndex, X).

/******/




/****f* Modules.Array/operator =#=, operator <#>
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	=#=/2, <#>/2
 *
 * SYNTAX
 *	A =#= B
 *	A <#> B
 *
 * PURPOSE
 *	The "=#=" operator succeeds if arrays A and B are equal.
 *	The "<#>" operator succeeds if arrays A and B are different.
 *
 * ARGUMENTS
 *	[A], [B] (i) -
 *		Arrays to compare.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

A =#= B :-
	array(A, AE),
	array(B, BE),
	AE = BE.
	
A <#> B :-
	array(A, AE),
	array(B, BE),
	not(AE = BE).

/******/
	
	
/****************************************************************************
 
	Internal helper predicates.
	 
 ****************************************************************************/

%
% array_create(Name, Length, Base) -
%	Creates the array [Name], of size [Length], having a base index of [Base]
%
array_create(Name, Length, Base) :-
	property_bag(Name),
	property_set(Name, base, Base),
	property_set(Name, length, Length).

%
% array_set_elements(Name, Elements, Index) -
%	Adds a property for each element of list [Elements] in the
%	[Name] property bag. Each property has a name of the form
%	element:idx, where [idx] is the index of the element in
%	the array.
%	[Index] is only an accumulator that starts from zero and is
%	incremented at each level of recursion.
%
array_set_elements(_, [], _).
array_set_elements(Name, [H|T], Index) :-
	property_set(Name, element:Index, H),
	NewIndex is Index + 1,
	array_set_elements(Name, T, NewIndex).

%
% array_get_elements(Name, Index, Length, Result) -
%	Unifies [Result] to the list of elements contained in the array
%	[Name]. 
%	[Index] is just an accumulator that starts from zero and
%	is increment at each recursion level.
%	[Length] is here to provide for a stop condition, without
%	having to query each time for the "length" property of the array.
%
array_get_elements(_, L, L, []).
array_get_elements(Name, Index, Length, X) :-
	NewIndex is Index + 1,
	property_get(Name, element:NewIndex, Element),
	array_get_elements(Name, NewIndex, Length, NewX),
	append([Element], NewX, X).

%
% array_delete_elements(Name, Start, Count) -
%	deletes [Count] elements from array [Name], starting at position
%	[Count].
%
array_delete_elements(_, _, 0).
array_delete_elements(Name, Start, Count) :-
	property_delete(Name, element:Start),
	NewStart is Start + 1,
	NewCount is Count -1,
	array_delete_elements(Name, NewStart, NewCount).

%
% array_inbound(Name, Index) -
%	Checks if [Index] is within the allowed range of indexes for
%	array [Name] (the allowed range of indexes is 1..length(Name)).
%
array_inbounds(Name, Index) :-
	not(nvar(Name)), not(var(Index)),
	Index > 0,
	property_get(Name, length, L),
	Index =< L.
array_inbounds(_, _) :-		% Cleans backtracking and fails if incorrect index
	!, fail.

%
% array_preprocess_index :
% 	Preprocess an array index. Replaces :
%	."first" by 1
%	."last" by length of array
%
array_preprocess_index(_, first, 1) :- !.
array_preprocess_index(Name, last, X) :-
	size(Name, X), !.
array_preprocess_index(Name, length, X) :-
	array_preprocess_index(Name, last, X).
array_preprocess_index(Name, size, X) :-
	array_preprocess_index(Name, last, X).
array_preprocess_index(_, X, X).

 :- end_body(array).