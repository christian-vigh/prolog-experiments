/****h* Modules.SparseArray
 ===============================================================================
 *
 * NAME
 *	SparseArray - Sparse Array-manipulation predicates.
 *
 * FILE
 *	Modules/SparseArray.pro
 *
 * CONTENTS
 *	Implements multi-dimensional sparse arrays.
 *
 * USES
 *	Utilities.Pro, list.Pro
 *
 * AUTHOR
 *	Christian Vigh, February 2007.
 *
 ===============================================================================
 ******/


:- module(sparsearray).

:- 	import(utilities).
:- 	import(list).

:- 	dynamic( array$value/3 ).
:- 	dynamic( array$options/2 ).

:- 	sorted( array$value/3 ).

:-	export(copy/2).
:-	export(delete/1).
:-	export(dimension/3).
:-	export(dimensions/2).
:-	export(get/3).
:- 	export(is_array_/1).
:-	export(lbound/3).
:-	export(option/3).
:-	export(portray/1).
:-	export(reset/1).
:-	export(set/3).
:-	export(sizeof/3).
:-	export(ubound/3).

:- end_module(sparsearray).


:- body(sparsearray).


/****f* Modules.SparseArray/Exceptions
 -------------------------------------------------------------------------------
 *
 * ERROR CODES
 *	Exceptions that can be thrown by sparse array manipulation predicates.
 *
 * DESCRIPTION
 *	ERR_ARRAY_ALREADY_DEFINED -
 *		An attempt to create an array has failed because the array already
 *		exist ; or the copy/2 predicate has been used specifying an existing
 *		array as the destination.
 *
 *	ERR_ARRAY_UNDEFINED_ARRAY -
 *		The referenced array does not exist.
 *	
 *	ERR_ARRAY_INVALID_BOUNDARY -
 *		The boundary specified as one of the dimensions of the array is
 *		incorrect (for examples, lower value is greater than upper value).
 *
 *	ERR_ARRAY_INVALID_DIMENSION_SIZE -
 *		The boundary range specified as a size is incorrect (either size
 *		is less than zero or the keyword for specifying infinity is
 *		incorrect).
 *
 *	ERR_ARRAY_INVALID_BOUNDARY_KEYWORD -
 *		The keyword used to specify an infinite value in an array dimension
 *		is incorrect.
 *
 *	ERR_ARRAY_INVALID_DIMENSION -
 *		The array does not contain the specified dimension.
 *
 *	ERR_ARRAY_INVALID_DIMENSION_COUNT -
 *		An attempt has been made to reference an array element, but the
 *		given number of dimensions is incorrect.
 *
 *	ERR_ARRAY_INDEX_OUT_OF_RANGE -
 *		One of the indexes specified in the get/3 predicate is out of range.
 *
 *	ERR_ARRAY_UNDEFINED_ELEMENT -
 *		The get/3 property has been used on an undefined element.
 *		This behavior is controlled by the undefined_get option. When set
 *		to 1 (the default), the get/3 predicate returns an empty atom if the
 *		specified element does not exist.
 *
 * NOTES
 *	Exceptions are thrown only if the use_exceptions property is set to 1,
 *	which is the default (see the option/3 predicate).
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

# define	ERR_ARRAY_ALREADY_DEFINED			10000
# define	ERR_ARRAY_UNDEFINED_ARRAY			10001
# define	ERR_ARRAY_INVALID_BOUNDARY			10002
# define	ERR_ARRAY_INVALID_DIMENSION_SIZE		10003
# define	ERR_ARRAY_INVALID_BOUNDARY_KEYWORD		10004
# define	ERR_ARRAY_INVALID_DIMENSION			10005
# define	ERR_ARRAY_INVALID_DIMENSION_COUNT		10006
# define	ERR_ARRAY_INDEX_OUT_OF_RANGE			10007
# define	ERR_ARRAY_UNDEFINED_ELEMENT			10008

/******/


# define 	ARRAY_PREDICATE(Name,Bounds)			array$array(Name,Bounds)
# define 	ARRAY_VALUE_PREDICATE(Name,Index,Value)		array$value(Name,Index,Value)
# define	ARRAY_OPTIONS_PREDICATE(Name,Value)		array$options(Name,Value)






/****f* Module.SparseArray/option
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	option/3
 *
 * SYNTAX
 *	option(Name, 
 *
 * PURPOSE
 *
 * ARGUMENTS
 *
 * NOTES
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

%***************************  General Options *********************************

% general options
% use_exceptions : 
%	1 if exceptions are to be used, 0 if normal prolog goal satisfaction
%	mechanism is to be used.
% base :
%	For array boundaries which are defined by giving a size instead of an
%	index range, the 'base' option determines if the first element has index
%	0 or one.
% undefined_get :
%	If set to 1, retrieving a not-yet defined array element is authorized.
%
option(use_exceptions, Value) :-
	( Value == 0 ; Value == 1 ),
	retractall( ARRAY_OPTIONS_PREDICATE(use_exceptions, _ ) ),
	assert( ARRAY_OPTIONS_PREDICATE(use_exceptions, Value) ).

option(base, Value) :-
	( Value == 0 ; Value == 1 ),
	retractall( ARRAY_OPTIONS_PREDICATE(base, _ ) ),
	assert( ARRAY_OPTIONS_PREDICATE(base, Value) ).

option(undefined_get, Value) :-
	( Value == 0 ; Value == 1 ),
	retractall( ARRAY_OPTIONS_PREDICATE(undefined_get, _ ) ),
	assert( ARRAY_OPTIONS_PREDICATE(undefined_get, Value) ).


% Default values
ARRAY_OPTIONS_PREDICATE(use_exceptions, 1).
ARRAY_OPTIONS_PREDICATE(base          , 1).
ARRAY_OPTIONS_PREDICATE(undefined_get , 1).



%***************************  Array predicates  *******************************

% is_array : satisfies if Name is an array
is_array(Name) :-
	ARRAY_PREDICATE(Name, _).

% dimensions : returns the number of dimensions for the specified array
dimensions(Name, Count) :-
	array_must_be_defined(Name),
	ARRAY_PREDICATE(Name, List),	
	list:length(List, Count).

% dimension : returns the lower and upper bound of the specified dimension
dimension(Name, Index, Value) :-
	array_must_be_defined(Name),
	dimension_must_be_valid(Name, Index),
	ARRAY_PREDICATE(Name, List),
	list:take(List, Index, Value).
	
% lbound, hbound
lbound(Name, Index, Low) :-
	dimension(Name, Index, [Low, _]).
hbound(Name, Index, High) :-
	dimension(Name, Index, [_, High]).

% Size of a dimension
sizeof(Name, Index, Size) :-
	dimension(Name, Index, [Low, High]),
	Size is High - Low + 1.
	
	
% create : creates an array
create(Name, Boundaries) :-
	array_must_be_undefined(Name),
	array_parse_boundaries(Boundaries, Bounds),
	assert( ARRAY_PREDICATE(Name, Bounds) ).


% delete : deletes an array
delete(Name) :-
	array_must_be_defined(Name),
	retractall( ARRAY_PREDICATE( Name, _ ) ),
	retractall( ARRAY_VALUE_PREDICATE( Name, _, _ ) ).

% copy : copy the specified array to a new name
copy(From, To) :-
	array_must_be_defined(From),
	array_must_be_undefined(To),
	ARRAY_PREDICATE(From, Boundaries),
	assert( ARRAY_PREDICATE(To, Boundaries) ),
	not( copy_values(From, To) ).
	
copy_values(From, To) :-
	ARRAY_VALUE_PREDICATE(From, Index, Value),
	assert( ARRAY_VALUE_PREDICATE(To, Index, Value) ),
	fail.


% reset the values in the specified array
reset(Array) :-
	array_must_be_defined(Array),
	retractall( ARRAY_VALUE_PREDICATE(Array, _, _) ).
	

% get : gets a value
get(Name, Index, Value) :-
	array_must_be_defined(Name),
	index_must_be_valid(Name, Index, NewIndex),
	do_get(Name, NewIndex, Value).
	
do_get(Name, Index, Value) :-
	ARRAY_VALUE_PREDICATE(Name, Index, Value).
do_get(Name, Index, '') :-
	ARRAY_OPTIONS_PREDICATE(undefined_get, Undefined),
	Undefined == 1.
do_get(Name, Index, _) :-
	array_throw( array_error( ERR_ARRAY_UNDEFINED_ELEMENT,
		'Array element does not have a value.', [Name, Index] ) ).


% set/2 : satisfies if the specified array element is defined
%set(Name, Index) :-
%	array_must_be_defined(Name),
%	index_must_be_valid(Name, Index),
%	ARRAY_VALUE_PREDICATE(Name, Index, _).
	
set(Name, Index, Value) :-
	array_must_be_defined(Name),
	index_must_be_valid(Name, Index, NewIndex),
	retractall( ARRAY_VALUE_PREDICATE(Name, NewIndex, _) ),
	assert( ARRAY_VALUE_PREDICATE(Name, NewIndex, Value) ).
		

% Prints the contents of an array 
portray(Name) :-
	array_must_be_defined(Name),
	print 'Array ' + Name + ' ',
	ARRAY_PREDICATE(Name, Boundaries),
	portray_boundaries(Boundaries),
	print ': ' + nl,
	findall([Index, Value], ARRAY_VALUE_PREDICATE(Name, Index, Value), ValueList),
	portray_elements(Name, ValueList).
	
portray_boundaries([]).
portray_boundaries( [ [Low, High] | Tail ] ) :-
	print '[' + Low + '..' + High + '] ',
	portray_boundaries(Tail).

portray_elements(Name, [ [Index, Value] | Tail ]) :-
	print tab + '[',
	portray_index(Index),
	print '] ' + Value + nl,
	portray_elements(Name, Tail).
portray_elements( _, [] ).
	
	
portray_index( [Head|Tail] ) :-
	print right(Head, 4),
	portray_index_optional_comma( Tail ),
	portray_index( Tail ).
portray_index( [] ).

portray_index_optional_comma( [] ).
portray_index_optional_comma( _ ) :-
	print ', '.


%***************************  Exceptions and checkings  ***********************

% array_throw : throws an exception if the array option 'use_exceptions' is 
% either undefined or non-zero.
array_throw( Exception ) :-
	( 
	  	not( ARRAY_OPTIONS_PREDICATE( use_exceptions, X ) ) ;
	  	X \== 0
	 ),
	throw( Exception ).
array_throw( Exception ) :- fail, !.




% Checks that the specified array does not already exist
array_must_be_undefined(Name) :-
	not( is_array(Name) ), !.
array_must_be_undefined(Name) :-
	array_throw( array_error(ERR_ARRAY_ALREADY_DEFINED, 'Array is already defined', Name) ).


% Checks that the specified array already exist
array_must_be_defined(Name) :-
	is_array(Name), !.
array_must_be_defined(Name) :-
	array_throw( array_error(ERR_ARRAY_UNDEFINED_ARRAY, 'Array does not exist', Name) ).

% Checks that the specified dimension index is valid
dimension_must_be_valid(Name, Index) :-
	Index > 0,
	ARRAY_PREDICATE(Name, List),
	list:length(List, Count),
	Index =< Count, !.
dimension_must_be_valid(Name, Index) :-
	array_throw( array_error(ERR_ARRAY_INVALID_DIMENSION, 
		'Invalid dimension for the specified array', [Name, Index]) ).

% Checks that the specified index list is valid
index_must_be_valid(Name, Index, Index) :-
	is_list(Index),
	ARRAY_PREDICATE(Name, Dimensions),
	list:length(Dimensions, DimensionLength),
	list:length(Index, IndexLength),
	(
		IndexLength == DimensionLength ;
		array_throw( array_error( ERR_ARRAY_INVALID_DIMENSION_COUNT,
			'Dimension count in array and specified index mismatch.',
			[Name, IndexLength, DimensionLength]) )
	 ),
	 dimension_index_must_be_valid( Name, Dimensions, Index ).
	 
index_must_be_valid(Name, Index, [Index]) :-
	ARRAY_PREDICATE(Name, Dimensions),
	list:length(Dimensions, DimensionLength),
	(
		DimensionLength == 1 ;
		array_throw( array_error( ERR_ARRAY_INVALID_DIMENSION_COUNT,
			'Dimension count in array and specified index mismatch.',
			[Name, 1, DimensionLength]) )
	 ),
	 dimension_index_must_be_valid( Name, Dimensions, [Index] ).


% Check that the index specification is within dimension boundaries
dimension_index_must_be_valid( _, [], [] ).

dimension_index_must_be_valid(Name, [ [Low, High] | DTail ], [Index | ITail] ) :-
	Index >= Low, Index =< High,
	dimension_index_must_be_valid(Name, DTail, ITail).

dimension_index_must_be_valid(Name, [ Range | _ ], [Index | _] ) :-
	array_throw( array_error( ERR_ARRAY_INDEX_OUT_OF_RANGE,
		'Index out of range.', [Name, Index, Range] ) ), !.


%***************************  Utility functions *******************************

% Parses the boundary list specified in the create/2 predicate.
array_parse_boundaries( [], [] ).
array_parse_boundaries( [Head|Tail], [X|RTail] ) :-
	array_parse_boundary( Head, X ),
	array_parse_boundaries( Tail, RTail ).
array_parse_boundaries(Range, [X]) :-
	array_parse_boundary( Range, X ).

array_parse_boundary( Low-High, [X, Y] ) :-
	array_get_low_boundary(Low, X),
	array_get_high_boundary(High, Y),
	( 
		X =< Y ; 
		array_throw( array_exception(ERR_ARRAY_INVALID_BOUNDARY, 'Invalid array boundary', [X, Y]) )
	  ), !.

array_parse_boundary(Size, [X, Y] ) :-
	ARRAY_OPTIONS_PREDICATE(base, Base),
	array_get_high_boundary(Size, NewSize),
	Low is 0, High is NewSize - 1,
	X is Low + Base, Y is High + Base, !.
array_parse_boundary(Size, _) :-
	Size =< 0,
	array_throw( array_exception(ERR_ARRAY_INVALID_DIMENSION_SIZE, 'Invalid dimension size', Size) ), !.
	

array_get_low_boundary(A, A) :-
	is_number(A).
array_get_low_boundary(Keyword, -inf) :-
	is_member(Keyword, 
		[unlimited, nolimit, infinite, infinity, inf, -infinite, -infinity, -inf]).
array_get_low_boundary( Keyword, _ ) :-
	array_throw( array_exception( ERR_ARRAY_INVALID_BOUNDARY_KEYWORD, 
		'Invalid keyword in boundary definition', Keyword ) ).
	

array_get_high_boundary(A, A) :-
	is_number(A).
array_get_high_boundary(Keyword, +inf) :-
	is_member(Keyword, 
		[unlimited, nolimit, infinite, infinity, inf, +infinite, +infinity, +inf]).
array_get_high_boundary( Keyword, _ ) :-
	array_throw( array_exception( ERR_ARRAY_INVALID_BOUNDARY_KEYWORD, 
		'Invalid keyword in boundary definition', Keyword ) ).

	

:- end_module(sparsearray).