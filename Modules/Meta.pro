/****h* Modules/Meta
 ===============================================================================
 *
 * NAME
 *	Meta - Prolog meta-predicates.
 *
 * FILE
 *	Modules/Meta.pro
 *
 * CONTENTS
 *	Implements Prolog meta-predicates.
 *
 * USES
 *	list.Pro
 *
 * AUTHOR
 *	Christian Vigh, July 2005.
 *
 ===============================================================================
 ******/

:- module(meta).
:- 	import(list).

:-	export([apply/3]).
:-	export([apply/4]).
:- end_module(meta).

:- body(meta).






/****f* Modules.Meta/apply
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	apply/3, apply/4
 *
 * SYNTAX
 *	apply(List, Operation, Result)
 * 	apply(List, Operation, Result, OpType)
 *
 * PURPOSE
 *	apply applies the specified [Operation] to every element of [List], and 
 *	unifies [Result] with the list of elements which have been applied the 
 *	[Operation].
 *		
 *	[Operation] can be either a string (enclosed with backquotes) or an atom
 *	(possibly enclosed with single quotes).
 *		
 *	When [Type] is 'expression' (or 'exp' or 'e'), the [Operation] can be any 
 *	valid mathematical expression, for example :
 *		
 *		apply([1,2,3], 'X * 2', Result).
 *		
 *	or even :
 *
 *		apply([1,2,3], 'X * sin(2 * X)', Result).
 *			
 *	(the first example unifies [R] with every element of [1,2,3] multiplied 
 *	by 2, ie : [2,4,6]).
 *		
 *	Within [Operation], the element of [List] currently processed must be 
 *	referenced as the variable X.
 *		
 *	The other accepted value for [Type] can be 'operation' (or 'op' or 'o').
 *	In that case, [Operation] is not a mathematical expression but a list of 
 *	predicates such as in the following example :
 *		
 *		apply([a,b,c], 'atom_uplow(R,X)', Result, operation)
 *		
 *	which puts in [Result] all the elements of [List] converted to upper case. 
 *	[Result] will thus be mapped to [A,B,C].
 *		
 *	The apply/3 version uses 'expression' for [OpType].
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List to be processed.
 *			
 *	[Operation] (i) -
 *		Operation to apply on each element of [List].
 *			
 *	[Result] (o) -
 *		List containing the result from applying [Operation] to	every 
 *		element of [List].
 *			
 *	[OpType] (i) -
 *		Type of [Operation]. Can be one of the following :
 *			
 *		'expression' or 'exp' or 'e' :
 *			[Operation] specifies a mathematical expression.
 *			In this expression, the variable 'X' refers to the element
 *			of [List] currently being processed.
 *				
 *		'operation' or 'op' or 'o' :
 *			[Operation] specifies a set of predicates to process each
 *			element of [List]. In this expression, the variable 'X'
 *			refers to the element of [List]	currently being processed. 
 *			The resulting value, referred to as the variable 'R', has
 *			to be unified by the list of predicates given in [Operation].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

apply(V, O, R, Type) :-
	belongs(Type, [expression, exp, e]),
	apply_prepare(V, O, R, expression),
	!.
apply(V, O, R, Type) :-
	belongs(Type, [operation, op, o]),
	apply_prepare(V, O, R, operation).
	
apply(V, O, R) :-
	apply_prepare(V, O, R, expression).
	

%
% apply_prepare only performs the necessary to declare a temp predicate that will 
% perform the operation, then calls the doapply predicate that performs
% the real stuff.
%
apply_prepare(Value, Operation, Result, Type) :-
	% If not a string, try all possible conversions -> string
	apply_stringify(Operation, NewOp),
		
	% Then we build a temp function definition that we will pass as an
	% argument to assert/1
	% When Type is 'expression', the definition looks like :
	%    temp_function(X, R) :- R is [Operation given as argument]
	% Otherwise ('operation') :
	%    temp(function(X, R) :- [Operation given as argument]
	apply_buildfunction(Type, NewOp, FDef),
		
	% We convert the string to a term (I'm using strings enclosed in
	% backquotes because I didn't find any way to process single-quoted
	% atoms
	string_term(FDef, FDefTerm),
	
	% Now add the temp predicate to Prolog base
	assert( FDefTerm ),

	% Apply the operation on all elements of the list
	apply_apply(Value, Result),
	
	% Remove the temp predicate from the base
	retract( FDefTerm ),

	% Cut to avoid unnecessery processing of other apply() clauses
	!.

%
% apply_apply(List, Result) -
%	Does the real stuff of applying the operation to each element
%	of the list.
%
apply_apply([], []).
apply_apply([V|Tail], Result) :-	% Does the real stuff
	apply_apply(Tail, TempResult),
	temp_function(V, R),		% Call the temp predicate defined in apply
	append([R], TempResult, Result).

%
% apply_stringify(V, Str) :
% 	unifies Str to V, after having converted it to a string.
%	V can be an atom or a string.
%
apply_stringify(V, V) :-
	is_string(V).
apply_stringify(V, Str) :-
	atom(V), 
	string_atom(Str, V).

%
% apply_buildfunction(Type, Output) -
%	Builds a function according to the type of operation given.
%
apply_buildfunction(expression, Operation, Output) :-
	stringlist_concat( 
		[ 'temp_function(X,R) :- R is ', Operation ], Output ).

apply_buildfunction(operation, Operation, Output) :-
	stringlist_concat( 
		[ 'temp_function(X,R) :- ', Operation ], Output ).

/******/

:- end_body(meta).