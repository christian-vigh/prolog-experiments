/****h* Modules/boolean
 ===============================================================================
 *
 * NAME
 *	boolean - boolean predicates.
 *
 * FILE
 *	Modules/boolean.pro
 *
 * CONTENTS
 *	The 'boolean' module implements operations on booleans and is designed in
 *	such a way that it can be used to generate truth tables, or go through the
 *	possible solutions of a boolean equation.
 *
 *	A boolean value is either 0 or 1, but the true() and false() predicates
 *	are provided to map some aliases to those values. Whatever the aliases
 *	used, the result of a boolean expression is either 0 or 1.
 *
 *	A boolean expression takes the form of an assignment using the '<-' operator,
 *	like in the following example :
 *
 *		X <- true or (A and not B).
 *
 *	which unifies 'X' with the boolean expression 'true or (A and not B)'.
 *
 *	Any element in a boolean expression can either be an atom or an unbound
 *	variable. This allows for generating truth tables using the Prolog backtracking
 *	mechanism, either at the command prompt or using the findall predicate :
 *
 *		X <- A or (B and not C).
 *
 *	will find all the values of X, A, B and C for which the expression is true.
  *	
 *	The following boolean operators are defined :
 *	* not	- logical not
 *	* and	- logical and
 *	* or	- logical or
 *	* xor	- logical xor
 *	* imp	- logical implication
 *	* nand	- logical not-and
 *	* nor	- logical nor
 *
 *	Each truth table is defined as a set of predicates, thus allowing for this
 *	unification/backtracking mechanism ; an example is given below for the 'and'
 *	logical operation :
 *
 *		boolean_and(0, 0, 0).	% false and false -> false
 *		boolean_and(0, 1, 0).	% false and true  -> false
 *		boolean_and(1, 0, 0).	% true  and false -> false
 *		boolean_and(1, 1, 1).	% true  and true  -> true
 *	
 * TODO
 *	* Implement exception for out of domain values.
 *	* Add the possibility to call internal predicates within a boolean
 *	  expression (the possibility already exist, is between comments, the only
 *	  problem being which kind of internal predicates could be used within a
 *	  boolean expression ?)
 *
 * AUTHOR
 *	Christian Vigh, January 2007.
 *
 ===============================================================================
 ******/


:- module(boolean).

:- 	export(true/1).			% Boolean to 0/1 mapping predicates
:- 	export(false/1).
:- 	export(boolean/1).
:-	export(boolean_value/2).

:-	export(boolean_identity/2).	% Truth tables for boolean operations
:-	export(boolean_not/2).
:-	export(boolean_or/2).
:-	export(boolean_and/2).
:-	export(boolean_xor/2).
:-	export(boolean_imp/2).
:-	export(boolean_nand/2).
:-	export(boolean_nor/2).
:-	export(boolean_or/2).

:-	export(<-/2).			% Boolean operators
:-	export(not/2).
:-	export(or/2).
:-	export(and/2).
:-	export(xor/2).
:-	export(imp/2).
:-	export(nand/2).
:-	export(nor/2).

: end_module(boolean).



:- body(boolean).

/****f* Modules.Boolean/boolean, false, true
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	false/1, true/1
 *
 * SYNTAX
 *	boolean(B), false(B), true(B)
 *
 * PURPOSE
 *	Boolean succeeds if [B] is a boolean value (either false(B) or true(B)).
 *	A boolean value can be :
 *	* true, 1, yes, on, y, vrai, oui or o
 *	* false, 0, no, off, n, faux, non
 *
 * ARGUMENTS
 *	[B] (i) -
 *		Boolean value to check.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% true's or false's...
true(1).	true(yes).	true(on).	true(true).	true(y).
true(vrai).	true(oui).	true(o).

false(0).	false(no).	false(off).	false(false).	false(n).
false(faux).	false(non).


% What is a boolean ?
boolean(X) :-
	true(X).
boolean(X) :- 
	false(X).

/******/




/****f* Modules.Boolean/Truth tables
 -------------------------------------------------------------------------------
 *
 * PURPOSE
 *	The various boolean operations are described using truth tables that are
 *	implemented with clauses :
 *	
 *	* boolean_identity(X,Y) -
 *		Truth table for the identity boolean operation. Succeeds only if
 *		[X] is the same boolean value as [Y].
 *
 *	* boolean_or(A, B, X) -
 *		Truth table for the OR logical operation. Succeeds only if the boolean
 *		expression :
 *			X = A or B
 *		is true.
 *
 *	etc. Other truth tables are :
 *	* boolean_xor
 *	* boolean_and
 *	* boolean_not
 *	* boolean_imp
 *	* boolean_nand
 *	* boolean_nor
 *
 * ARGUMENTS
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% Equality testing
boolean_identity(0,0).
boolean_identity(1,1).


% The NOT operation
boolean_not(1,0).
boolean_not(0,1).


% The OR operation
boolean_or(0, 0, 0).
boolean_or(0, 1, 1).
boolean_or(1, 0, 1).
boolean_or(1, 1, 1).


% The AND operation
boolean_and(0, 0, 0).
boolean_and(0, 1, 0).
boolean_and(1, 0, 0).
boolean_and(1, 1, 1).


% The XOR operation
boolean_xor(0, 0, 0).
boolean_xor(0, 1, 1).
boolean_xor(1, 0, 1).
boolean_xor(1, 1, 0).


% The IMP operation
boolean_imp(0, 0, 1).
boolean_imp(0, 1, 1).
boolean_imp(1, 0, 0).
boolean_imp(1, 1, 1).


% The NAND operation
boolean_nand(0, 0, 1).
boolean_nand(0, 1, 1).
boolean_nand(1, 0, 1).
boolean_nand(1, 1, 0).


% The NOR operation
boolean_nor(0, 0, 1).
boolean_nor(0, 1, 0).
boolean_nor(1, 0, 0).
boolean_nor(1, 1, 0).

/******/




/****f* Modules.Boolean/boolean_value
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	boolean_value/2
 *
 * SYNTAX
 *	boolean_value(X, Y)
 *
 * PURPOSE
 *	boolean_value is what is used to associate boolean value aliases with either
 *	the value 0 or 1. It adds the possibility of additional cosmetics in a 
 *	boolean expression at the expense of a little bit more of complexity !
 *	
 *	The predicate behaves differently depending on whether [X] is an unbound
 *	variable or not :
 *	* if [X] is an atom, then [Y] is unified with either 0 or 1, depending on
 *	  the value of X.
 *	* if not, boolean_value acts as a placeholder and simply unifies its second
 *	  argument with the first.
 *
 *	This trick is to allow ANY element of a logical assignment (including the
 * 	left-hand side) to contain a variable.
 *
 * 	Examples:
 *		X <- not true.
 *		X <- not A.
 *		true <- not A.
 *
 * ARGUMENTS
 *	[X] (i) -
 *		Boolean constant (see boolean()).
 *	[Y] (o) -
 *		Value of the constant (either 0 or 1).
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

boolean_value(X, X) :-		
	var(X), !. 
boolean_value(X, 1) :-
	nonvar(X),
	true(X), !.
boolean_value(X, 0) :-
	nonvar(X),
	false(X), !.

/******/



	
/****f* Modules.Boolean/assignment operator : <-/2
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	<-/2
 *
 * SYNTAX
 *	X <- boolean_expression
 *
 * PURPOSE
 *	Unifies X with the specified boolean expression ; for example :
 *
 *		X <- true or (false and not true)
 *
 *	The supported boolean operators are : not, and, or, xor, imp, nand & nor.
 *	Parentheses can be used to facilitate expression grouping.
 *
 *	Every element of a boolean expression can be an unbound variable ; this 
 *	allows for Prolog unification & backtracking system to give all possible
 *	solutions for the specified expression (either at the command prompt or
 *	using the findall predicate) :
 *
 *		X <- A or B.
 *
 *	will list the truth table for the 'or' boolean operation. A combination of
 *	both can be used :
 *
 *		true <- A or B.
 *
 *	will give all the values of [A] and [B] for which the boolean operation 
 *	'A or B' is true.
 *
 *	More complex expressions can be used :
 *
 *		X <- A or (B and ( not C nand D )).
 *
 *	The operator having the highest priority is 'not', then '<-', then the
 *	other boolean operations.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

%
% Operators :
% 	Important !!!
% 	- Unary operators must have the greatest priority
% 	- then assignment operators
% 	- then binary operators
%
% Binary operators must not have a priority higher than assignment, otherwise
% prolog would arrange the expression grouping in a bizarre way...
%
:- op( 700, xfx,   <-).
:- op( 900,  fx,  not).
:- op( 500, xfx,   or).
:- op( 500, xfx,  and).
:- op( 500, xfx,  xor).
:- op( 500, xfx,  imp).
:- op( 500, xfx,  nand).
:- op( 500, xfx,  nor).



%
% Various boolean expressions
%
X <- not A  :-				% NOT operator
	structured_expression(A, Xa),
	boolean_value(Xa, Abis),
	boolean_value(X , Xbis),
	boolean_not(Abis, Xbis).


X <- A or B  :-				% OR operator
	binary_operator(A, B, X, Abis, Bbis, Xbis),
	boolean_or(Abis, Bbis, Xbis).


X <- A and B  :-			% AND operator
	binary_operator(A, B, X, Abis, Bbis, Xbis),
	boolean_and(Abis, Bbis, Xbis).


X <- A xor B  :-			% XOR operator
	binary_operator(A, B, X, Abis, Bbis, Xbis),
	boolean_xor(Abis, Bbis, Xbis).


X <- A imp B  :-			% IMP operator
	binary_operator(A, B, X, Abis, Bbis, Xbis),
	boolean_imp(Abis, Bbis, Xbis).


X <- A nand B  :-			% NAND operator
	binary_operator(A, B, X, Abis, Bbis, Xbis),
	boolean_nand(Abis, Bbis, Xbis).


X <- A nor B  :-			% NOR operator
	binary_operator(A, B, X, Abis, Bbis, Xbis),
	boolean_nor(Abis, Bbis, Xbis).

X <- Y :-				% Simple assignment
	simple_expression(Y),
	boolean_value(X, Xbis),
	boolean_value(Y, Ybis),
	boolean_identity(Xbis, Ybis).			




%
% Operations common to binary operators :
%
binary_operator(A, B, X, Abis, Bbis, Xbis) :-
	structured_expression(A, Xa),	% Check if A is a compound expression
	structured_expression(B, Xb),	% same for B
	boolean_value(Xa, Abis),	% Convert A into either 0 or 1
	boolean_value(Xb, Bbis),	# then B
	boolean_value(X , Xbis).	# then X



%
% Since unification is heavily used, we must restrain Prolog from searching
% into undesirable goals ; we thus has to do the distinction between structured
% expressions ('A or B') and simple ones ('A').
%
simple_expression(A) :-			% Expression is an atom
	not(structure(A)).
	
structured_expression(A, Xa) :-		% Expression is a complex one (a structure)
	structure(A),
	Xa <- A.
structured_expression(A, A).

/******/


/*

%
% To implement if one day I find some ideas about which internal predicates could
% be called from within a boolean expression :
%
X <*  F :- 				% For calling internal predicates in the
   	F =.. [Pred|Args], 		% boolean formula
   	P =.. [Pred, X|Args], 
   	call(P). 
*/

:- end_body(boolean).