/****h* Modules/Interval
 ===============================================================================
 *
 * NAME
 *	Interval - Operations on intervals.
 *
 * FILE
 *	Modules/Array.pro
 *
 * DESCRIPTION
 *   * Introduction *
 *	The Interval module define basic operations on intervals.
 *	An interval has 4 properties :
 *	. Its lower bound
 *	. Its upper bound
 *	. Whether its is left closed or...
 *	. ...right-closed
 *
 *	Internally, an interval is represented by the interval/4, which has the
 *	following definition :
 *
 *		interval(Low, High, Left, Right)
 *
 *	where [Low] and [High] are the lower- and upper- bounds, repectively, and
 *	[Left] and [Right] are both binary values (0 or 1) representing the fact
 *	that the interval is left-closed or right-closed. A 1 means closed and a 
 *	zero means open. 
 *
 *	For example :
 *
 *		interval(1, 10, 1, 1) represents the interval [1, 10]
 *		interval(20,30, 0, 1) represents the interval ]20,30]
 *
 *	Note that the interval/0 predicate can sometimes arise during operations
 *	on intervals to represent an invalid or empty interval.
 *	
 *	Syntactical cosmetics are provided to mimic the mathematical notation on
 *	intervals. Thus, the example intervals given below can also be written as :
 *
 *		interval(1, 10, 1, 1) : </1..10/>
 *		interval(20,30, 0, 1) : >/20..30/>
 *
 *	Below are the 5 operators used for interval notation :
 *	* '</'  : closed left boundary
 *	* '>/'  : open left boundary
 *	* '/>'  : closed right boundary
 *	* '/<'  : open right boundary
 *	* '..'  : Separates the low- and high-boundaries of the interval.
 *
 *	Both Low and High values can be either any-type numbers, or the keyword
 *	'inf' (or 'infinite' or 'infinity'), with an optional plus or minus sign.
 *	An expression can also be provided instead of a number ; but for interpreter's
 *	sake, it will be much better to enclose them within parentheses, as in :
 *
 *		X = 32, I covers /< (X*2)..(X*3) /> .
 *
 *	which will unify I with />64..96/>.
 *
 *	Two other sets of operators are provided for operations on intervals ; 
 *	The first one is for performing math operations on intervals, the second
 *	one is for logical operations.
 *
 *   * Operations on intervals *
 *	The basic operator for manipulation intervals is 'covers' ; covers binds
 *	the variable to the left-hand side with the interval definition specified
 *	on the right-hand side :
 *
 *		X covers </1..10/> . 
 *
 *	will bind [X] with interval(1,10,1,1).
 *	The same operation could also have been written :
 *
 *		X covers interval(1,10,1,1).
 *
 *	
 *	The operators for manipulating intervals are :
 *	* difference		: computes the difference between two intervals.
 *	* intersect		: computes the intersection of two intervals.
 *	* union			: computes the union of two intervals.
 *	* negation		: computes the complement in R of an interval.
 *	* /+/, /-/, /./, /// 	: addition, substraction, multiplication and
 *				  division of two intervals.
 *
 *	Some operations can unify X with a set of intervals. In that case, instead
 *	of being unified to an interval/4 predicate, [X] will be susceptible to hold
 *	a list of interval definitions. For example, the 'negation' unary operator
 *	which gives the complement of the specified interval, can give two resulting
 *	intervals if the input interval is finite :
 *
 *		X covers negation </1..10/> .
 * 
 *	will unify X to [ interval(-inf, 1, 0, 0), interval(10, +inf, 0, 0) ].
 *
 *	All the interval manipulation operators accept either a single interval or
 *	a set of intervals as the input parameter.
 *
 *   * Logical operations on intervals *
 *	A set of logical operators are provided for testing intervals. They are used
 *	in standard predicates, and succeeds if the condition is verified :
 *
 *	X within Y (or X /<>/ Y) :
 *		Succeeds if Y contains X.
 *	X outside Y (or X /></ Y) :
 *		Succeeds if X and Y are disjoints.
 *	X joins Y :
 *		Succeeds if X and Y are coalescable, ie, they overlap and their
 *		union is a single, joint interval.
 *	X Op Y :
 *		Logical comparison ; Op can be any of :
 *		/==/ : X and Y are identical (same boundaries, sames closures)
 *		/~=/ : X and Y are not identical
 *		/</  : X is less than Y (X and Y are disjoint, and X is below Y)
 *		/<=/ : X is less than Y or X is identical to Y
 *		/>/  : X is greater than Y (X and Y are disjoint, and X is above Y)
 *		/>=/ : X is greater than Y or identical to Y
 *
 * BUGS
 *	* Due to an Amzi Prolog special handling of the '.' (dot) character, do not
 *	  put a dot immediately after the '/>' operator, otherwise the interpreter
 *	  will go into a loop.
 *	* Not sure to have tested all the operations on infinite intervals
 *
 * AUTHOR
 *	Christian Vigh, February 2007.
 *
 ===============================================================================
 ******/

/***
:- module(interval).

:- 	export( distance/3 ).
:- 	export( lbound/2 ).
:- 	export( length/2 ).
:- 	export( is_interval/1 ).
:-	export( ubound/2).

:- 	export( [
		covers/2,
		'..'/2,	'</'/2,	'>/'/2,	'/>'/2, '/<'/2,
		'/==/'/2, '/\==/'/2, '/<=/'/2, '/</'/2, '/>=/'/2, '/>/'/2, 
		'/<>/'/2, '/></'/2,
		'/~<=/', '/~</', '/~>=/', '/~>/', '/~==/',
		'/+/'/2, '/-/'/2, '///'/2, '/./'/2,
		within/2, outside/2, joins/2,
		union/2, intersect/2, difference/2, negation/2
	       ] ).

:- end_module(interval).

:- body(interval).
 ***/



/****if* Modules.Interval/operators
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	Operators for interval operations.
 *
 * DESCRIPTION
 *	The following categories of operators are available :
 *	* Assignment : covers
 *		Unifies the variable on the left-hand side with the interval on the
 *		right-hand side ; for example :
 *			X covers </1..10/>
 *		will unify X with interval(1, 10, 1, 1).
 *	* Interval construction : </, >/, .., /> and /<. For the low value of the
 *	  interval, '</' is used to specify a bounded side (left value belongs to
 *	  the interval), and '>/' for an unbounded one (left value does not belong
 *	  to the interval). For the high value of the interval, '/>' specifies a
 *	  bounded side, and '/<' an unbounded one. The '..' operator is used to
 *	  separate low and high values. For example :
 *		</1..10/<	>/-inf..100/>		>/0..9/<
 *	* Logical operators : X within Y (X is within Y), X outside Y (X and Y
 *	  are disjoint), X joins Y (X and Y overlap).
 *	* Comparison operators : /==/ (equality), /~=/ (inequality), /</ (X and Y
 *	  are disjoint, and X is below Y), /<=/ (X is below Y or X equals Y),
 *	  /~<=/ (X is below Y, X and Y can overlap and X can be identical to Y), 
 *	  />/ (X and Y are disjoint, and X is above Y), />=/ (X is greater than 
 *	  or equals Y), /~>=/ (X is above Y, X and Y can overlap and X can be 
 *	  identical to Y), /~</ (X is below Y, X and Y can overlap but X and Y 
 *	  cannot be identical), /~>/ (X is above Y, X and Y can overlap, but X
 *	  and Y cannot be identical), /~==/ ( X and Y overlap), /~\==/ (X and Y
 *	  do not overlap).
 *	* Mathematical operations :
 *		X union Y 	- computes the union of X and Y
 *		X intersect Y	- computes the intersection of X and Y
 *		X difference Y  - computes the difference of X and Y
 *		negation X	- computes the negation of X
 *		X /+/ Y		- computes the addition of X and Y
 *		X /-/ Y		- computes the substraction of Y from X
 *		X /./ Y		- computes the multiplication of X by Y
 *		X /// Y		- computes the division of X by Y
 *
 * NOTES
 *	For complex expressions to be parsed correctly, it is really important to
 *	take a special care to the operators' precedence values. Consider an
 *	expression like :
 *
 *		X covers </1..10/> union negation </100..200/>
 *
 *	The precedence values must grow in the following order :
 *	* '..'
 *	* '/>' and '/<' (interval closure)
 *	* '</' and '>/' (interval opening)
 *	* union (and other mathematical operators)
 *	* covers
 *
 *	Based on the Amzi documentation, the precedence that has been assigned to
 *	the 'covers' operator is the same as the other assignment operators. The
 *	precedence value for the mathematical operators has been chosen to be the 
 *	same as the one for standard mathematical operators like '+', '-', etc.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

:- op( 700, xfx, covers ).		% Assignment operator

:- op( 150, xfx, .. ).			% Interval-construction operators
:- op( 210,  fx, </ ).
:- op( 210,  fx, >/ ).
:- op( 200,  xf, /> ).
:- op( 200,  xf, /< ).

:- op( 500, xfx, [ /==/, /\==/, 	% Logical, relational operators
		   /<=/, /</, />=/, 
		   />/, /<>/, /></,
		   /~<=/, /~</, /~>=/, /~>/, /~==/
		  ] ).

:- op( 500, xfx, [within, inside] ).	% Other logical operators
:- op( 500, xfx, outside ).
:- op( 500, xfx, joins ).

:- op( 500, xfx, union ).		% Binary operators
:- op( 500, xfx, intersect ).
:- op( 500,  fx, negation ).
:- op( 500, xfx, difference ).

:- op( 500, xfx, [/+/, /-/, ///, /./] ).% Common interval operations

/******/



/****if* Modules.Interval/expressions
 -------------------------------------------------------------------------------
 *
 * NAME
 *	Predicates for expressions of interval arithmetics.
 *
 * PURPOSE
 *	This set of predicates is used for manipulating interval arithmetics ; 
 *	they are all based on the interval assignment operator 'covers' and have
 *	the following form :
 *	
 *		X covers [Expression]
 *
 *	where [Expression] is an interval, or an operation between intervals
 *	(see [operators]).
 *
 *	Interval construction : an interval construction form will unify a variable
 *	X with an interval definition, such as
 *
 *		X covers </1..10/>  	 	
 *	or the equivalent :
 *		X covers interval(1,10,1,1)
 *
 *	The code corresponding to such an assignment is the following :
 *	01: X covers </ L .. H /> :-
 *	02:	Low is L, High is H,
 *	03:	interval_build(X, Low, High, 1, 1).
 *
 *	Line 01 is the definition of the assignment predicate (here using a left-
 *	and right-closed interval).
 *
 *	Line 02 is used to evaluate the expression in [L] and [H]. This is the trick
 *	that allows for using mathematical expressions for lower and upper bounds
 *	of the interval, such as :
 *
 *		X = 32, Y covers </(X*2)..(X*3)/> .
 *
 *	Line 03 call the interval_build internal function, which checks that the
 *	interval is valid, then builds the interval. Since interval_build can fail
 *	if the interval is invalid, the cut before interval_build prevents Prolog
 *	from backtracking if such a case happens (this would give incorrect results).
 *
 *	Interval operations : an interval operation has the following form :
 *
 *		X covers A Op B		for binary operators or
 *		X covers Op A		for unary operators.
 *
 *	For example :
 *		X covers </1..10/>  union </5..100/> .
 *
 *	The code for such an assignment is the following :
 *
 *	01: X covers A union B :-
 *	02:	XA covers A, 
 *	03:	XB covers B,
 *	04:	!, interval_union(X, XA, XB).
 *
 *	Line 01 is the definition of the assignment predicate.
 *	Lines 02 and 03 are used to recursively expand compound expression, by 
 *	calling again the covers operator on each of the arguments.
 *	Line 04 calls the internal function performing the operation. Since such
 *	a function can fail if the operation is invalid, the cut before the call
 *	prevents Prolog from backtracking and search for incorrect results.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% Simple assignments
X covers </ L .. H /> :-			% Build left- and right-closed interval
	Low is L, High is H,
	interval_build(X, Low, High, 1, 1).
X covers </ L .. H /< :-			% Build left-closed, right-open interval
	Low is L, High is H,
	interval_build(X, Low, High, 1, 0).
X covers >/ L .. H /< :-			% Build left-open, right-open interval
	Low is L, High is H,
	interval_build(X, Low, High, 0, 0).
X covers >/ L .. H /> :-			% Build left- and right- open interval
	Low is L, High is H,
	interval_build(X, Low, High, 0, 1).
X covers interval(L, H, Left, Right) :-		% Use the interval/4 predicate to build the interval
	Low is L, High is H,
	interval_build(X, Low, High, Left, Right).


% Mathematical operations
X covers A union B :-				% Union between two overlapping intervals
	XA covers A, 
	XB covers B,
	!, interval_union(X, XA, XB).

X covers A intersect B :-			% Intersection between two intervals
	XA covers A, 
	XB covers B,
	!, interval_intersection(X, XA, XB).

X covers A difference B :-			% Difference between two intervals
	XA covers A, 
	XB covers B,
	!, interval_difference(X, XA, XB).

X covers A /+/ B :-				% Addition of two intervals
	XA covers A, 
	XB covers B,
	!, interval_add(X, XA, XB).

X covers A /-/ B :-				% Substraction of two intervals
	XA covers A, 
	XB covers B,
	!, interval_sub(X, XA, XB).

X covers A /// B :-				% Division of two intervals
	XA covers A, 
	XB covers B,
	!, interval_div(X, XA, XB).

X covers A /./ B :-				% Multiplication of two intervals
	XA covers A, 
	XB covers B,
	!, interval_mul(X, XA, XB).

X covers negation A :-				% Complement of an interval
	XA covers A,
	!, interval_negation(X, XA).

X covers FunctionCall :- 			% Allow for internal function calls such as :
   FunctionCall =.. [Predicate | Args], 	% X covers func(A)
   Goal =.. [Predicate, X | Args], 
   call(Goal). 



% Logical expressions 
X within Y :-					% Checks if Y contains X
	XA covers X,
	YA covers Y,
	!, interval_within(XA, YA).

X within Y :-					% Same, for a single point
	not( is_interval(X) ),			% Prevent backtracking to 2nd version
	YA covers Y,
	!, interval_within(X, YA).

X inside Y :-					% Alias to 'within'
	XA covers X,
	YA covers Y,
	!, interval_within(XA, YA).

X inside Y :-					% Same, for a single point
	not( is_interval(X) ),			% Prevent backtracking to 2nd version
	YA covers Y,
	!, interval_within(X, YA).



X /<>/ Y :-					% Alias for within
	X within Y.	

	
X outside Y :-					% Checks if X and Y are disjoint
	XA covers X,
	YA covers Y,
	!, interval_outside(XA, YA).

X /></ Y :-					% Alias for outside
	X outside Y.

X joins Y :-					% Check if intervals are coalescable
	XA covers X,
	YA covers Y,
	!, interval_coalescable(XA, YA).

X /==/ Y :-					% Checks if X and Y are identical
	XA covers X,
	YA covers Y,
	!, interval_comparison_eq( XA, YA ).

X /\==/ Y :-					% Checks if X and Y are not identical
	XA covers X,
	YA covers Y,
	!, interval_comparison_ne( XA, YA ).

X /<=/ Y :-					% Check if X is less than or identical to Y
	XA covers X,
	YA covers Y,
	!, interval_comparison_le( XA, YA ).

X /</ Y :-					% Check if X is less than Y
	XA covers X,
	YA covers Y,
	!, interval_comparison_lt( XA, YA ).

X />=/ Y :-					% Check if X is above or identical to Y
	XA covers X,
	YA covers Y,
	!, interval_comparison_ge( XA, YA ).

X />/ Y :-					% Check if X is above Y
	XA covers X,
	YA covers Y,
	!, interval_comparison_gt( XA, YA ).

X /~<=/ Y :-					% Check if X is below or equal to Y
	XA covers X,				% or overlaps Y
	YA covers Y,
	!, interval_comparison_nle( XA, YA ).

X /~>=/ Y :-					% Check if X is above or equal to Y
	XA covers X,				% or overlaps Y
	YA covers Y,
	!, interval_comparison_nge( XA, YA ).

X /~</ Y :-					% Check if X is below or overlaps Y
	XA covers X,				% but is not equal to Y
	YA covers Y,
	!, interval_comparison_nlt( XA, YA ).

X /~>/ Y :-					% Check if X is above or overlaps Y
	XA covers X,				% but is not equal to Y
	YA covers Y,
	!, interval_comparison_ngt( XA, YA ).

/******/




/****f* Modules.Interval/is_interval
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	is_interval/1
 *
 * SYNTAX
 *	is_interval(Name)
 *
 * PURPOSE
 *	Succeeds if [Name] is an interval.
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Variable to check.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% Check if object is an interval
is_interval( interval(_, _, _, _) ).
is_interval( interval ).	% Empty interval

/******/




/****f* Modules.Interval/length, lbound, ubound
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	length/2, lbound/2, ubound/2
 *
 * SYNTAX
 *	length(Name, Length)
 *	lbound(Name, Bound)
 *	ubound(Name, Bound)
 *
 * PURPOSE
 *	length/2 gives the length of an interval (as an integer value).
 *	lbound/2 gives the lower bound of the interval, and ubound/2 the upper bound.
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the interval to test for.
 *
 *	[Length] (o) -
 *		Unified with the length of the interval.
 *
 *	[Bound] (o) -
 *		Unified with the lower- (lbound/2) or upper- (ubound/2) bound of
 *		the interval.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
	
% Gives the length of an interval
length( interval(-inf,    _, _, _), inf).
length( interval(    , +inf, _, _), inf).
length( interval(-inf, +inf, _, _), inf).
length( interval( Low, High, _, _), X ) :-
	X is High - Low.

% Attribute-retrieving
lbound( interval(Low, _, _, _), Low).
ubound( interval(_, High, _, _), High).

/******/




/****f* Modules.Interval/distance
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	distance/3
 *
 * SYNTAX
 *	distance(I1, I2, Distance)
 *
 * PURPOSE
 *	Computes the distance between two intervals [I1] and [I2]. The distance 
 *	for two intervals [a,b], [c,d] is given by :
 *
 *		distance = max( |a-c|, |b-d] )
 *
 * ARGUMENTS
 *	[I1], [I2] (i) -
 *		Interval whose distance is to be computed.
 *
 *	[Distance] (o) -
 *		Unified with the distance between the two intervals [I1] and [I2].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

distance( interval(Low1, High1, _, _), interval(Low2, High2, _, _), X) :-
	Low1 =\= -inf, Low2 =\= -inf, High1 =\= +inf, High2 =\= +inf,
	A is Low1  - Low2 , AX is abs(A),
	B is High1 - High2, BX is abs(B),
	X is max(AX, BX).
distance( interval(_, _, _, _), interval(_, _, _, _), inf).

/******/




/****f* Modules.Interval/portray
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	portray/1
 *
 * SYNTAX
 *	portray(Interval)
 *
 * PURPOSE
 *	Portray is used to print the contents of an interval in a human-readable
 *	manner. Portray works on intervals as well as sets of intervals.
 *
 * ARGUMENTS
 *	[Interval] (i) -
 *		Interval whose contents are to be printed.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

portray_left(0, ']').
portray_left(1, '[').
portray_right(0, '[').
portray_right(1, ']').

portray( interval(A, B, Left, Right) ) :-
	portray_single(interval(A, B, Left, Right)),
	nl.

portray_single( interval(A, B, Left, Right) ) :-
	portray_left(Left, XL), write(XL),
	write(A), write('..'), write(B),
	portray_right(Right, XR),
	write(XR).
portray(interval) :-
	write('{empty}').

portray( [Head | Tail] ) :-
	write('( '),
	portray_list( [Head | Tail] ),
	write(' )'), nl.

portray_list( [Head | Tail] ) :-
	portray_single(Head),
	portray_optional_comma(Tail),
	portray_list(Tail).

portray_list( [] ).

portray_optional_comma( [] ).
portray_optional_comma( _ ) :-
	write(' , ').

/******/


%*******************************************************************************
%*******************************************************************************
%
%                             Basic Functions
%
%*******************************************************************************
%*******************************************************************************



/****if* Modules.Interval/interval_build
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	interval_build/5
 *
 * SYNTAX
 *	interval_build(Result, Low, High, Left, Right)
 *
 * PURPOSE
 *	Builds an interval (with the interval/4 predicate). This predicate is 
 *	called by the 'X covers interval' statement to unify X with the specified
 *	interval. Lower and upper bound are checked for validity, and keywords
 *	for infinite intervals are processed.
 *
 *	The keywords for an infinite interval are :
 *	. [+/-]inf
 *	. [+/-]infinite
 *	. [+/-]infinity
 *
 *	[Left] and [Right] specify the left- and right- closures of the interval.
 *	They take the value 1 is the interval is closed, or 0 if open. 
 *	interval_build allows for closed interval specification with infinite
 *	value, but replaces that with an open interval ; for example :
 *
 *		[-inf..inf]
 *
 *	will be considered as ]-inf..+inf[.
 *	
 *	interval_build fails if the interval boundaries are invalid ; this includes
 *	situations where [Low] is greater than [High], but also when the interval
 *	is impossible due to its closure, as in :
 *
 *		[1..1[
 *
 * ARGUMENTS
 *	[Result] (o) -
 *		Variable that will be unified with the result, which takes the
 *		form : interval(Low, High, Left, Right).
 *
 *	[Low] (i) -
 *		Lower bound of the interval.
 *
 *	[High] (i) -
 *		Upper bound of the interval.
 *
 *	[Left] (i) -
 *		Left-closure of the interval. If set to 1, the interval includes
 *		its lower value ; otherwise, it includes the value immediately
 *		after.
 *
 *	[Right] (i) -
 *		Right-closure of the interval.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% Build an interval
% Syntax : X covers Low..High
interval_build( X, Low, High, Left, Right ) :-
	interval_left(Low, Left, NewLow, NewLeft),		% Process potential infinity to the left
	interval_right(High, Right, NewHigh, NewRight),		% then to the right
	interval_check(NewLow, NewHigh, NewLeft, NewRight),	% Check the interval
	X = interval(NewLow, NewHigh, NewLeft, NewRight), !.	% Unify it

	
% Aliases for infinity
infinity('inf').
infinity('infinite').
infinity('infinity').


% Check exclusion/inclusion of lower bound
% (allow inclusive range for infinity)
interval_left(Keyword, _, -inf, 0) :-				% Infinite keyword
	infinity(Keyword).
interval_left(-Keyword, _, -inf, 0) :-
	infinity(Keyword).
interval_left(Value, Left, Value, Left).			% Normal value

% Same, for the upper bound
interval_right(Keyword, _, +inf, 0) :-
	infinity(Keyword).
interval_right(+Keyword, _, +inf, 0) :-
	infinity(Keyword).
interval_right(Value, Left, Value, Left).


% Checks the validity of an interval
interval_check(A, B, 1, 1) :-					% Left and right inclusive :
	A =< B.							% Lower bound must be <= Upper
interval_check(A, B, Left, Right) :-				% Left OR right inclusive :
	A < B, ( Left =:= 0 ; Right =:= 0 ).			% Lower bound must be < Upper

interval_check(A, B, Left, Right) :-				% Error case : fuck
	portray_left(Left, SLeft),
	portray_right(Right, SRight),
	atomlist_concat( [SLeft, A, '..', B, SRight], I),
	throw( interval_error('Invalid range', I) ).

/******/




/****if* Modules.Interval/interval_within
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	interval_within/2
 *
 * SYNTAX
 *	interval_within(I1, I2)
 *
 * PURPOSE
 *	Logical predicates that succeeds if the interval [I1] is completely included
 *	in [I2]. This predicate is called when the following expression is
 *	encountered :
 *
 *		I1 within I2.
 *
 * ARGUMENTS
 *	[I1], [I2] (i) -
 *		Intervals to check.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% Version which checks that one interval is inside another one
interval_within( interval(Low1, High1, Left1, Right1),
		 interval(Low2, High2, Left2, Right2) ) :-
	% Bottom value of I1 must be greater than bottom value of I2
	% (taking into account their respective left-closures)
	interval_left_greater( Low1,  Left1,  Low2,  Left2),
	% Same for upper value
	interval_right_less(  High1, Right1, High2, Right2), !. 

% Version which checks that one point is within one interval
interval_within( Point, interval(Low2, High2, Left2, Right2) ) :-
	not( is_interval( Point ) ),
	interval_left_greater( Point,  1,  Low2,  Left2),
	interval_right_less(  Point, 1, High2, Right2), !. 

% Check if the lower value of I1 is greater than the lower value of I2
interval_left_greater(V1, 1, V2, 1) :-
	V1 >= V2.
interval_left_greater(V1, 0, V2, 1) :-
	V1 >= V2.
interval_left_greater(V1, 0, V2, 0) :-
	V1 > V2.
interval_left_greater(V1, 1, V2, 0) :-
	V1 > V2.

% Check if the upper value of I1 is less than the upper value of I2
interval_right_less(V1, 1, V2, 1) :-
	V1 =< V2.
interval_right_less(V1, 0, V2, 1) :-
	V1 =< V2.
interval_right_less(V1, 0, V2, 0) :-
	V1 < V2.
interval_right_less(V1, 1, V2, 0) :-
	V1 < V2.

/******/




/****if* Modules.Interval/interval_outside
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	interval_outside/2
 *
 * SYNTAX
 *	interval_outside(I1, I2)
 *
 * PURPOSE
 *	Logical predicate that checks that [I1] and [I2] are disjoint. This predicate
 *	is called when the following expression is met :
 *
 *		I1 outside I2.
 *
 *	interval_outside/2 fails if [I1] and [I2] overlap.
 *
 * ARGUMENTS
 *	[I1], [I2] (i) -
 *		Intervals to check.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% Checks if intervals are disjoint ; this version considers I1 below I2
interval_outside( interval(   _, High1,     _, Right1),
		  interval(Low2,     _, Left2,      _) :-
	interval_do_outside( High1, Low2, Right1, Left2 ).

% And this one considers I1 above I2
interval_outside( interval(Low1,     _, Left1,      _),
		  interval(   _, High2,     _, Right2) :-
	interval_do_outside( High2, Low1, Right2, Left1 ).
	
	
interval_do_outside( High, High, 1, 0).		% [a,x] is outside ]x,b]
interval_do_outside( High, High, 0, 0).		% [a,x[ is outside ]x,b]
interval_do_outside( High, High, 0, 1).		% [a,x] is outside [x,b]
interval_do_outside( High, Low , _, _) :-	% Other cases : [a,x? is outside ?y,b]
	High < Low.

/******/





/****if* Modules.Interval/interval_coalescable
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	interval_coalescable/2
 *
 * SYNTAX
 *	interval_coalescable(I1, I2)
 *
 * PURPOSE
 *	Logical predicate that checks that [I1] and [I2] are coalescable, ie, that
 *	they overlap and their union is a single, non-disjoint interval.
 *	This predicate is called when the following expression is met :
 *
 *		I1 joins I2.
 *
 * ARGUMENTS
 *	[I1], [I2] (i) -
 *		Intervals to check.
 *
 * NOTES
 *	interval_coalescable/2 fails if [I1] and [I2] are disjoint.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% Check if intervals are coalescable : first, consider I1 <= I2
interval_coalescable( interval(    _, High1,    _ , Right1 ),
		      interval( Low2,     _, Left2,      _ ) ) :-
	interval_do_coalescable(High1, Low2, Right1, Left2).
% then I2 >= I1
interval_coalescable( interval( Low1,     _, Left1,      _ ),
		      interval(    _, High2,     _, Right2 ) ) :-
	interval_do_coalescable(Low1, High2, Left1, Right2).

interval_do_coalescable(High1, High1, 1, 1).		% [a,x] and [x,b] overlap
interval_do_coalescable(High1, High1, 1, 0).		% [a,x] and ]x,b] do not overlap...
interval_do_coalescable(High1, High1, 0, 1).		% but their union is a single non disjoint interval
interval_do_coalescable(High1,  Low2, _, _) :-		% Normal case [a,x] and [y,b] overlap if
	High1 > Low2.					% x > y

/******/




/****if* Modules.Interval/interval_union
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	interval_union/3
 *
 * SYNTAX
 *	interval_union(Result, I1, I2)
 *
 * PURPOSE
 *	Performs the union of the intervals [I1] and [I2] and unifies [Result] with
 *	the corresponding set.
 *	This predicate is called when the following expression is met :
 *
 *		Result covers I1 union I2.
 *
 *	[I1] and [I2] can be disjoint ; in that case, [Result] will be a set of
 *	intervals. Similarly, [I1] and [I2] can themselves be sets of intervals.
 *
 * ARGUMENTS
 *	[I1], [I2] (i) -
 *		Interval or sets of intervals whose union is to be computed.
 *
 *	[Result] (o) -
 *		Result of the union (either a single interval or a result set).
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

interval_union( interval(NewLow, NewHigh, NewLeft, NewRight),
		interval(Low1, High1, Left1, Right1),
		interval(Low2, High2, Left2, Right2) ) :-
	interval_coalescable( interval(Low1, High1, Left1, Right1),
			      interval(Low2, High2, Left2, Right2) ),
	interval_min( Low1,  Low2,  Left1,  Left2,  NewLow, NewLeft ),
	interval_max(High1, High2, Right1, Right2, NewHigh, NewRight).

/******/




/****if* Modules.Interval/interval_intersection
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	interval_intersection/3
 *
 * SYNTAX
 *	interval_intersection(Result, I1, I2)
 *
 * PURPOSE
 *	Performs the intersection of the intervals [I1] and [I2] and unifies [Result]
 *	with the corresponding set.
 *	This predicate is called when the following expression is met :
 *
 *		Result covers I1 intersect I2.
 *
 *	[I1] and [I2] must be coalescable.
 *
 * ARGUMENTS
 *	[I1], [I2] (i) -
 *		Interval or sets of intervals whose union is to be computed.
 *
 *	[Result] (o) -
 *		Result of the union (either a single interval or a result set).
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

interval_intersection( interval(NewLow, NewHigh, NewLeft, NewRight),
		       interval(Low1, High1, Left1, Right1),
		       interval(Low2, High2, Left2, Right2) ) :-
	interval_coalescable( interval(Low1, High1, Left1, Right1),
			      interval(Low2, High2, Left2, Right2) ),
	interval_max( Low1,  Low2,  Left1,  Left2,  NewLow, NewLeft ),
	interval_min(High1, High2, Right1, Right2, NewHigh, NewRight).

/******/





/****if* Modules.Interval/interval_min, interval_max
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	interval_min/6, interval_max/6
 *
 * SYNTAX
 *	interval_min(Low1, Low2, Left1, Left2, NewLow, NewLeft)
 *	interval_max(Low1, Low2, Left1, Left2, NewLow, NewLeft)
 *
 * PURPOSE
 *	For limits [Low1] and [Low2] of two intervals, with closures [Left1] and
 *	[Left2] (0 for open, 1 for closed), interval_min and interval_max unify
 *	[NewLow] with the result min/max value, and [NewLeft] with the new closure.
 *	This predicate is used by interval_union/3 and interval_intersection/3 to
 *	compute the closure and lower/upper bounds of the resulting interval.
 *
 * ARGUMENTS
 *	[Low1], [Low2] (i) -
 *		Lower bounds of the two intervals.
 *
 *	[Left1], [Left2] (i) -
 *		Left-closure of the two intervals.
 *
 *	[NewLow] (o) -
 *		New lower (or upper) bound.
 *
 *	[NewLeft] (o) -
 *		New left (or right) closure.
 *
 * NOTES
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% min of two lower bounds
interval_min(Value ,  Value,     1,     _,  Value,     1).	% min([a,b] , ?a ,c]) = a
interval_min(Value ,  Value,     _,     1,  Value,     1).	% min(?a,b] , [a ,c]) = a
interval_min(Value1, Value2, Left1,     _, Value1, Left1) :-	% min([a1,b], [a2,c]) = a1...
	Value1 < Value2.					% if a1 < a2
interval_min(     _, Value2,     _, Left2, Value2, Left2). 	% Or a2 if a1 > a2

% max of two upper bounds
interval_max(Value ,  Value,     1,     _,  Value,     1).	% max([a,b ], [c,b ?) = b
interval_max(Value ,  Value,     _,     1,  Value,     1).	% max([a,b ?, [c, b]) = b
interval_max(Value1, Value2, Left1,     _, Value1, Left1) :-	% max([a,b1], [c, b2?) = b1...
	Value1 > Value2.					% if b1 > b2
interval_max(     _, Value2,     _, Left2, Value2, Left2). 	% Or b2 if b1 < b2

/******/




/****if* Modules.Interval/interval_negation
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	interval_negation/2
 *
 * SYNTAX
 *	interval_negation(Result, I)
 *
 * PURPOSE
 *	Unifies [Result] with the complement in R of the interval [I]. 
 *	The following gives the possible results depending on the value of [I] :
 *
 *	I = [a,b]        -->  Result = [-inf,a[ union ]b,+inf[
 *	I = ]-inf,a]     -->  Result = ]a, +inf[
 *	I = ]-inf,+inf[  -->  Result = undefined interval
 *	etc.
 *
 *	This predicate is called when the following expression is met :
 *
 *		R covers negation I
 *
 * ARGUMENTS
 *	[Result] (o) -
 *		Unified with the complement set of [I].
 *
 *	[I] (i) -
 *		Interval whose complement is to be computed. [I] can be a single
 *		interval or a set of intervals.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

interval_negation( interval, 				% [I] = ]-inf,+inf[, R = undefined
		   interval(-inf,  +inf, _, _) ).

interval_negation( interval(High, +inf, NewRight, 0),	% [I] = ]-inf, a?, R = ?a,+inf]
		   interval(-inf, High, _, Right) ) :-
	interval_negate_inclusion(Right, NewRight).

interval_negation( interval(-inf, Low, 0, NewLeft),	% [I] = ?a,+inf[, R = ]-inf,a?
		   interval(Low , +inf, Left, _) ) :-
	interval_negate_inclusion(Left, NewLeft).
	
interval_negation( [ interval(-inf, Low, 0, NewLeft),	% [I] = ?a,b?, R = ]-inf,a? union ?b,+inf[
		     interval(High, +inf, NewRight, 0) ],
		   interval(Low , High, Left, Right) ) :-
	interval_negate_inclusion(Left , NewLeft),
	interval_negate_inclusion(Right, NewRight).

%
% The complement of an interval must have its closure reversed (closed becomes
% open, and open becomes closed, unless one of the bound is infinite).
% interval_negate_inclusion is used for that.	
%
interval_negate_inclusion(0, 1).
interval_negate_inclusion(1, 0).

/******/




/****if* Modules.Interval/interval_difference
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	interval_difference/3
 *
 * SYNTAX
 *	interval_difference(Result, I1, I2)
 *
 * PURPOSE
 *	Performs the difference of the intervals [I1] and [I2] and unifies [Result]
 *	with the corresponding set.
 *	This predicate is called when the following expression is met :
 *
 *		Result covers I1 difference I2.
 *
 * ARGUMENTS
 *	[I1], [I2] (i) -
 *		Interval or sets of intervals whose union is to be computed.
 *
 *	[Result] (o) -
 *		Result of the union (either a single interval or a result set).
 *
 * NOTES 
 *	Difference between two intervals can be explained by the 5 following cases ;
 *	Let I1 be [a,b] and I2 be [c,d] :
 *
 *	. Case 1 :
 *		a	b	c	d
 *		|-------|	|-------|
 *
 *		I1 and I2 are disjoint so I1 - I2 = [a,b]
 *
 *	. Case 2 :
 *		a			b
 *		|-----------------------|	
 *			c	d
 *			|-------|
 *
 *		I2 strictly includes I2 (c > a and d < b) so I1 - I2 = [a,c[ union ]d,b]
 *
 *	. Case 3 :
 *		a			b
 *		|-----------------------|
 *			c		d
 *			|---------------|
 *
 *		a < c and b <= d so I1 - I2 = [a,c[
 *
 *	. Case 4 :
 *		a			b
 *		|-----------------------
 *		c	d
 *		|-------|	
 *
 *		a >= c, d < b so I1 - I2 = ]d,b]
 *
 *	. Case 5 (complement of case 2) :
 *			a	b
 *			|-------|
 *		c			d
 *		|-----------------------|
 *
 *		a > c, b < d so I1 - I2 = []
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
 
% Case 1
interval_difference( interval(Low1, High1, Left1, Right1 ),
		     interval(Low1, High1, Left1, Right1 ),
		     interval(Low2, High2, Left2, Right2 ) ) :-
	interval_outside(
	     interval(Low1, High1, Left1, Right1 ),
	     interval(Low2, High2, Left2, Right2 ) ).

% Case 2
interval_difference( [ interval(Low1, Low2, Left1, 0 ), interval(High2, High1, 0, Right1) ],
		     interval(Low1, High1, Left1, Right1 ),
		     interval(Low2, High2, Left2, Right2 ) ) :-
	Low1 < Low2, High1 > High2.
	
% Case 3     
interval_difference( interval(Low1, Low2, Left1, 0 ),
		     interval(Low1, High1, Left1, Right1 ),
		     interval(Low2, High2, Left2, Right2 ) ) :-
	Low1 < Low2, High1 =< High2.

% Case 4
interval_difference( interval(High2, High1, 0, Right1 ),
		     interval(Low1, High1, Left1, Right1 ),
		     interval(Low2, High2, Left2, Right2 ) ) :-
	Low1 >= Low2, High1 > High2.

% Case 5
interval_difference( interval,
		     interval(Low1, High1, Left1, Right1 ),
		     interval(Low2, High2, Left2, Right2 ) ) :-
	Low1 >= Low2, High1 =< High2.

/******/




/****if* Modules.Interval/interval_comparison operators
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	interval_comparison_xx/2
 *
 * SYNTAX
 *	interval_comparison_xx( I1, I2 )
 *
 * PURPOSE
 *	This predicate is called for comparing the relative values of two intervals.
 *	The relation between predicate and comparison operators is the following :
 *
 *	. interval_comparison_eq(I1, I2)  :  I1 /==/ I2
 *		Succeeds if I1 is identical to I2.
 *	. interval_comparison_ne(I1, I2)  :  I1 /~=/ I2
 *		Succeeds if I1 is not indentical to I2.
 *	. interval_comparison_lt(I1, I2)  :  I1 /</  I2
 *		Succeeds if I1 is less than I2 (I1 is disjoint from I2, and below I2).
 *	. interval_comparison_le(I1, I2)  :  I1 /<=/ I2
 *		Succeeds if I1 is less than or identical to I2.
 *	. interval_comparison_gt(I1, I2)  :  I1 />/  I2
 *		Succeeds if I1 is greater than I2 (I1 is disjoint from I2, and
 *		above I2).
 *	. interval_comparison_ge(I1, I2)  :  I1 />=/ I2
 *		Succeeds if I1 is greater than or identical to I2.
 *	. interval_comparison_nle(I1, I2)  :  I1 /~<=/ I2
 *		Succeeds if I1 is nearly less I2 (I1 is less than or identical to
 *		I2, and I1 and I2 can overlap).
 *	. interval_comparison_nge(I1, I2)  :  I1 /~>=/ I2
 *		Succeeds if I1 is nearly greater than I2 (I1 is greater than or 
 *		identical to I2, and I1 and I2 can overlap).
 *	. interval_comparison_nl(I1, I2)  :  I1 /~</ I2
 *		Succeeds if I1 is nearly less I2 (I1 is less than I2, and I1 and 
 *		I2 can overlap).
 *	. interval_comparison_ng(I1, I2)  :  I1 /~>/ I2
 *		Succeeds if I1 is nearly greater than I2 (I1 is greater than I2 
 *		I2, and I1 and I2 can overlap).
 *
 * ARGUMENTS
 *	[I1], [I2] (i) -
 *		Intervals to compare.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% Operator : /==/
interval_comparison_eq( interval(Low, High, Left, Right),
		        interval(Low, High, Left, Right) ).

% Operator : /\==/
interval_comparison_ne( A, B ) :-
	not( interval_comparison_eq( A, B ) ).
	
% Operator : /</
interval_comparison_lt( interval(    _, High1, _, _),
			interval( Low2,     _, _, _) ) :-
		High1 < Low2.
interval_comparison_lt( interval(    _, High1, _, 1),
			interval(High1,     _, 0, _) ).
interval_comparison_lt( interval(    _, High1, _, 0),
			interval(High1,     _, 0, _) ).
interval_comparison_lt( interval(    _, High1, _, 0),
			interval(High1,     _, 1, _) ).

% Operator : /<=/
interval_comparison_le( A, B ) :-
	interval_comparison_lt( A, B ).
interval_comparison_le( A, B ) :-
	interval_comparison_eq( A, B ).

% Operator : />/
interval_comparison_gt( A, B ) :-
	interval_comparison_lt( B, A ).

% Operator : />=/
interval_comparison_ge( A, B ) :-
	interval_comparison_le( B, A ).


% Operator : /~<=/
interval_comparison_nle( A, B ) :-
	interval_comparison_le( A, B ).
interval_comparison_nle( A, B ) :-
	A joins B, not( A within B ).
	
% Operator : /~</
interval_comparison_nlt( A, B ) :-
	interval_comparison_lt( A, B ).
interval_comparison_nlt( A, B ) :-
	A joins B, not( A within B ).

% Operator : /~>=/
interval_comparison_nge( A, B ) :-
	interval_comparison_le( B, A ).
interval_comparison_nge( A, B ) :-
	B joins A, not( B within A ).
	
% Operator : /~>/
interval_comparison_ngt( A, B ) :-
	interval_comparison_lt( B, A ).
interval_comparison_ngt( A, B ) :-
	B joins A, not( B within A ).

% Operator : /~==/
interval_comparison_neq( A, B ) :-
	A joins B.

/******/





/****if* Modules.Interval/Mathematic operations
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	interval_add/3, interval_sub/3, interval_mul/3, interval_div/3
 *
 * SYNTAX
 *	interval_add(Result, I1, I2)
 *	interval_sub(Result, I1, I2)
 *	interval_mul(Result, I1, I2)
 *	interval_div(Result, I1, I2)
 *
 * PURPOSE
 *	Perform basic mathematic operations on intervals ; if I1 is [a,b] and
 *	I2 is [b,c] then :
 *
 *	. interval_add (invoked by the expression : Result covers I1 /+/ I2) :
 *		Computes the sum of two intervals : [a+c, b+d]
 *	. interval_sub (invoked by the expression : Result covers I1 /-/ I2) :
 *		Computes the subtraction of two intervals : [a-d, b-c]
 *	. interval_mul (invoked by the expression : Result covers I1 /./ I2) :
 *		Computes the multiplication of two intervals : 
 *			[min(ac, ad, bc, bd), max(ac, ad, bc, bd)]
 *	. interval_div (invoked by the expression : Result covers I1 /// I2) :
 *		Computes the division of two intervals : 
 *			[min(a/c, a/d, b/c, b/d), max(a/c, a/d, b/c, b/d)]
 *		The operations gives an undefined result ('interval') if c or d
 *		are equal to zero.
 *
 * ARGUMENTS
 *	[Result] (o) -
 *		Unified with the result of the operation.
 *
 *	[I1], [I2] (i) -
 *		First and second operands of the mathematical operation.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% Addition of two intervals
interval_add( interval(NewLow, NewHigh, NewLeft, NewRight),
	      interval(Low1  , High1  , Left1  , Right1  ),
	      interval(Low2  , High2  , Left2  , Right2  ) ) :-
	interval_inclusion_union(Left1 , Left2 , NewLeft ),
	interval_inclusion_union(Right1, Right2, NewRight),
	NewLow  is Low1 + Low2,
	NewHigh is High1 + High2.

% Substraction of two intervals
interval_sub( interval(NewLow, NewHigh, NewLeft, NewRight),
	      interval(Low1  , High1  , Left1  , Right1  ),
	      interval(Low2  , High2  , Left2  , Right2  ) ) :-
	interval_inclusion_union(Left1 , Left2 , NewLeft ),
	interval_inclusion_union(Right1, Right2, NewRight),
	NewLow  is Low1 - High2,
	NewHigh is High1 - Low2.

% Multiplication of two intervals
interval_mul( interval(NewLow, NewHigh, NewLeft, NewRight),
	      interval(Low1  , High1  , Left1  , Right1  ),
	      interval(Low2  , High2  , Left2  , Right2  ) ) :-
	interval_inclusion_union(Left1 , Left2 , NewLeft ),
	interval_inclusion_union(Right1, Right2, NewRight),
	AC is Low1 * Low2,
	AD is Low1 * High2,
	BC is High1 * Low2,
	BD is High1 * High2,
	NewLow is min(AC, min(AD, min(BC, BD))),
	NewHigh is max(AC, max(AD, max(BC, BD))).


% Division of two intervals
interval_div( interval, _, interval(0, 0, _, _) ).		% Limit cases
interval_div( interval, _, interval(0, _, _, _) ).
interval_div( interval, _, interval(_, 0, _, _) ).

interval_div( interval(NewLow, NewHigh, NewLeft, NewRight),
	      interval(Low1  , High1  , Left1  , Right1  ),
	      interval(Low2  , High2  , Left2  , Right2  ) ) :-
	interval_inclusion_union(Left1 , Left2 , NewLeft ),
	interval_inclusion_union(Right1, Right2, NewRight),
	AC is Low1  / Low2,
	AD is Low1  / High2,
	BC is High1 / Low2,
	BD is High1 / High2,
	NewLow  is min(AC, min(AD, min(BC, BD))),
	NewHigh is max(AC, max(AD, max(BC, BD))).


%
% Closures are to be taken into account during math operations on intervals.
% The interval_inclusion_union predicate determines what will be the resulting
% closure of the resulting interval, given the closures of the two intervals
% involved in the operation.
%
interval_inclusion_union(0, 0, 0).	% ]a..b?  op  ]c..d?  = ]...?
interval_inclusion_union(1, 0, 0).	% [a..b?  op  ]c..d?  = ]...?
interval_inclusion_union(0, 1, 0).	% ]a..b?  op  [c..d?  = ]...?
interval_inclusion_union(1, 1, 1).	% [a..b?  op  [c..d?  = [...?

/******/

	
	
%:- end_body(interval).