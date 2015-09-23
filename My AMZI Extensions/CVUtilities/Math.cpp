/****h* CVUtilities/Math
 ===============================================================================
 *
 * NAME
 *	Math - Extended predicates for mathematical operations
 *
 * FILE
 *	Math.cpp
 *
 * CONTENTS
 *	This file contains a set of AMZI extended predicates for performing 
 *	mathematical operations that would otherwise show poor performance in
 *	pure Prolog code.
 *
 * AUTHOR
 *	Christian Vigh, February 2007.
 *
 * HISTORY
 *	[Version]  [Date]	[Contents]
 *	   1.00	   11/02/2007	Initial version.
 *
 ===============================================================================
 ******/


# include	<Math.h>

# include	"CVUtilities.h"
# include	"Math.h"




/****if* CVUtilities.Math/fact
 -------------------------------------------------------------------------------
 *
 * FUNCTION
 *	fact
 *
 * SYNTAX
 *	double  fact ( value ) ;
 *
 * PURPOSE
 *	Computes the factorial of [value].
 *
 * ARGUMENTS
 *	double	value (i) -
 *		Value whose factorial is to be computed.
 *
 * RETURN VALUE
 *	Factorial of the specified [value]. Note that :
 *		fact(0) = 1 
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
double fact ( double  Fact )
   {
	double		Result, X ;


	X = Fact ; Result = 1 ; Fact = floor ( Fact ) ;

	// Compute the value
	while ( X > 1 )
	   {
		Result *= X ;
		X -- ;
	     }

	return ( Result ) ;
     }

/******/




/****if* CVUtilities.Math/partial_fact
 -------------------------------------------------------------------------------
 *
 * FUNCTION
 *	partial_fact
 *
 * SYNTAX
 *	double	partial_fact ( double  X, double  Y ) ;
 *
 * PURPOSE
 *	Computes a partial factorial given by :
 *
 *			      X
 *		Result = ------------
 *			   (X - Y) !
 *
 *	ie, X * (X-1) * (X-2) * ... * (X-n), where Y = X - n
 *
 * ARGUMENTS
 *	double		X, Y (i) -
 *		See the description above.
 *
 * RETURN VALUE
 *	Value of the partial factorial.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

double	partial_fact ( double  X, double  Y )
   {
	double		Result = 1 ;
	double		I ;

	for ( I = Y + 1 ; I <= X ; I += 1 )
		Result *= I ;

	return ( Result ) ;
     }

/******/





/****f* CVUtilities.Math/comb
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	comb/3
 *
 * SYNTAX
 *	comb( Number, Subset Result ).
 *
 * PURPOSE
 *	Unifies [Result] with the the number of combinations of [Subset] elements
 *	among [Number].
 *
 * ARGUMENTS
 *	[Number] (i) -
 *		Number of elements in the set whose permutation count is to be 
 *		computed.
 *
 *	[Subset] (i) -
 *		Number of elements of the permutation.
 *		
 *	[Result] (o) -
 *		Resulting value.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_comb ( ENGid	Engine )
   {
	double		X, Y ;
	double		Result ;
	

	// Get the value
	lsGetParm ( Engine, 1, cDOUBLE, & X ) ;
	lsGetParm ( Engine, 2, cDOUBLE, & Y ) ;
	X = floor ( X ) ; Y = floor ( Y ) ;

	// Check if X and Y are valid values
	if ( X < Y  ||  X < 1  ||  Y < 1 )
		return ( FALSE ) ;

	// Compute the value
	Result = partial_fact( X, (X - Y) ) / fact ( Y ) ;

	// Unify the result
	lsUnifyParm ( Engine, 3, cDOUBLE, & Result ) ;

	return ( TRUE ) ;
     }

/******/






/****f* CVUtilities.Math/fact
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	fact/2
 *
 * SYNTAX
 *	fact(Number, Result).
 *
 * PURPOSE
 *	Unifies [Result] with the factorial of [Number].
 *
 * ARGUMENTS
 *	[Number] (i) -
 *		Number whose factorial is to be computed.
 *
 *	[Result] (o) -
 *		Resulting value.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_fact ( ENGid	Engine )
   {
	double		Fact, Result ;


	// Get the value
	lsGetParm ( Engine, 1, cDOUBLE, & Fact ) ;
	
	if ( Fact  < 0 ) 
		return ( FALSE ) ;

	Result = fact ( floor ( Fact ) ) ;

	// Unify the result
	lsUnifyParm ( Engine, 2, cDOUBLE, & Result ) ;

	return ( TRUE ) ;
     }

/******/




/****f* CVUtilities.Math/perm
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	perm/3
 *
 * SYNTAX
 *	perm( Number, Subset Result ).
 *
 * PURPOSE
 *	Unifies [Result] with the the number of permutations of [Subset] elements
 *	among [Number].
 *
 * ARGUMENTS
 *	[Number] (i) -
 *		Number of elements in the set whose permutation count is to be 
 *		computed.
 *
 *	[Subset] (i) -
 *		Number of elements of the permutation.
 *		
 *	[Result] (o) -
 *		Resulting value.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_perm ( ENGid	Engine )
   {
	double		X, Y ;
	double		Result ;
	

	// Get the value
	lsGetParm ( Engine, 1, cDOUBLE, & X ) ;
	lsGetParm ( Engine, 2, cDOUBLE, & Y ) ;
	X = floor ( X ) ; Y = floor ( Y ) ;

	// Check if X and Y are valid values
	if ( X < Y  ||  X < 1  ||  Y < 1 )
		return ( FALSE ) ;

	// Compute the value
	Result = partial_fact( X,  X - Y ) ;

	// Unify the result
	lsUnifyParm ( Engine, 3, cDOUBLE, & Result ) ;

	return ( TRUE ) ;
     }

/******/
