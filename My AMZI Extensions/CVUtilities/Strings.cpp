/****h* CVUtilities/Strings
 ===============================================================================
 *
 * NAME
 *	Strings - Extended string manipulation predicates.
 *
 * FILE
 *	Strings.cpp
 *
 * CONTENTS
 *	Contains a set of extended predicates to use for string manipulation.
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


# include	<malloc.h>
# include	<string.h>

# include	"CVUtilities.h"




/****f* CVUtilities.Strings/string_index/3
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	string_index/3
 *
 * SYNTAX
 *	string_index(S1, S2, Index).
 *
 * PURPOSE
 *	Unifies [Index] with the index of string [S2] within [S1].
 *
 * ARGUMENTS
 *	[S1] (i) -
 *		String to search in.
 *	[S2] (i) -
 *		String to search for.
 * 	[Index] (o) -
 *		Unified with the index of string [S2] within [S1] (the first 
 *		character of a string has the index 1), or zero if the searched 
 *		string has not been found.
 *
 * NOTES
 *	string_index works on a dynamically allocated copy of [S1] and [S2]. 
 *	The predicate will fail if not enough memory is available.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_string_index ( ENGid	Engine )
   {
	char *			S1,
	     *			S2 ;
	int			S1Length,
				S2Length ;
	int			Index ;
	register char *		p ;


	// Retrieve the lengths of the string arguments
	S1Length = lsStrParmLen( Engine, 1 ) + 1 ;
	S2Length = lsStrParmLen( Engine, 2 ) + 1 ;

	// Allocate S1
	if ( ( S1 = (char *) malloc( S1Length ) )  ==  NULL ) 
		return ( FALSE ) ;

	// Allocate S2
	if ( ( S2 = (char *) malloc( S2Length ) )  ==  NULL ) 
	   {
		free (S1) ;
		return ( FALSE ) ;
	     }

	// Get S1 and S2
	lsGetParm ( Engine, 1, cSTR, S1 ) ;
	lsGetParm ( Engine, 2, cSTR, S2 ) ;

	// Perform the search and compute the index
	p = strstr(S1, S2) ;

	if ( p  ==  NULL ) 
		Index = -1 ;
	else
		Index = ( (int) (p - S1) ) + 1 ;

	lsUnifyParm ( Engine, 3, cINT, & Index ) ;

	// Cleanup
	free ( S1 ) ;
	free ( S2 ) ;
	return( TRUE ) ;
     }

/******/






/****f* CVUtilities.Strings/string_index/4
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	string_index/4
 *
 * SYNTAX
 *	string_index(S1, S2, Start, Index).
 *
 * PURPOSE
 *	Unifies [Index] with the index of string [S2] within [S1].
 *	The search is performed starting at position [Start] in [S1].
 *
 * ARGUMENTS
 *	[S1] (i) -
 *		String to search in.
 *	[S2] (i) -
 *		String to search for.
 *	[Start] (i) -
 *		Position in [S1] where to start the search.
 *	[Index] (o) -
 *		Unified with the index of string [S2] within [S1] (the first
 *		character of a string has the index 1), or zero if the searched 
 *		string has not been found.
 *
 * NOTES
 *	string_index works on a dynamically allocated copy of [S1] and [S2]. 
 *	The predicate will fail if not enough memory is available, if the searched 
 *	string is not found, or if [Start] is too big.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_string_index_start ( ENGid		Engine )
   {
	char *			S1,
	     *			S2 ;
	int			S1Length,
				S2Length,
				Start ;
	int			Index ;
	register char *		p,
		      *		q ;


	// Retrieve the lengths of the string arguments
	S1Length = lsStrParmLen ( Engine, 1 ) + 1 ;
	S2Length = lsStrParmLen ( Engine, 2 ) + 1 ;
	lsGetParm( Engine, 3, cINT, & Start ) ;

	// Fail if S1[Start] is out of range
	if ( S1Length < Start ) 
		return( FALSE ) ;

	// Allocate S1
	if ( ( S1 = (char *) malloc( S1Length ) )  ==  NULL ) 
		return ( FALSE ) ;

	// Allocate S2
	if ( ( S2 = (char *) malloc( S2Length ) )  ==  NULL ) 
	   {
		free (S1) ;
		return ( FALSE ) ;
	     }

	// Get S1 and S2
	lsGetParm ( Engine, 1, cSTR, S1 ) ;
	lsGetParm ( Engine, 2, cSTR, S2 ) ;

	// Perform the search and compute the index
	q = S1 + Start - 1 ;
	p = strstr(q, S2) ;

	if ( p  ==  NULL ) 
		Index = -1 ;
	else
		Index = ( (int) (p - S1) ) + 1 ;

	lsUnifyParm ( Engine, 4, cINT, & Index ) ;

	// Cleanup
	free ( S1 ) ;
	free ( S2 ) ;
	return( TRUE ) ;
     }

/******/