/****h* CVUtilities/System
 ===============================================================================
 *
 * NAME
 *	System - System-related extended predicates
 *
 * FILE
 *	System.cpp
 *
 * CONTENTS
 *	This file contains a set of AMZI extended predicates for system operations,
 *	and so on.
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


# include	"CVUtilities.h"







/****f* CVUtilities.System/computername
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	computername/1
 *
 * SYNTAX
 *	computername(Name).
 *
 * PURPOSE
 *	Unifies [Name] with the name of the computer.
 *
 * ARGUMENTS
 *	[Name] (o) -
 *		String that will receive the name of the computer. 
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_getcomputername ( ENGid	Engine )
   {
	char		Name [ MAX_COMPUTERNAME_LENGTH + 1 ] ;
	DWORD		Size = sizeof ( Name ) ;

	// Get current directory
	if ( ! GetComputerName( Name, & Size ) )
		return ( FALSE ) ;	

	// Set parameter 1 to current directory
	lsUnifyParm ( Engine, 1, cATOM, Name ) ;

	return( TRUE ) ;
     }

/******/



/****f* CVUtilities.System/getenv
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	getenv/2
 *
 * SYNTAX
 *	getenv(Name, Value).
 *
 * PURPOSE
 *	Unifies [Name] with the contents of the environment variable [Value].
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the environment variable to query. 
 *
 *	[Value] (o) - 
 *		Unified with the contents of the environment variable [Name].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_getenv ( ENGid	Engine )
   {
	char		Name  [ 1024 ] ;
	char 		Value [ 1024 ] ;


	// Get variable name
	lsGetParm ( Engine, 1, cSTR, Name ) ;
	GetEnvironmentVariable ( Name, Value, sizeof ( Value ) ) ;

	// Set parameter 1 to current directory
	lsUnifyParm ( Engine, 2, cATOM, Value ) ;

	return( TRUE ) ;
     }

/******/




/****f* CVUtilities.System/getenvlist
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	getenvlist/1
 *
 * SYNTAX
 *	getenvlist( List ).
 *
 * PURPOSE
 *	Unifies [List] with a list of currently defined environment variables.
 *
 * ARGUMENTS
 *	[List] (o) -
 *		Unified with the list of environment variables.
 *
 * NOTES
 *	getenvlist always succeed. 
 *	Most of the code is to handle the fact that the variable names are in 
 *	sorted order. When using lsPushList, they become in reverse order. This
 *	is why the names are temporarily hold in the Names[] array, then pushed
 *	from last to first.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_getenvlist ( ENGid	Engine )
   {
	char			Name  [ 1024 ] ;
	char *			Names [ 1024 ] ;
	int			NameCount	=  0 ;
	char *			equal_sign ;
	char *			env_block ;
	register char *		p ;
	register int		length,
				copy_count ;
	TERM			ListTerm ;
	int			return_value	= TRUE ;
	register int		i ;


// Get environment strings
	p = env_block = GetEnvironmentStrings() ;

	if ( p  ==  NULL )
		return ( FALSE ) ;

// Create the list term
	if ( lsMakeList ( Engine, & ListTerm ) )
		return ( FALSE ) ;

// Cycle through the environment variables
// (a list of "Name=Value" strings terminated by an empty string)
	while ( * p )				// Block is ended by two nuls
	   {
		length = strlen ( p ) ;

	// Locate the equal sign
		equal_sign = strchr ( p, '=' ) ;

		if ( equal_sign  ==  NULL )	// I am really paranoiac
			copy_count = length ;
		else
			copy_count = ( int ) ( equal_sign - p ) ;

	// Isolate the variable name
		strncpy ( Name, p, copy_count ) ;
		Name [copy_count] = 0 ;

	// May have some garbage at the beginning of the environment block
		if ( * Name )
			Names [ NameCount ++ ] = strdup ( Name ) ;

		p += length + 1 ;		// Go to next env string
	    }

// Create the list 
	for ( i = NameCount - 1 ; i >= 0 ; i -- )
	   {
		TERM		ListItem ;


	// Create the atom for this new variable name
		if ( lsMakeAtom ( Engine, & ListItem, Names [i] ) ) 
		{
			return_value = FALSE ;
			goto Cleanup ;
		}

	// Push the new variable name
		if ( lsPushList ( Engine, & ListTerm, ListItem ) )
		{
			return_value = FALSE ;
			goto Cleanup ;
		}
	    }

Cleanup :
// Unify the result
	lsUnifyParm ( Engine, 1, cTERM, & ListTerm ) ;

// Free the variables names 
	for ( i = 0 ; i < NameCount ; i ++ )
		free ( Names [i] ) ;

// Free the environment block
	FreeEnvironmentStrings ( env_block ) ;

	return( return_value ) ;
     }

/******/




/****f* CVUtilities.System/setenv
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	setenv/2
 *
 * SYNTAX
 *	setenv(Name, Value).
 *
 * PURPOSE
 *	Assigns [Value] to the environment variable [Name].
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the environment variable to set. 
 *
 *	[Value] (i) - 
 *		New value of the environment variable.
 *
 * NOTES
 *	setenv always succeeds.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_setenv ( ENGid	Engine )
   {
	char		Name  [ 1024 ] ;
	char		Value [ 4096 ] ;

	// Get variable name and value
	lsGetParm ( Engine, 1, cSTR, Name ) ;
	lsGetParm ( Engine, 2, cSTR, Value ) ;

DebugMessage("setenv", "%s = %s", Name, Value ) ;

	SetEnvironmentVariable ( Name, Value ) ;
	return( TRUE ) ;
     }

/******/






/****f* CVUtilities.System/username
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	username/1
 *
 * SYNTAX
 *	username(Name).
 *
 * PURPOSE
 *	Unifies [Name] with the name of the current user.
 *
 * ARGUMENTS
 *	[Name] (o) -
 *		String that will receive the name of the current user. 
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_getusername ( ENGid	Engine )
   {
	char		Name [ 1024 ] ;
	DWORD		Size = sizeof ( Name ) ;


	// Get current directory
	if ( ! GetUserName( Name, & Size ) )
		return ( FALSE ) ;	

	// Set parameter 1 to current directory
	lsUnifyParm ( Engine, 1, cATOM, Name ) ;

	return( TRUE ) ;
     }

/******/




