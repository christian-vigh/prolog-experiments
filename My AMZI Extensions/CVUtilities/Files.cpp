/****h* CVUtilities/Files
 ===============================================================================
 *
 * NAME
 *	Files - Extended predicates for operation on files
 *
 * FILE
 *	Files.cpp
 *
 * CONTENTS
 *	This file contains a set of AMZI extended predicates for file operations.
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


# include	<stdio.h>
# include	<malloc.h>
# include	<string.h>

# include	"CVUtilities.h"




/****f* CVUtilities.Files/string_file/2
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	string_file/2
 *
 * SYNTAX
 *	string_file(String, File).
 *
 * PURPOSE
 *	Unifies [String] with the contents of the file [File].
 *
 * ARGUMENTS
 *	[String] (o) -
 *		String that will receive the contents of the specified file.
 *
 *	[File] (i) -
 *		File to read.
 *
 * NOTES
 *	string_file will fail if the specified file does not exist, or if not 
 *	enough memory is available.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_string_file ( ENGid	Engine )
   {
	char *			String ;
	characters		FileName [ 1024 ] ;
	int			Length ;
	register FILE *		fp ;
	


	// Retrieve the filename
	lsGetParm ( Engine, 2, cSTR, FileName ) ;

	// Open the file (in binary mode, otherwise file length will be greater
	// than bytes read, since CRLF are converted)
	if ( ( fp = fopen( FileName, "rb" ) )  ==  NULL )
		return( FALSE ) ;

	Length = FileLength( fp ) ;
	String = ( char * ) malloc( Length + 1 ) ;

	// Check if we are running out of memory
	if ( String  ==  NULL )
	   {
		fclose ( fp ) ;
		return ( FALSE ) ;
	     }

	// Read the file then unify the result
	fread( String, 1, Length, fp ) ;
	fclose( fp ) ;

	String [ Length ] = '\0' ;

	lsUnifyParm ( Engine, 1, cSTR, String ) ;
	free( String ) ;

	return( TRUE ) ;
     }

/******/



