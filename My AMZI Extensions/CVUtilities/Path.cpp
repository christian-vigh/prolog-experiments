/****h* CVUtilities/Path
 ===============================================================================
 *
 * NAME
 *	Path - Extended predicates for path manipulation.
 *
 * FILE
 *	Path.cpp
 *
 * CONTENTS
 *	This file contains a set of AMZI extended predicates for path operations.
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

# include	<shlwapi.h>
# include	<shlobj.h>



/****f* CVUtilities.Path/chdir
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	chdir/1
 *
 * SYNTAX
 *	chdir(Path).
 *
 * PURPOSE
 *	Sets the current directory to [Path].
 *
 * ARGUMENTS
 *	[Path] (i) -
 *		New current directory. This parameter can specify a drive different
 *		from the working drive. In that case, the chdir predicate will also
 *		change the working drive.
 *
 * NOTES
 *		chdir can also be called as cd/1.
 *		The predicate succeeds if the directory change is successful.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC	cv_chdir ( ENGid	Engine ) 
   {
	char		Path [ MAX_PATH + 1 ] ;
	char		Drive ;


	// Get first argument
	lsGetParm ( Engine, 1, cSTR, Path ) ;

	// Check if we need to change the current drive
	if ( Path [1]  ==  ':' )
	   {
		Drive = toupper (Path [0]) - 'A' + 1 ;

		// Drive change unsuccessful : the predicate will fail
		if ( _chdrive (Drive)  <  0 ) 
			return ( FALSE ) ;
	     }

	// Change the current directory
	if ( chdir ( Path )  <  0 ) 
		return ( FALSE ) ;

	// All done, everything was ok
	return ( TRUE ) ;
    }

/******/




/****f* CVUtilities.Path/copyfile
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	copyfile/2, copyfile/3
 *
 * SYNTAX
 *	copyfile(From, To).
 *	copyfile(From, To, FailIfExists).
 *
 * PURPOSE
 *	Copies the file [From] to the file [To]. If [FailIfExists] is set to a 
 *	non-zero value, the predicate will fail when the destination file already
 *	exists.
 *	The copyfile/2 predicate calls copyfile/3, specifying a zero value
 *	for the [FailIfExists] parameter : in that case, the destination file will
 *	be overwritten if it already exists.
 *
 * ARGUMENTS
 *	[From] (i) -
 *		File to copy.
 *
 *	[To] (i) -
 *		Destination file.
 *
 *	[FailIfExists] (i) -
 *		If set to non-zero and the file specified by [To] already exists, the
 *		predicate will fail. Otherwise (zero value), the destination
 *		file will be overwritten.
 *
 * NOTES
 *	The predicate fails if an error occurred or if the destination file already
 *	exists and [FailIfExists] is set to zero.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

// copyfile/3 predicate
AMZIFUNC cv_copyfile_3 ( ENGid	Engine )
   {
	char		From [ MAX_PATH + 1 ],
			To   [ MAX_PATH + 1 ] ;
	int		FailIfExists ;


	// Get the three arguments
	lsGetParm ( Engine, 1, cSTR, From ) ;
	lsGetParm ( Engine, 2, cSTR, To ) ;
	lsGetParm ( Engine, 3, cINT, & FailIfExists ) ;

	// Adjust the boolean flag
	FailIfExists = ( FailIfExists ) ? TRUE : FALSE ;

	// Copy the file
	if  ( CopyFile ( From, To, FailIfExists ) ) 
		return ( TRUE ) ;
	else
		return ( FALSE ) ;
    }



// copyfile/2 predicate
AMZIFUNC cv_copyfile_2 ( ENGid	Engine )
   {
	char		From [ MAX_PATH + 1 ],
			To   [ MAX_PATH + 1 ] ;


	// Get the three arguments
	lsGetParm ( Engine, 1, cSTR, From ) ;
	lsGetParm ( Engine, 2, cSTR, To ) ;

	// Copy the file
	if  ( CopyFile ( From, To, 0 ) ) 
		return ( TRUE ) ;
	else
		return ( FALSE ) ;
    }

/******/




/****f* CVUtilities.Path/delext
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	delext/2
 *
 * SYNTAX
 *	delext( File, Result ).
 *
 * PURPOSE
 *	Deletes the extension part from [File] and unifies [Result] with the result.
 *
 * ARGUMENTS
 *	[File] (i) -
 *		File name whose extension is to be removed.
 *	[Result] (i) -
 *		Result (filename without extension).
 *
 * NOTES
 *	delext/2 always succeeds.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC	cv_delext ( ENGid  Engine )
   {
	char		Filename [ MAX_PATH + 1 ] ;


	// Get the filename
	lsGetParm ( Engine, 1, cSTR, Filename ) ;

	// Delete the extension
	PathRemoveExtension ( Filename ) ;

	// Unify the result
	lsUnifyParm ( Engine, 2, cSTR, Filename ) ;
	return ( TRUE ) ;
    }

/******/





/****f* CVUtilities.Path/filexists
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	fileexists/1
 *
 * SYNTAX
 *	fileexists(Name).
 *
 * PURPOSE
 *	Checks if the specified file [Name] exists. Succeeds if yes, and fails
 *	if the file has not been found.
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the file to check.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_fileexists ( ENGid	Engine )
   {
	char		Filename [ MAX_PATH + 1 ] ;


	// Get path to expand and expand it
	lsGetParm (Engine, 1, cSTR, Filename ) ;

	// Check file existence
	if  ( PathFileExists ( Filename ) )
		return( TRUE ) ;
	else
		return ( FALSE ) ;
     }

/******/




/****f* CVUtilities.Path/fileflag
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	fileflag/2
 *
 * SYNTAX
 *	fileflag(Path, Flags).
 *
 * PURPOSE
 *	The fileflag/2 predicate has two behaviors ; the first one, when [Flags]
 *	represent a file flag, makes the predicate to succeed when the file has
 *	the specified flag. The second one, when [Flags] is an unbound variable,
 *	unifies it with the list of flag names defined for [Path].
 *
 * ARGUMENTS
 *	[Path] (i) -
 *		File whose flags are to be retrieved or tested.
 *	[Flags] (io) -
 *		Either a flag to check or an unbound variable that will be unified
 *		to the list of flags defined for the specified file.
 *
 * NOTES
 *	The fileflag predicate fails if an incorrect path or incorrect flag name
 *	has been specified.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */


// Association table for the list of file flags
BEGIN_ASSOCIATION ( fileflag_values )
	ASSOCIATION ( "archive"		, FILE_ATTRIBUTE_ARCHIVE	),
	ASSOCIATION ( "compressed"	, FILE_ATTRIBUTE_COMPRESSED	),
	ASSOCIATION ( "directory"	, FILE_ATTRIBUTE_DIRECTORY	),
	ASSOCIATION ( "encrypted"	, FILE_ATTRIBUTE_ENCRYPTED	),
	ASSOCIATION ( "hidden"		, FILE_ATTRIBUTE_HIDDEN		),
	ASSOCIATION ( "normal"		, FILE_ATTRIBUTE_NORMAL		),
	ASSOCIATION ( "offline"		, FILE_ATTRIBUTE_OFFLINE	),
	ASSOCIATION ( "readonly"	, FILE_ATTRIBUTE_READONLY	),
	ASSOCIATION ( "reparsepoint"	, FILE_ATTRIBUTE_REPARSE_POINT	),
	ASSOCIATION ( "sparsefile"	, FILE_ATTRIBUTE_SPARSE_FILE	),
	ASSOCIATION ( "system"		, FILE_ATTRIBUTE_SYSTEM		),
	ASSOCIATION ( "temporary"	, FILE_ATTRIBUTE_TEMPORARY	),
END_ASSOCIATION ;



// Fileflag predicate
AMZIFUNC  cv_fileflag ( ENGid  Engine ) 
   {
	char				Path [ MAX_PATH + 1 ] ;
	int				IsUnified	=  0 ;
	DWORD				Value ;
	WIN32_FILE_ATTRIBUTE_DATA	Data ;
	register int			i ;
	TERM				List, Atom ;


	// Get the Path parameter
	lsGetParm ( Engine, 1, cSTR, Path ) ;

	// Get the file attributes
	if  ( ! GetFileAttributesEx ( Path, GetFileExInfoStandard, & Data ) )
		return ( FALSE ) ;

	// Check if we need to verify that the specified flag is set or if
	// we need to retrieve the list of defined flags
	IsUnified = ( lsGetParmType ( Engine, 2 )  !=  pVAR ) ;

	// IsUnified == TRUE : check if the specified flag is set
	if  ( IsUnified )
	   {
		char		FlagName [ MAX_PATH + 1 ] ;


		// Retrieve the flag name and get its value
		lsGetParm ( Engine, 2, cSTR, FlagName ) ;
		Value = association_value ( fileflag_values, FlagName ) ;

		return ( ( Value & Data. dwFileAttributes ) ? TRUE : FALSE ) ;
	     }

	// IsUnified = FALSE : we have to retrieve the list of defined flags for
	// the specified path, and unify the second argument to that list
	if  ( lsMakeList ( Engine, & List )  !=  OK ) 
		return ( FALSE ) ;

	// Chek for each possible value of the flag
	// (scan the flags table in reverse order)
	for  ( i = ASSOCIATION_SIZE ( fileflag_values ) - 1 ; i >= 0 ; i -- )
	   {
		if  ( Data. dwFileAttributes & fileflag_values [i]. ConstantValue )
		   {
			lsMakeAtom ( Engine, & Atom, fileflag_values [i]. ConstantName ) ;
			lsPushList ( Engine, & List, Atom ) ;
		     }
	     }

	// All done unify the result
	lsUnifyParm ( Engine, 2, cTERM, & List ) ;
	return ( TRUE ) ;
    }

/******/





/****f* CVUtilities.Path/filematch
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	filematch/2
 *
 * SYNTAX
 *	filematch(Name, Wildcard).
 *
 * PURPOSE
 *	Succeeds if the file specified by [Name] corresponds to the wildcard 
 *	specification [Wildcard].
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name to check.
 *	[Wildcard] (i) -
 *		Wildcard to compare against [Name].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC	cv_filematch ( ENGid  Engine )
   {
	char		Filename [ MAX_PATH + 1 ],
			Wildcard [ MAX_PATH + 1 ] ;


	// Get the arguments
	lsGetParm ( Engine, 1, cSTR, Filename ) ;
	lsGetParm ( Engine, 2, cSTR, Wildcard ) ;

	// Call the function
	if  ( PathMatchSpec ( Filename, Wildcard ) )
		return ( TRUE ) ;
	else
		return ( FALSE ) ;
    }

/******/




/****f* CVUtilities.Path/filesize
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	filesize/2
 *
 * SYNTAX
 *	filesize(Path, Size).
 *
 * PURPOSE
 *	Unifies [Size] with the size of the specified file name.
 *
 * ARGUMENTS
 *	[Path] (i) -
 *		File whose size is to be retrieved.
 *	[Size] (o) -
 *		Unified with the size of file [Path].
 *
 * NOTES
 *	filesize/2 fails if the file does not exist or its attributes can not
 *	been retrieved.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC  cv_filesize ( ENGid  Engine ) 
   {
	char				Path [ MAX_PATH + 1 ] ;
	WIN32_FILE_ATTRIBUTE_DATA	Data ;


	// Get the Path parameter
	lsGetParm ( Engine, 1, cSTR, Path ) ;

	// Get the file attributes
	if  ( ! GetFileAttributesEx ( Path, GetFileExInfoStandard, & Data ) )
		return ( FALSE ) ;

	// Unify the result and return 
	lsUnifyParm ( Engine, 2, cINT, & Data. nFileSizeLow ) ;
	return ( TRUE ) ;
     }

/******/





/****f* CVUtilities.Path/filetime
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	filetime/3, filetime/4
 *
 * SYNTAX
 *	filetime(Path, Keyword, DateAndTime).
 *	filetime(Path, Keyword, Date, Time).
 *
 * PURPOSE
 *	Retrieve the date and time for the file specified by [Path]. [Keyword]
 *	can be any of : 'creation', for getting the creation time ; 'modification'
 *	for the last update time ; and 'access' for the last access time.
 *
 * ARGUMENTS
 *	[Path] (i) -
 *		Path of the file.
 *
 *	[Keyword] (i) -
 *		Type of type to be retrieved : creation, modification or access.
 *
 *	[DateAndTime] (o) -
 *		Unified with the term datetime(yy, mm, dd, dow, hh, mn, ss, ms),
 *		where 'yy' is the year, 'mm' the month, 'dd' the day in the month,
 *		'dow' the number of the day in the week, 'hh' the hour, 'mn' the
 *		minute, 'ss' the second and 'ms' the number of milliseconds.
 *
 *	[Date] (o) -
 *		Unified with the term date(yy, mm, dd, dow).
 *
 *	[Time] (o) -
 *		Unified with the term time(hh, mn, ss, ms).
 *
 * NOTES
 *	filetime fails if the file does not exist or its attributes can not
 *	been retrieved.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

// cv_filetime
AMZIFUNC	cv_filetime ( ENGid	Engine,
			      int	use_date_and_time )
   {
	char				Path    [ MAX_PATH + 1 ] ;
	char				Keyword [ MAX_PATH + 1 ] ;
	char				buffer  [ 256 ] ;
	WIN32_FILE_ATTRIBUTE_DATA	Data ;
	FILETIME *			TimePtr ;
	SYSTEMTIME			FileTime ;
	TERM				term ;


	// Get the parameters 
	lsGetParm ( Engine, 1, cSTR, Path ) ;
	lsGetParm ( Engine, 2, cSTR, Keyword ) ;

	// Get the file attributes
	if  ( ! GetFileAttributesEx ( Path, GetFileExInfoStandard, & Data ) )
		return ( FALSE ) ;

	// What kind of date is requested ?
	if  (      ! stricmp ( Keyword, "creation" )		||
		   ! stricmp ( Keyword, "c" ) )
		TimePtr = & Data. ftCreationTime ;
	else if  ( ! stricmp ( Keyword, "modification" )	||
		   ! stricmp ( Keyword, "m" )			||
		   ! stricmp ( Keyword, "update" )		||
		   ! stricmp ( Keyword, "u" ) )
		TimePtr = & Data. ftLastWriteTime ;
	else if ( ! stricmp ( Keyword, "access" )		||
		  ! stricmp ( Keyword, "a" ) )
		TimePtr = & Data. ftLastAccessTime ;
	else	// Bad keyword
		return ( FALSE ) ;

	// Convert the file time to system time 
	FileTimeToSystemTime ( TimePtr, & FileTime ) ;

	// Unify the result
	if ( use_date_and_time )
	   {
		sprintf ( buffer, "datetime( %d, %d, %d, %d, %d, %d, %d, %d )",
			FileTime. wYear,
			FileTime. wMonth,
			FileTime. wDay,
			FileTime. wDayOfWeek,
			FileTime. wHour,
			FileTime. wMinute,
			FileTime. wSecond,
			FileTime. wMilliseconds ) ;
		lsStrToTerm ( Engine, & term, buffer ) ;
		lsUnifyParm ( Engine, 3, cTERM, & term ) ;
	     }
	else
	   {
		sprintf ( buffer, "date( %d, %d, %d, %d)",
			FileTime. wYear,
			FileTime. wMonth,
			FileTime. wDay,
			FileTime. wDayOfWeek ) ;
		lsStrToTerm ( Engine, & term, buffer ) ;
		lsUnifyParm ( Engine, 3, cTERM, & term ) ;

		sprintf ( buffer, "time( %d, %d, %d, %d)",
			FileTime. wHour,
			FileTime. wMinute,
			FileTime. wSecond,
			FileTime. wMilliseconds ) ;
		lsStrToTerm ( Engine, & term, buffer ) ;
		lsUnifyParm ( Engine, 4, cTERM, & term ) ;
	     }

	return ( TRUE ) ;
    }


// cv_filetime/3
AMZIFUNC	cv_filetime_3 ( ENGid  Engine ) 
   {
	return ( cv_filetime ( Engine, 1 ) ) ;
     }


// cv_filetime/4
AMZIFUNC	cv_filetime_4 ( ENGid  Engine ) 
   {
	return ( cv_filetime ( Engine, 0 ) ) ;
     }


/******/





/****f* CVUtilities.Path/findfiles
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	findfiles/2
 *
 * SYNTAX
 *	findfiles(WildCard, List).
 *
 * PURPOSE
 *	Searches for all the files corresponding to [WildCard] and unifies [List]
 *	with the found file names.
 *
 * ARGUMENTS
 *	[WildCard] (i) -
 *		Wildcard to use for the search.
 *	[List] (o) -
 *		List that will be unified with the list of the file names found.
 *
 * NOTES
 *	findfiles always succeeds.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC	cv_findfiles ( ENGid  Engine ) 
   {
	char *			Names [ 4096 ] ;
	int			NameCount = 0 ;
	char			Wildcard [ MAX_PATH + 1 ] ;
	HANDLE			FindHandle ;
	TERM			List, ListItem ;
	WIN32_FIND_DATA		FindData ;
	register int		i ;


	// Get the wildcard specification and build the list
	lsGetParm ( Engine, 1, cSTR, Wildcard ) ;
	lsMakeList ( Engine, & List ) ;

	// Start the search, put a copy of each entry into the Names array
	FindHandle = FindFirstFile ( Wildcard, & FindData ) ;

	if  ( FindHandle  !=  INVALID_HANDLE_VALUE )	// none found
	    {
		do 
		   {
			Names [ NameCount ++ ] = strdup ( FindData. cFileName ) ;
		    }  while  ( FindNextFile ( FindHandle, & FindData ) ) ;
	      }

	// Create the list
	for  ( i = NameCount - 1 ; i >= 0 ; i -- )
	    {
		lsMakeAtom ( Engine, & ListItem, Names [i] ) ;
		lsPushList ( Engine, & List, ListItem ) ;
	     }

	// All done : unify the second parm with the list, perform memory cleanup and exit
	lsUnifyParm ( Engine, 2, cTERM, & List ) ;

	for  ( i = 0 ; i < NameCount ; i ++ )
		free ( Names [i] ) ;

	return ( TRUE ) ;
    }
/******/




/****f* CVUtilities.Path/fullpath
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	fullpath/2
 *
 * SYNTAX
 *	fullpath(Name, Result).
 *
 * PURPOSE
 *	Unifies [Result] with the full path name of [Path].
 *
 * ARGUMENTS
 *	[Path] (i) -
 *		Path to expand.
 *	
 *	[Result] (o) -
 *		Full path name of [Path].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_getfullpath ( ENGid	Engine )
   {
	char		Filename [ 1024 ],
			FullPath [ 1024 ] ;
	char *		FilePart ;


	// Get path to expand and expand it
	lsGetParm (Engine, 1, cSTR, Filename ) ;
	GetFullPathName( Filename, sizeof (Filename), FullPath, & FilePart ) ;

	// Set parameter 2 to full path
	lsUnifyParm ( Engine, 2, cATOM, FullPath ) ;

	return( TRUE ) ;
     }

/******/






/****f* CVUtilities.Path/getcwd
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	getcwd/1
 *
 * SYNTAX
 *	getcwd(Path).
 *
 * PURPOSE
 *	Unifies [Path] with the contents of the current directory.
 *
 * ARGUMENTS
 *	[Path] (o) -
 *		String that will receive the path of the current directory.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_getcwd ( ENGid	Engine )
   {
	char		Filename [ 1024 ] ;


	// Get current directory
	if ( _getcwd( Filename, sizeof( Filename ) )  ==  NULL )
		return ( FALSE ) ;	

	// Set parameter 1 to current directory
	lsUnifyParm ( Engine, 1, cATOM, Filename ) ;

	return( TRUE ) ;
     }

/******/




/****f* CVUtilities.Path/getlfn
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	getlfn/2
 *
 * SYNTAX
 *	getlfn(ShortPath, LongPath).
 *
 * PURPOSE
 *	Unifies [LongPath] with the long path name corresponding to the short path
 *	name specification [ShortPath].
 *
 * ARGUMENTS
 *	[ShortPath] (i) -
 *		Name to translate in long file name.
 *	[LongPath] (o) -
 *		Unified with the result of the translation. If the translation fails,
 *		[LongPath] will be unified to [ShortPath].
 *
 * NOTES
 *	The getlfn/2 predicate always succeed.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC	cv_getlfn ( ENGid  Engine )
   {
	char		SFN [ MAX_PATH + 1 ],
			LFN [ MAX_PATH + 1 ] ;


	// Get the short file name
	lsGetParm ( Engine, 1, cSTR, SFN ) ;

	// Convert it
	if  ( ! GetLongPathName ( SFN, LFN, sizeof ( LFN ) ) )
		strcpy ( LFN, SFN ) ;

	// Unify the result
	lsUnifyParm ( Engine, 2, cATOM, LFN ) ;

	return ( TRUE ) ;
    }

/******/





/****f* CVUtilities.Path/getsfn
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	getsfn/2
 *
 * SYNTAX
 *	getsfn(LongPath, ShortPath).
 *
 * PURPOSE
 *	Unifies [ShortPath] with the short path name corresponding to the long path
 *	name specification [LongPath].
 *
 * ARGUMENTS
 *	[LongPath] (i) -
 *		Name to translate in short file name.
 *	[ShortPath] (o) -
 *		Unified with the result of the translation. If the translation fails,
 *		[ShortPath] will be unified to [LongPath].
 *
 * NOTES
 *	The getsfn/2 predicate always succeed.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC	cv_getsfn ( ENGid  Engine )
   {
	char		SFN [ MAX_PATH + 1 ],
			LFN [ MAX_PATH + 1 ] ;


	// Get the short file name
	lsGetParm ( Engine, 1, cSTR, LFN ) ;

	// Convert it
	if  ( ! GetShortPathName ( LFN, SFN, sizeof ( SFN ) ) )
		strcpy ( SFN, LFN ) ;

	// Unify the result
	lsUnifyParm ( Engine, 2, cATOM, SFN ) ;

	return ( TRUE ) ;
    }

/******/





/****f* CVUtilities.Path/isxxx functions
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	isdir/1, isempty/1, isexe/1, isfilespec/1, ishtmlfile/1, islfnfilespec/1,
 *	isnetworkpath/1, isrelative/1, isroot/1, issameroot/2, isunc/1, isuncserver/1,
 *	isuncservershare/1, isurl/1
 *
 *
 * PURPOSE
 *	Performs the selected operation on the specified path [Path] :
 *	- isdir :
 *		Checks if [Path] is a directory.
 *	- isempty :
 *		Checks if the specified directory is empty.
 *	- isexe :
 *		Checks if the specified path is an executable file.
 *	- isfilespec :
 *		Checks if the specified path is a file specification, without
 *		any reference to a directory.
 *	- ishtmlfile :
 *		Checks if the specified path is an html file.
 *	- islfnfilespec :
 *		Checks if the specified path is a file specification using the
 *		long file name format.
 *	- isnetworkpath :
 *		Determines if the specified path is a network resource.
 *	- isrelative :
 *		Determines if the specified path is relative.
 *	- isroot :
 *		Checks if the specified path is a root (drive root or network
 *		resource root).
 *	- issameroot :
 *		Checks if [Path1] and [Path2] have the same root.
 *	- isunc :
 *		Checks if the specified path is a UNC path.
 *	- isuncserver :
 *		Checks if the specified path is a UNC path for a server (without
 *		a share name).
 *	- isuncservershare :
 *		Checks if the specified path is a UNC path with a server name
 *		and a share name.
 *	- isurl :
 *		Checks if the specified path is an URL.
 *
 * ARGUMENTS
 *	[Path] (i) -
 *		Path to check.
 *
 * NOTES
 *	The predicate fails if the specified path does not correspond to the 
 *	specified criteria.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

// General type of a Windows PathIsxxx function
typedef  BOOL  ( * isfunction ) ( LPCTSTR  Path ) ;

// Macro to define an isxxx function
# define	ISFUNCTION(Name, WindowsFunc)				\
			AMZIFUNC Name ( ENGid  Engine )			\
			   {						\
				char	Path [ MAX_PATH + 1 ] ;		\
				lsGetParm ( Engine, 1, cSTR, Path ) ;	\
				if ( WindowsFunc ( Path ) )		\
					return ( TRUE ) ;		\
				else					\
					return ( FALSE ) ;		\
			     }

// Declaration of the isxxx functions
ISFUNCTION ( cv_isdir		, PathIsDirectory	)
ISFUNCTION ( cv_isempty		, PathIsDirectoryEmpty	)	/* don't know if the PathIsDirectoryEmpty API really works */
ISFUNCTION ( cv_isfilespec	, PathIsFileSpec	)
ISFUNCTION ( cv_ishtmlfile	, PathIsHTMLFile	)
ISFUNCTION ( cv_islfnfilespec	, PathIsLFNFileSpec	)
ISFUNCTION ( cv_isnetworkpath	, PathIsNetworkPath	)
ISFUNCTION ( cv_isrelative	, PathIsRelative	) 
ISFUNCTION ( cv_isroot		, PathIsRoot		)
ISFUNCTION ( cv_isunc		, PathIsUNC		)
ISFUNCTION ( cv_isuncserver	, PathIsUNCServer	)
ISFUNCTION ( cv_isuncservershare, PathIsUNCServerShare	)
ISFUNCTION ( cv_isurl		, PathIsURL		)



// Evidemment, fallait bien qu'il y aie une fonction PathIsxxx qui accepte
// autre chose qu'un LPCTSTR, histoire de faire ch...
AMZIFUNC cv_isexe ( ENGid  Engine )		
   {	
	char		Path  [ MAX_PATH + 1 ] ;
	wchar_t		WPath [ MAX_PATH + 1 ] ;


	// Get the paramater and convert it to wide char
	lsGetParm ( Engine, 1, cSTR, Path ) ;
	mbstowcs( WPath, Path, sizeof ( WPath ) ) ;

	// Call the function
	if ( PathIsExe ( WPath ) )
		return ( TRUE ) ;
	else
		return ( FALSE ) ;
     }


// issameroot/2 predicate
AMZIFUNC cv_issameroot ( ENGid  Engine )		
   {	
	char		Path1  [ MAX_PATH + 1 ],
			Path2  [ MAX_PATH + 1 ] ;


	// Get the parameters
	lsGetParm ( Engine, 1, cSTR, Path1 ) ;
	lsGetParm ( Engine, 2, cSTR, Path2 ) ;

	// Call the function
	if ( PathIsSameRoot ( Path1, Path2 ) )
		return ( TRUE ) ;
	else
		return ( FALSE ) ;
     }


/******/





/****f* CVUtilities.Path/mkdir
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	mkdir/1
 *
 * SYNTAX
 *	mkdir(Path).
 *
 * PURPOSE
 *	Creates the directory specified by [Path].
 *
 * ARGUMENTS
 *	[Path] (i) -
 *		Directory to create.
 *
 * NOTES
 *	mkdir/1 fails if the specified directory already exist or if an error
 *	occurred while trying to create it.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC	cv_mkdir ( ENGid	Engine ) 
   {
	char		Path [ MAX_PATH + 1 ] ;


	// Get first argument
	lsGetParm ( Engine, 1, cSTR, Path ) ;

	// Remove it 
	if  ( CreateDirectory ( Path, NULL ) ) 
		return ( TRUE ) ;
	else 
		return ( FALSE ) ;
    }

/******/





/****f* CVUtilities.Path/movefile
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	movefile/2, movefile/3
 *
 * SYNTAX
 *	movefile(From, To).
 *	movefile(From, To, FailIfExists).
 *
 * PURPOSE
 *	Moves the file [From] to the file [To]. If [FailIfExists] is set to a 
 *	non-zero value, the predicate will fail when the destination file already
 *	exists.
 *	The movefile/2 predicate calls movefile/3, specifying a zero value
 *	for the [FailIfExists] parameter : in that case, the destination file will
 *	be overwritten if it already exists.
 *
 * ARGUMENTS
 *	[From] (i) -
 *		File to move.
 *
 *	[To] (i) -
 *		Destination file.
 *
 *	[FailIfExists] (i) -
 *		If set to non-zero and the file specified by [To] already exists, the
 *		predicate will fail. Otherwise (zero value), the destination
 *		file will be overwritten.
 *
 * NOTES
 *	The predicate fails if an error occurred or if the destination file already
 *	exists and [FailIfExists] is set to zero.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

// movefile/3 predicate
AMZIFUNC cv_movefile_3 ( ENGid	Engine )
   {
	char		From [ MAX_PATH + 1 ],
			To   [ MAX_PATH + 1 ] ;
	int		FailIfExists ;
	DWORD		Options ;


	// Get the three arguments
	lsGetParm ( Engine, 1, cSTR, From ) ;
	lsGetParm ( Engine, 2, cSTR, To ) ;
	lsGetParm ( Engine, 3, cINT, & FailIfExists ) ;

	// Adjust the boolean flag
	Options  = ( FailIfExists ) ? 0 : MOVEFILE_REPLACE_EXISTING ;
	Options |= MOVEFILE_COPY_ALLOWED ;

	// Copy the file
	if  ( MoveFileEx ( From, To, Options ) ) 
		return ( TRUE ) ;
	else
		return ( FALSE ) ;
    }



// movefile/2 predicate
AMZIFUNC cv_movefile_2 ( ENGid	Engine )
   {
	char		From [ MAX_PATH + 1 ],
			To   [ MAX_PATH + 1 ] ;


	// Get the two arguments
	lsGetParm ( Engine, 1, cSTR, From ) ;
	lsGetParm ( Engine, 2, cSTR, To ) ;

	// Copy the file
	if  ( MoveFileEx ( From, To, MOVEFILE_COPY_ALLOWED | MOVEFILE_REPLACE_EXISTING ) ) 
		return ( TRUE ) ;
	else
		return ( FALSE ) ;
    }

/******/




/****f* CVUtilities.Path/rm
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	rm/1
 *
 * SYNTAX
 *	rm(Path).
 *
 * PURPOSE
 *	Deletes the file specified by [Path].
 *
 * ARGUMENTS
 *	[Path] (i) -
 *		File to remove.
 *
 * NOTES
 *	rm/1 fails if the file does not exist or if an error occurs while trying
 *	to delete the file.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC	cv_removefile ( ENGid	Engine ) 
   {
	char		Path [ MAX_PATH + 1 ] ;


	// Get first argument
	lsGetParm ( Engine, 1, cSTR, Path ) ;

	// Remove it 
	if  ( DeleteFile ( Path ) ) 
		return ( TRUE ) ;
	else 
		return ( FALSE ) ;
    }

/******/





/****f* CVUtilities.Path/rmdir
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	rmdir/1
 *
 * SYNTAX
 *	rmdir(Path).
 *
 * PURPOSE
 *	Removes the directory specified by [Path].
 *
 * ARGUMENTS
 *	[Path] (i) -
 *		Directory to remove.
 *
 * NOTES
 *	rmdir/1 fails if the specified directory does not exist or if an error
 *	occurred while trying to remove it (for example, if it is not empty).
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC	cv_rmdir ( ENGid	Engine ) 
   {
	char		Path [ MAX_PATH + 1 ] ;


	// Get first argument
	lsGetParm ( Engine, 1, cSTR, Path ) ;

	// Remove it 
	if  ( RemoveDirectory ( Path ) ) 
		return ( TRUE ) ;
	else 
		return ( FALSE ) ;
    }

/******/





/****f* CVUtilities.Path/path analysis functions
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	splitpath/5, splitpath/6
 *	pathdir/2, pathdrive/2, pathext/2, pathfilename/2, pathname/2
 *
 * SYNTAX
 *	splitpath(Path, Drive, Dir, Name, Ext).
 *	splitpath(Path, Drive, Dir, Name, Ext, FName).
 *	pathdir(Path, Dir).
 *	pathdrive(Path, Drive).
 *	pathext(Path, Ext).
 *	pathfilename(Path, FName).
 *	pathname(Path, Name).
 *
 * PURPOSE
 *	Splits the path specified by [Path] is several components : Drive letter 
 *	[Drive], directory [Dir], file name without extension [Name] and
 *	extension [Ext].
 *	The splitpath/6 version also unifies [FName] with the [Name] and [Extension].
 *	The other pathxxx function unify their second argument with the corresponding
 *	part of the path specified by [Path] : pathdir/2 returns the directory part,
 *	pathdrive/2 the drive letter, pathext/2 the extension, pathfilename/2 the
 *	filename (name + extension) and pathname/2 the name (name without extension).
 *
 * ARGUMENTS
 *	[Path] (i) -
 *		Path to split.
 *	[Drive] (o) -
 *		Unified with the drive letter specified in [Path].
 *	[Dir] (o) -
 *		Unified with the directory parth of [Path].
 *	[Name] (o) -
 *		Unified with the filename part of the path (without directory and
 *		extension).
 *	[Ext] (o] -
 *		Unified with the extension of the specified path.
 *	[FName] (o) -
 *		Unified with the filename + extension parts.
 *
 * NOTES
 *	splitpath always succeed.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */


// splitpath
AMZIFUNC	cv_splitpath   ( ENGid	Engine,
				 int	GetParm2,
				 int	GetParm3,
				 int	GetParm4,
				 int    GetParm5,
				 int	GetParm6 ) 
   {
	char		Path  [ MAX_PATH + 1 ],
			Drive [ MAX_PATH + 1 ],
			Dir   [ MAX_PATH + 1 ],
			Name  [ MAX_PATH + 1 ],
			Ext   [ MAX_PATH + 1 ] ;


	// Get first argument
	lsGetParm ( Engine, 1, cSTR, Path ) ;

	// Call the splitpath function
	_splitpath( Path, Drive, Dir, Name, Ext ) ;

	// Unify the parameters
	if  ( GetParm2 )
		lsUnifyParm ( Engine, GetParm2, cSTR, Drive ) ;
	
	if  ( GetParm3 )
		lsUnifyParm ( Engine, GetParm3, cSTR, Dir ) ;

	if  ( GetParm4 )
		lsUnifyParm ( Engine, GetParm4, cSTR, Name ) ;

	if  ( GetParm5 )
		lsUnifyParm ( Engine, GetParm5, cSTR, Ext ) ;

	if  ( GetParm6 )
	   {
		char		FName [ MAX_PATH + 1 ] ;

	
		sprintf ( FName, "%s%s", Name, Ext ) ;
		lsUnifyParm ( Engine, GetParm6, cSTR, FName ) ;
	     }

	return ( TRUE ) ;
    }


// splitpath/6
AMZIFUNC	cv_splitpath_6 ( ENGid	Engine ) 
   {
	return ( cv_splitpath ( Engine, 2, 3, 4, 5, 6 ) ) ;
     }


// splitpath/5
AMZIFUNC	cv_splitpath_5 ( ENGid	Engine ) 
   {
	return ( cv_splitpath ( Engine, 2, 3, 4, 5, 0 ) ) ;
     }


// pathdir
AMZIFUNC	cv_pathdir ( ENGid  Engine )
   {
	return ( cv_splitpath ( Engine, 0, 2, 0, 0, 0 ) ) ;
     }


// pathdrive
AMZIFUNC	cv_pathdrive ( ENGid  Engine )
   {
	return ( cv_splitpath ( Engine, 2, 0, 0, 0, 0 ) ) ;
     }


// pathext
AMZIFUNC	cv_pathext ( ENGid  Engine )
   {
	return ( cv_splitpath ( Engine, 0, 0, 0, 2, 0 ) ) ;
     }


// pathfilename
AMZIFUNC	cv_pathfilename ( ENGid  Engine )
   {
	return ( cv_splitpath ( Engine, 0, 0, 0, 0, 2 ) ) ;
     }


// pathname
AMZIFUNC	cv_pathname ( ENGid  Engine )
   {
	return ( cv_splitpath ( Engine, 0, 0, 0, 0, 2 ) ) ;
     }

	
/******/





/****f* CVUtilities.Path/rename
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	rename/2
 *
 * SYNTAX
 *	rename(Old, New).
 *
 * PURPOSE
 *	Renames the file [Old] into [New].
 *
 * ARGUMENTS
 *	[Old] (i) -
 *		File to rename.
 *	[New] (i) -
 *		New name for the file.
 *
 * NOTES
 *	rename/2 fails if the rename operation failed.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC	cv_rename ( ENGid  Engine )
   {
	char		Old [ MAX_PATH + 1 ],
			New [ MAX_PATH + 1 ] ;


	// Get the parameters
	lsGetParm ( Engine, 1, cSTR, Old ) ;
	lsGetParm ( Engine, 2, cSTR, New ) ;


	// Call the function
	if  ( MoveFile ( Old, New ) )
		return ( TRUE ) ;
	else
		return ( FALSE ) ;
    }

/******/





/****f* CVUtilities.Path/renext
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	renext/3
 *
 * SYNTAX
 *	renext(File, New, Result).
 *
 * PURPOSE
 *	Replaces the extension part of [File] with [New] and unifies [Result] with
 *	the result.
 *
 * ARGUMENTS
 *	[File] (i) -
 *		File whose extension is to be renamed.
 *	[New] (i) -
 *		New extension for the file.
 *	[Result] (i) -
 *		Result of the operation.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC	cv_renext ( ENGid  Engine )
   {
	char		File [ MAX_PATH + 1 ],
			New  [ MAX_PATH + 1 ] ;


	// Get the parameters
	lsGetParm ( Engine, 1, cSTR, File ) ;
	lsGetParm ( Engine, 2, cSTR, New ) ;


	// Call the function
	if  ( PathRenameExtension ( File, New )  )
	   {
		lsUnifyParm ( Engine, 3, cSTR, File ) ;
		return ( TRUE ) ;
	     }
	else
		return ( FALSE ) ;
    }

/******/




/****f* CVUtilities.Path/tempfile
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	tempfile/4, tempfile/3, tempfile/2
 *
 * SYNTAX
 *	tempfile( Directory, Prefix, UniqueID, Filename ).
 *	tempfile( Prefix, UniqueID, Filename ).
 *	tempfile( Prefix, Filename ).
 *
 * PURPOSE
 *	Unifies [Filename] with a temp file name. [Directory] is the directory 
 *	part of the file name (or the system temp directory if not specified). 
 *	[UniqueID] is a unique id for the generated filename. Specify 0 if 
 *	tempfile() is to search for the next available id.
 *	[Prefix] is the prefix used for the temp file name (first three characters
 *	only).
 *
 * ARGUMENTS
 *	[Directory] (i) -
 *		Directory for the temp file name. tempfile/3 and tempfile/2 use the
 *		default temp directory.
 *
 *	[Prefix] (i) -
 *		Prefix to use for generating the filename. Only the first three 
 *		characters will be taken.
 *
 *	[UniqueID] (i) -
 *		Unique ID to use when generating the temp file name.
 *		If zero, tempfile will search in the specified directory for the
 *		next available unique ID.
 *		tempfile/2 uses zero for [UniqueID].
 *
 *	[Filename] (o) -
 *		Unified to the generated temp file name.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

// Entry point for all version of tempfile
AMZIFUNC cv_gettempfile ( ENGid		Engine,
			  int		TempDirArg,
			  int		PrefixArg,
			  int		IDArg,
			  int		FilenameArg )
   {
	char		TempDir  [ MAX_PATH + 1 ],
			Prefix   [ MAX_PATH + 1 ],
			TempFile [ MAX_PATH + 1 ] ;
	int		UniqueID ;


	// Get the temp directory
	if ( TempDirArg )
		lsGetParm ( Engine, TempDirArg, cSTR, TempDir ) ;
	else
		GetTempPath ( sizeof ( TempDir ), TempDir ) ;

	// Get the Prefix for temp file
	lsGetParm ( Engine, PrefixArg, cSTR, Prefix ) ;

	// Get the unique ID
	if ( IDArg )
		lsGetParm ( Engine, IDArg, cINT, & UniqueID ) ;
	else
		UniqueID = 0 ;

	// Generate the file name
	GetTempFileName ( TempDir, Prefix, UniqueID, TempFile ) ;

	// Unify the result
	lsUnifyParm ( Engine, FilenameArg, cATOM, TempFile ) ;
	return ( TRUE ) ;
    }


// tempfile/4
AMZIFUNC cv_gettempfile_4 ( ENGid	Engine ) 
   {
	return ( 
		cv_gettempfile ( Engine, 1, 2, 3, 4 ) 
		) ;
     }


// tempfile/3
AMZIFUNC cv_gettempfile_3 ( ENGid	Engine ) 
   {
	return ( 
		cv_gettempfile ( Engine, 0, 1, 2, 3 ) 
		) ;
     }


// tempfile/2
AMZIFUNC cv_gettempfile_2 ( ENGid	Engine ) 
   {
	return ( 
		cv_gettempfile ( Engine, 0, 1, 0, 2 ) 
		) ;
     }

/******/




/****f* CVUtilities.System/temppath
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	temppath/1
 *
 * SYNTAX
 *	temppath(Value).
 *
 * PURPOSE
 *	Unifies [Value] with the current temp directory path.
 *
 * ARGUMENTS
 *	[Value] (o) - 
 *		Unified with the contents of the current temp directory.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC cv_gettemppath ( ENGid	Engine )
   {
	char		Buffer [ MAX_PATH + 1 ] ;


	GetTempPath ( sizeof ( Buffer ), Buffer ) ;
	lsUnifyParm ( Engine, 1, cATOM, Buffer ) ;

	return ( TRUE ) ;
    }

/******/