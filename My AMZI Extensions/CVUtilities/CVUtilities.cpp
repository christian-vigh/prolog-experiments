/****h* CVUtilities/General helper functions
 ===============================================================================
 *
 * NAME
 *	General - General helper functions for extended predicates.
 *
 * FILE
 *	CVUtilities.cpp
 *
 * CONTENTS
 *	Contents a set of functions :
 *	. To initialize the AMZI Engine and declare extended predicates
 *	. To provide utility functions to extended predicates
 *
 * AUTHOR
 *	Christian Vigh, February 2007.
 *
 * HISTORY
 *	[Version]  [Date]	[Contents]
 *	  1.00	   11/02/2007	Initial version.	   
 *
 ===============================================================================
 ******/
# include	<stdarg.h>
# include	<string.h>

# include	"CVUtilities.h"



/*-------------------------------------------------------------------------------
 
	Global variables.

  -------------------------------------------------------------------------------*/

HINSTANCE		DLLInstance ;
char			DLLPath  [ MAX_PATH + 1 ] ;
char			HelpPath [ MAX_PATH + 1 ] ;
PrologHelp *		Help ;



/****if* CVUtilities.General/Extended predicates
 -------------------------------------------------------------------------------
 *
 * DESCRIPTION
 *	The extended_predicate tables contains the list of extended predicates 
 *	that will be passed to the lsInitPreds AMZI function.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC	cv_list_0		( ENGid		Engine ) ;


BEGIN_PREDICATE_TABLE(extended_predicates)

	// General predicates
	PREDICATE("help"		, 1, cv_help_1		),
	PREDICATE("modlist"		, 0, cv_modlist_0	),
	PREDICATE("module"		, 1, cv_module_1	),
	PREDICATE("module"		, 0, cv_module_0	),
	PREDICATE("usage"		, 1, cv_usage_1		),
	PREDICATE("where"		, 1, cv_where_1		),

	// File Files.Cpp
	PREDICATE("string_file"		, 2, cv_string_file	),

	// File Math.Cpp
	PREDICATE("fact"		, 2, cv_fact		),
	PREDICATE("comb"		, 3, cv_comb		),
	PREDICATE("perm"		, 3, cv_perm		),

	// File Path.Cpp
	PREDICATE("cd"			, 1, cv_chdir		),
	PREDICATE("copyfile"		, 2, cv_copyfile_2	),
	PREDICATE("copyfile"		, 3, cv_copyfile_3	),
	PREDICATE("delext"		, 2, cv_delext		),
	PREDICATE("fileexists"		, 1, cv_fileexists	),
	PREDICATE("fileflag"		, 2, cv_fileflag	),
	PREDICATE("filematch"		, 2, cv_filematch	),
	PREDICATE("filesize"		, 2, cv_filesize	),
	PREDICATE("filetime"		, 3, cv_filetime_3	),
	PREDICATE("filetime"		, 4, cv_filetime_4	),
	PREDICATE("findfiles"		, 2, cv_findfiles	),
	PREDICATE("fullpath"		, 2, cv_getfullpath	),
	PREDICATE("getcwd"		, 1, cv_getcwd		),
	PREDICATE("getlfn"		, 2, cv_getlfn		),
	PREDICATE("getsfn"		, 2, cv_getsfn		),
	PREDICATE("isdir"		, 1, cv_isdir		),
	PREDICATE("isdirempty"		, 1, cv_isempty		),
	PREDICATE("isexe"		, 1, cv_isexe		),
	PREDICATE("isfilespec"		, 1, cv_isfilespec	),
	PREDICATE("ishtmlfile"		, 1, cv_ishtmlfile	),
	PREDICATE("islfnfilespec"	, 1, cv_islfnfilespec	),
	PREDICATE("isnetworkpath"	, 1, cv_isnetworkpath	),
	PREDICATE("isrelative"		, 1, cv_isrelative	),
	PREDICATE("isroot"		, 1, cv_isroot		),
	PREDICATE("issameroot"		, 2, cv_issameroot	),
	PREDICATE("isunc"		, 1, cv_isunc		),
	PREDICATE("isuncserver"		, 1, cv_isuncserver	),
	PREDICATE("isuncshare"		, 1, cv_isuncservershare),
	PREDICATE("isurl"		, 1, cv_isurl		),
	PREDICATE("mkdir"		, 1, cv_mkdir		),
	PREDICATE("movefile"		, 2, cv_movefile_2	),
	PREDICATE("movefile"		, 3, cv_movefile_3	),
	PREDICATE("pathdir"		, 2, cv_pathdir		),
	PREDICATE("pathdrive"		, 2, cv_pathdrive	),
	PREDICATE("pathext"		, 2, cv_pathext		),
	PREDICATE("pathfilename"	, 2, cv_pathfilename	),
	PREDICATE("pathname"		, 2, cv_pathname	),
	PREDICATE("rename"		, 2, cv_rename		),
	PREDICATE("renext"		, 3, cv_renext		),
	PREDICATE("rm"			, 1, cv_removefile	),
	PREDICATE("rmdir"		, 1, cv_rmdir		),
	PREDICATE("splitpath"		, 5, cv_splitpath_5	),
	PREDICATE("splitpath"		, 6, cv_splitpath_6	),
	PREDICATE("tempfile"		, 4, cv_gettempfile_4	),
	PREDICATE("tempfile"		, 3, cv_gettempfile_3	),
	PREDICATE("tempfile"		, 2, cv_gettempfile_2	),
	PREDICATE("temppath"		, 1, cv_gettemppath	),

	// File Strings.Cpp
	PREDICATE("string_index"	, 3, cv_string_index	),
	PREDICATE("string_index"	, 4, cv_string_index_start),

	// File System.cpp
	PREDICATE("computername"	, 1, cv_getcomputername ),
	PREDICATE("getenv"		, 2, cv_getenv		),
	PREDICATE("getenvlist"		, 1, cv_getenvlist	),
	PREDICATE("setenv"		, 2, cv_setenv		),
	PREDICATE("username"		, 1, cv_getusername	),

/***
AMZIFUNC	cv_movefile_3		( ENGid		EngineId ) ;
AMZIFUNC	cv_movefile_2		( ENGid		EngineId ) ;
AMZIFUNC	cv_setenv		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathdir		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathdrive		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathext		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathfilename		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathmakepretty	( ENGid		EngineId ) ;
AMZIFUNC	cv_pathmatchspec	( ENGid		EngineId ) ;
AMZIFUNC	cv_pathname		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathremoveext	( ENGid		EngineId ) ;
AMZIFUNC	cv_pathrenameext	( ENGid		EngineId ) ;
AMZIFUNC	cv_pathsplit		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathunexpandenv	( ENGid		EngineId ) ;
***/

	// File Windows.Cpp
	PREDICATE("sysmetric"		, 2, cv_getsystemmetric ),
	PREDICATE("message"		, 1, cv_message_1	),
	PREDICATE("message"		, 2, cv_message_2	),
	PREDICATE("message"		, 4, cv_message_4	),


END_PREDICATE_TABLE ;

/******/




/****if* CVUtilities.General/DLLMain
 -------------------------------------------------------------------------------
 *
 * FUNCTION
 *	DllMain
 *
 * SYNTAX
 *	BOOL WINAPI DllMain (HINSTANCE	Instance, 
 *			     DWORD	Reason,
 *			     LPVOID	Result) ;
 *
 * PURPOSE
 *	Initializes the DLL. Reads the help file (HELP_FILE in the DLL dir).
 *
 * NOTES
 *	The parameters are not checked.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

BOOL WINAPI DllMain(HINSTANCE Instance, DWORD Reason, LPVOID Result)
   {
	register char *		p ;


	switch ( Reason )
	   {
		case  DLL_PROCESS_ATTACH :
			// Get the DLL filename and strip the name part
			GetModuleFileName ( Instance, DLLPath, sizeof ( DLLPath ) ) ;
			p = strrchr ( DLLPath, '\\' ) ;

			if  ( p  !=  NULL ) 
				* p = 0 ;

			// Save the current instance
			DLLInstance = Instance ;

			// Load the help file
			sprintf ( HelpPath, "%s\\%s", DLLPath, HELPFILE ) ;
			Help = new PrologHelp ( HelpPath ) ;

		case  DLL_PROCESS_DETACH :
		case  DLL_THREAD_ATTACH :
		case  DLL_THREAD_DETACH :
			break ;

		default :
			break ;
	     }

	return (TRUE) ;
     }

/******/




/****if* CVUtilities.General/InitPreds
 -------------------------------------------------------------------------------
 *
 * FUNCTION
 *	InitPreds
 *
 * SYNTAX
 *	InitPreds( ENGid  eid, void *  p ) ;
 *
 * PURPOSE
 *	Adds the CVUtilities extended predicates to Amzi logicbase.	
 *
 * ARGUMENTS
 *	ENGid	engine (i) -
 *		AMZI Server id.
 *
 *	void *	p (i) -
 *		Not used.
 *
 * RETURN VALUE
 *	This function always returns 0.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
DLLENTRYPOINT	InitPreds ( ENGid  eid, void *  p )
   {
	RC		rc ;
	characters	buffer [128] ;


	// Perform the initialization
	rc = lsInitPreds ( eid, extended_predicates );


	// Something wrong ?
	if (rc)
	    {
		sprintf( buffer, TEXTSTRING( "Error #%d while loading the predicates from module %s." ),
				rc, LSXNAME ) ;
		MessageBox ( NULL, buffer, LSXNAME, MB_OK);
	     }
   
	return (0) ;
    }

/******/




/****f* CVUtilities.General/xlist
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	xlist/0, xlist/1
 *
 * SYNTAX
 *	xlist.
 *	xlist(Predicate).
 *
 * PURPOSE
 *	Displays the predicates defined in the CVUtilities library if no argument
 *	is given, or displays the help for the specified predicate.
 *
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC  cv_list_0 ( ENGid  Engine ) 
   {
	char		buffer [ 256 ] ;
	int		i ;
	TERM		dummy ;


// Cycle through the predicates
	for ( i = 0 ; extended_predicates [i]. Pname  !=  NULL ; i ++ )
	   {

	// Write the string "predicate/arity"
		sprintf ( buffer, "write(%s/%d)", 
				extended_predicates [i]. Pname,
				extended_predicates [i]. Parity ) ;
		
		lsExecStr ( Engine, & dummy, buffer ) ;

	// End of list or not ?
		if  ( extended_predicates [i+1]. Pname  ==  NULL )	
			lsExecStr ( Engine, & dummy, "nl" ) ;
		else
			lsExecStr ( Engine, & dummy, "write(', ')" ) ;

	     }

	return ( TRUE ) ;
    }

/******/






/****if* CVUtilities.General/DebugMessage
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	DebugMessage
 *
 * SYNTAX
 *	void DebugMessage ( char *	Title,
 *			    char *	Message,
 *			    ... ) ;
 *
 * PURPOSE
 *	Displays a message box. This function is used for debugging purposes.
 *
 * ARGUMENTS
 *	char *	Title (i) -
 *		Title of the message box to display.
 *
 *	char *	Message (i) -
 *		Message to display. This message can hold modifiers recognized 
 *		by the sprintf() function.
 *
 *	... -
 *		Additional arguments to print.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
void	DebugMessage( char *	Title,
		      char *	Message,
		      ... )
   {
	char		buffer [4096] ;
	va_list		list ;


	va_start ( list, Message ) ;
	vsprintf( buffer, Message, list ) ;
	va_end ( list ) ;

	MessageBox ( NULL, buffer, Title, MB_OK ) ;
     }

/******/





/****f* CVUtilities.General/association_name
 -------------------------------------------------------------------------------
 *
 * FUNCTION
 *	association_name, association_value, association_bits
 *
 * SYNTAX
 *	char *  association_name  ( Association *	table,
 *				    UINT		assoc_value ) ;
 *	UINT	association_bits  ( Association *	table,
 *				    ENGid		Engine,
				    TERM		term ) ;
 *	UINT	association_value ( Association *	table,
 *				    char *		assoc_name ) ;
 *
 * PURPOSE
 *	Developing wrapper for Windows functions sometimes require to map from 
 *	Windows constants to Prolog atoms, and vice-versa. For example, the
 *	4th parameter of the Windows function MessageBox is a combination of MB_xx
 *	constants ; and it returns an IDyy value (for example, IDOK, YESCANCEL, 
 *	etc.).
 *
 *	The association_* functions are here to facilitate such a mapping ; they
 *	all take an array of associations (Name/Value pairs) and perform the 
 *	mapping :
 * 
 *	- association_name() will return the string name of the specified 
 *	  constant, assoc_value. It can be used to convert a constant value to
 *	  a string which can later be used to unify with a prolog term ; for example :
 *
 *		Result = MessageBox(NULL, "caption", "message", MB_OKCANCEL) ;
 *		ResultStr = association_name ( MBValues, Result ) ;
 *
 *	  will assign to ResultStr the string corresponding to the return value
 *	  of the MessageBox function (in this case, either "IDOK" or "IDCANCEL").
 *
 *	  MBValues is an array of associations (Name/Value pairs) that is used for
 *	  the mapping.
 *
 *	  An association table is defined using the predefined macros BEGIN_ASSOCIATION,
 *	  ASSOCIATION and END_ASSOCIATION. A typical association table for the Windows
 *	  MessageBox function would be :
 *
 *		BEGIN_ASSOCIATION( MBValues )
 *			ASSOCIATION("IDOK"	, IDOK),
 *			ASSOCIATION("IDCANCEL"	, IDCANCEL),
 *			(etc.)
 *		END_ASSOCIATION ;
 *
 *	- association_value performs the reverse operation with respects to
 *	  association_name : it returns the string value corresponding to an integer
 *	  constant.
 *
 *	- association_bits is used to convert between a Prolog list of association
 *	  values (in string format) and performs a logical OR to form a flag set.
 *	  This function can be used to convert a Prolog list such as :
 *
 *		[mb_ok, mb_iconexclamation]
 *
 *	  to a flag set that will be passed as a parameter to the MessageBox function.
 *
 * ARGUMENTS
 *	Association *	table (i) -
 *		Table to scan for the operation.
 *
 *	char *		assoc_name (i) -
 *		Name of the constant whose value is to be retrieved.
 *
 *	UINT		assoc_value (i) -
 *		Value of the constant whose name is to be retrieved.
 *		
 *	ENGid		Engine (i) -
 *		AMZI Server engine ID.
 *
 *	TERM		Term (i) -
 *		Prolog list containing the values to retrive and combine.
 *
 * RETURN VALUE
 *	association_name returns the string representing the given constant 
 *	[assoc_value].
 *	association_value returns the value corresponding to the given string
 *	[assoc_name].
 *	association_bits returns the value corresponding to the combination
 *	of constant strings specified in the prolog list [Term].
 *
 * NOTES
 *	Comparisons are not case-sensitive, thus "idok" is the same as "IDOK".
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

// Retrieve association value
UINT association_value ( Association *	assoc,
			 char *		name ) 
   {
	register Association *	p ;


	for ( p = assoc ; p -> ConstantName  !=  NULL ; p ++ )
	    {
		if ( ! stricmp ( p -> ConstantName, name ) )
			return ( p -> ConstantValue ) ;
	     }

	return ( 0 ) ;
     }


// Retrieve association name
char * association_name  ( Association *	assoc,
			   UINT 		value ) 
   {
	register Association *	p ;


	for ( p = assoc ; p -> ConstantName  !=  NULL ; p ++ )
	    {
		if ( p -> ConstantValue  ==  value )
			return ( p -> ConstantName ) ;
	     }

	return ( NULL ) ;
     }



// Retrieve a combined bits value
UINT   association_bits  ( Association *	assoc,
			   ENGid		engine,
			   TERM			term ) 
   {
	char			Value [ 512 ] ;
	unsigned int		Flags = 0 ;
	register int		status ;


	// Get first list element
	status = lsPopList ( engine, & term, cSTR, Value ) ;

	// Then cycle through the list until last element has been processed
	while ( status  ==  OK )
	   {
		// Get the flag value
		Flags |= association_value ( assoc, Value ) ;

		// Process next item
		status = lsPopList ( engine, & term, cSTR, Value ) ;
	     }

	return ( Flags ) ;
     }




/****if* CVUtilities.General/check_help_loaded
 -------------------------------------------------------------------------------
 *
 * FUNCTION
 *	check_help_loaded
 *
 * SYNTAX
 *	int	check_help_loaded ( ENGid	Engine )
 *
 * PURPOSE
 *	Checks if the help file has been successfully loaded.
 *
 * ARGUMENTS
 *	ENGid	Engine -
 *		AMZI engine id to print an error message if the help file is not
 *		found.
 *
 * RETURN VALUE
 *	check_help_loaded returns TRUE if the help file has been successfully
 *	loaded, or FALSE otherwise.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

int	check_help_loaded ( ENGid	Engine )
   {
	char		buffer [ 1024 ] ;
	TERM		Cmd ;


	if ( Help -> Succeeded ( )  ==  FALSE )
	   {
		sprintf ( buffer, "write('Help file ''%s'' not found.'), nl", HELPFILE ) ;
		lsExecStr ( Engine, & Cmd, buffer ) ;
		lsExecStr ( Engine, & Cmd, "nl" ) ;
		return ( FALSE ) ;
	     }
	else
		return ( TRUE ) ;
     }

/******/




/****if* CVUtilities.General/prolog_printf
 -------------------------------------------------------------------------------
 *
 * FUNCTION
 *	prolog_printf
 *
 * SYNTAX
 *	void	prolog_printf ( ENGid		Engine,
 *				char *		format, 
 *				... ) 
 *
 * PURPOSE
 *	Writes a formatted string to the AMZI Prolog output window. The format
 *	used is the same as printf().
 *
 * ARGUMENTS
 *	ENGid	Engine -
 *		AMZI Prolog engine id.
 *	char *	format -
 *		Format used for printing.
 *	...
 *		Additional arguments for the format string.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

void	prolog_printf ( ENGid		Engine,
		        char *		format, 
		        ... ) 
   {
	char		buffer [4096] ;
	va_list		list ;
	TERM		Write ;


	va_start ( list, format ) ;
	vsprintf( buffer, format, list ) ;
	va_end ( list ) ;

	// Don't call ExecStr directly because we would have problems with quoted
	// strings ; call lsMakeFA and lsUnifyArg instead before calling lsExec
	lsMakeFA ( Engine, & Write, "write", 1 ) ;
	lsUnifyArg ( Engine, & Write, 1, cATOM, buffer ) ;
	lsExec ( Engine, & Write ) ;

	//lsExecStr ( Engine, & Cmd, "nl" ) ;
     }

/******/




/****if* CVUtilities.General/print_module_entries
 -------------------------------------------------------------------------------
 *
 * FUNCTION
 *	print_module_entries
 *
 * SYNTAX
 *	void  print_module_entries ( ENGid	Engine, 
 *				     int	module_index,
 *				     Module *   module )
 *
 * PURPOSE
 *	Prints the predicate help defined by [module_index].
 *
 * ARGUMENTS
 *	ENGid		Engine -
 *		AMZI Prolog engine id.
 *	int		module_index -
 *		Index of the module whose entries are to be printed.
 *	Module *	module -
 *		Module description.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

void  print_module_entries ( ENGid	Engine, 
			     int	module_index,
			     Module *   module )
   { 
	register int		i, length ;
	register char *		p ;
	char			buffer [ 256 ] ;


	// Print header
	sprintf ( buffer, "Module %s (%s) :\n", 
		module -> Name, module -> Description ) ;	
	prolog_printf ( Engine, buffer ) ;


	// ... underlined by a string of '-' characters
	length = strlen ( buffer ) - 3 ;

	for  ( p = buffer, i = 0 ; i < length ; i ++, p ++ )
		* p = '-' ;
	* p = 0 ;

	// Print it
	prolog_printf ( Engine, "%s\n", buffer ) ;

	// Print predicates
	for  ( i = 0 ; i < Help -> GetPredicateCount ( module_index ) ; i ++ )
	   {
		Predicate *	predicate = Help -> GetPredicate ( module_index, i ) ;

		sprintf ( buffer, "%s/%d", predicate -> Name, predicate -> Arity ) ;
		prolog_printf ( Engine, "%-22s  %s\n", buffer, predicate -> ShortDescription ) ;
	      }
    }		
	
/******/




/****f* CVUtilities.CVUtilities/modlist
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	modlist/0
 *
 * SYNTAX
 *	modlist.
 *
 * PURPOSE
 *	Prints the module list.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
AMZIFUNC  cv_modlist_0 ( ENGid   Engine )
   {
	register int	i ;


	// Check if help is loaded
	if ( ! check_help_loaded ( Engine ) )
		return ( FALSE ) ;

	// Print header
	prolog_printf ( Engine, "List of available modules :\n" ) ;
	prolog_printf ( Engine, "-------------------------\n" ) ;

	// Cycle through module list
	for ( i = 0 ; i < Help -> GetModuleCount ( ) ; i ++ )
	   {
		Module *	module = Help -> GetModule ( i ) ;

		prolog_printf ( Engine, "%-20s  %s\n", 
			module -> Name, module -> Description ) ;	
		delete  module ;
	    }

	return ( TRUE ) ;
    }

/******/




/****f* CVUtilities.CVUtilities/module
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	module/0, module/1
 *
 * SYNTAX
 *	module.
 *	module(Module).
 *
 * PURPOSE
 *	The module predicate displays the list of available predicates in the 
 *	specified module (module/1 version) or in all modules (module/0 version).
 *
 * ARGUMENTS
 *	[Module]  (i)  -
 *		Name of the module whose predicates are to be printed.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

// module/1 version
AMZIFUNC  cv_module_1 ( ENGid   Engine )
   {
	register int	i ;
	char		module_name [ 256 ] ;


	// Check if help file is loaded
	if ( ! check_help_loaded ( Engine ) )
		return ( FALSE ) ;

	// Get module name
	lsGetParm ( Engine, 1, cSTR, module_name ) ;

	for ( i = 0 ; i < Help -> GetModuleCount ( ) ; i ++ )
	   {
		Module *	module = Help -> GetModule ( i ) ;

		
		// Module has been found : print it
		if  ( ! stricmp ( module_name, module -> Name ) )
			print_module_entries ( Engine, i, module ) ;

		delete  module ;
	     }

	return ( TRUE ) ;
    }



// module/0 version
AMZIFUNC  cv_module_0 ( ENGid   Engine )
   {
	register int	i ;


	// Check if help file is loaded
	if ( ! check_help_loaded ( Engine ) )
		return ( FALSE ) ;

	// Cycle through the modules
	for ( i = 0 ; i < Help -> GetModuleCount ( ) ; i ++ )
	   {
		Module *	module = Help -> GetModule ( i ) ;

		// Print entry
		print_module_entries ( Engine, i, module ) ;
		delete  module ;
	     }

	return ( TRUE ) ;
    }

/******/




/****f* CVUtilities.CVUtilities/help
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	help/1
 *
 * SYNTAX
 *	help(predicate)
 *
 * PURPOSE
 *	Displays full help about the specified predicate.
 *
 * ARGUMENTS
 *	[Predicate] (i) -
 *		Predicate whose help is to be printed.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC  cv_help_1 ( ENGid   Engine )
   {
	register int	i, j ;
	Argument *	ap ;
	char		predicate_name	     [ 256 ] ;
	char		predicate_name_arity [ 256 ] ;
	TERM		FA ;


	// Check if help file is loaded
	if ( ! check_help_loaded ( Engine ) )
		return ( FALSE ) ;

	// We can specify :
	//	help(term).
	//	help('term/arity')
	//	help(term/arity)
	// In the latter case, we receive a term instead of an atom, so we must
	// convert it to a string
	if  ( lsGetParm ( Engine, 1, cSTR, predicate_name )  !=  OK )
	   {
		lsGetParm ( Engine, 1, cTERM, & FA ) ;
		lsTermToStr ( Engine, FA, predicate_name_arity, sizeof ( predicate_name_arity ) ) ;
	     }
	else	
		* predicate_name_arity = 0 ;

	// Cycle through all modules
	for ( i = 0 ; i < Help -> GetModuleCount ( ) ; i ++ )
	   {
		Module *	module = Help -> GetModule ( i ) ;

		// Cycle through each module predicates
		for  ( j = 0 ; j < Help -> GetPredicateCount ( i ) ; j ++ )
		    {
			char			buffer [ 256 ] ;
			Predicate *		pred = Help -> GetPredicate ( i, j ) ;


			// Make sure that it will succeed even if an arity is 
			// provided
			sprintf ( buffer, "%s/%d",
				pred -> Name, pred -> Arity ) ;

			// Found ?
			if  ( ! stricmp ( predicate_name, pred -> Name )  ||
			      ! stricmp ( predicate_name, buffer )        || 
			      ! stricmp ( predicate_name_arity, buffer ) )
			   {
				// Yes : print the name
				prolog_printf ( Engine, "Name          : %s/%d\n",
					pred -> Name, pred -> Arity ) ;

				// Print the module name
				prolog_printf ( Engine, "Module        : %s (%s)\n", 
					module -> Name, module -> Description ) ;

				// Print the usage string
				prolog_printf ( Engine, "Usage         : %s\n",
					pred -> ShortDescription ) ;

				// Print Succeeds when and Fails when
				if  ( pred -> SucceedsWhen  !=  NULL ) 
					prolog_printf ( Engine, "Succeeds when : %s\n",
						pred -> SucceedsWhen ) ;

				if  ( pred -> FailsWhen  !=  NULL ) 
					prolog_printf ( Engine, "Fails when    : %s\n",
						pred -> FailsWhen ) ;

				// Print long description
				if  ( pred -> LongDescription  !=  NULL )
					prolog_printf ( Engine, "Description   :\n\t%s\n",
						pred -> LongDescription ) ;

				// Argument header
				if  ( pred -> Arguments [0]. Name  !=  NULL )	
					prolog_printf ( Engine, "Arguments     :\n" ) ;

				// Print argument list
				for  ( ap = pred -> Arguments ; ap -> Name !=  NULL ; ap ++ )
				   {
					prolog_printf ( Engine, "\tArgument %d : %s (access = %s)\n",
						ap -> Position, ap -> Name, ap -> Access ) ;
					prolog_printf ( Engine, "\t\t%s\n",
						ap -> Description ) ;
				     }

				prolog_printf ( Engine, "\n" ) ;
			    }
			delete  pred ;
		     }

		delete  module ;
	     }

	return ( TRUE ) ;
    }

/******/





/****f* CVUtilities.CVUtilities/usage
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	usage/1
 *
 * SYNTAX
 *	usage(Predicate).
 *
 * PURPOSE
 *	Displays the a short help about the specified predicate.
 *	[Predicate] can be a functor/arity expression, or a single atom, or just
 *	the beginning of the searched predicate.
 *
 * ARGUMENTS
 *	[Predicate] (i) -
 *		Predicate to search.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
 
AMZIFUNC  cv_usage_1 ( ENGid   Engine )
   {
	register int	i, j, k ;
	char		predicate_name	     [ 256 ] ;
	char		predicate_name_arity [ 256 ] ;
	TERM		FA ;
	register int	found = 0 ;


	// Check if help file is loaded
	if ( ! check_help_loaded ( Engine ) )
		return ( FALSE ) ;

	// We can specify :
	//	help(term).
	//	help('term/arity')
	//	help(term/arity)
	// In the latter case, we receive a term instead of an atom, so we must
	// convert it to a string
	if  ( lsGetParm ( Engine, 1, cSTR, predicate_name )  !=  OK )
	   {
		lsGetParm ( Engine, 1, cTERM, & FA ) ;
		lsTermToStr ( Engine, FA, predicate_name_arity, sizeof ( predicate_name_arity ) ) ;
	     }
	else	
		* predicate_name_arity = 0 ;

	// Cycle through the modules
	for ( i = 0 ; i < Help -> GetModuleCount ( ) ; i ++ )
	   {
		Module *	module = Help -> GetModule ( i ) ;

		// Cycle through the predicates
		for  ( j = 0 ; j < Help -> GetPredicateCount ( i ) ; j ++ )
		    {
			char			buffer [ 256 ] ;
			Predicate *		pred = Help -> GetPredicate ( i, j ) ;



			// Make sure that it will succeed even if an arity is 
			// provided
			sprintf ( buffer, "%s/%d",
				pred -> Name, pred -> Arity ) ;

			if  ( ! strnicmp ( predicate_name, pred -> Name, strlen ( predicate_name ) )  ||
			      ! strnicmp ( predicate_name, buffer, strlen ( predicate_name ) )        || 
			      ! strnicmp ( predicate_name_arity, buffer, strlen ( predicate_name ) )  ||
			        strstr   ( pred -> Name, predicate_name )  !=  NULL )
			   {
				char		usage [ 256 ] ;

				// May need a linefeed between this predicate and the preceding one
				if  ( found )
					prolog_printf  ( Engine, "\n" ) ;
				found = 1 ;

				// Build the usage string
				sprintf ( usage, "Usage : %s( ", pred -> Name ) ;

				for ( k = 0 ; pred -> Arguments [k]. Name  !=  NULL ; k ++ )
				   {
					strcat ( usage, pred -> Arguments [k]. Name ) ;

					if  ( pred -> Arguments [k+1]. Name  !=  NULL ) 
						strcat ( usage, ", " ) ;
				     }

				strcat ( usage, " )." ) ;
 
				// print the usage string and short description
				prolog_printf ( Engine, "%s\n\t%s\n",
					usage, pred -> ShortDescription ) ;
			    }

			delete  pred ;
		     }

		delete  module ;
	     }

	return ( TRUE ) ;
    }

/******/





/****f* CVUtilities.CVUtilities/where
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	where/1
 *
 * SYNTAX
 *	where(Predicate).
 *
 * PURPOSE
 *	Displays the location of the specified predicate.
 *	[Predicate] can be a functor/arity expression, or a single atom, or just
 *	the beginning of the searched predicate.
 *
 * ARGUMENTS
 *	[Predicate] (i) -
 *		Predicate to search?
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

AMZIFUNC  cv_where_1 ( ENGid   Engine )
   {
	register int	i, j ;
	char		predicate_name	     [ 256 ] ;
	char		predicate_name_arity [ 256 ] ;
	TERM		FA ;
	register int	found		=  0 ;


	// Check if help file is loaded
	if ( ! check_help_loaded ( Engine ) )
		return ( FALSE ) ;

	// We can specify :
	//	help(term).
	//	help('term/arity')
	//	help(term/arity)
	// In the latter case, we receive a term instead of an atom, so we must
	// convert it to a string
	if  ( lsGetParm ( Engine, 1, cSTR, predicate_name )  !=  OK )
	   {
		lsGetParm ( Engine, 1, cTERM, & FA ) ;
		lsTermToStr ( Engine, FA, predicate_name_arity, sizeof ( predicate_name_arity ) ) ;
	     }
	else	
		* predicate_name_arity = 0 ;

	// Cycle through the modules
	for ( i = 0 ; i < Help -> GetModuleCount ( ) ; i ++ )
	   {
		Module *	module = Help -> GetModule ( i ) ;

		// Cycle through the predicates
		for  ( j = 0 ; j < Help -> GetPredicateCount ( i ) ; j ++ )
		    {
			char			buffer [ 256 ] ;
			Predicate *		pred = Help -> GetPredicate ( i, j ) ;



			// Make sure that it will succeed even if an arity is 
			// provided
			sprintf ( buffer, "%s/%d",
				pred -> Name, pred -> Arity ) ;

			if  ( ! strnicmp ( predicate_name, pred -> Name, strlen ( predicate_name ) )  ||
			      ! strnicmp ( predicate_name, buffer, strlen ( predicate_name ) )        || 
			      ! strnicmp ( predicate_name_arity, buffer, strlen ( predicate_name ) )  ||
			        strstr   ( pred -> Name, predicate_name )  !=  NULL )
			   {
				char		buffer [ 256 ] ;

				// Print the header before the first found predicate
				if  ( ! found ) 
					prolog_printf ( Engine, "predicate '%s' can be found in the following module(s) : \n",
						predicate_name ) ;

				found = 1 ;

				// Format functor/arity to a certain width
				sprintf ( buffer, "%s/%d", pred -> Name, pred -> Arity ) ;

				//
				prolog_printf ( Engine, "\t%-20s  %-20s  %s\n",
					buffer,
					module -> Name, module -> Description ) ;
			    }

			delete  pred ;
		     }

		delete  module ;
	     }

	return ( TRUE ) ;
    }

/******/

