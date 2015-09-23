/*****************************************************************************

	CVUtilities.h - Christian Vigh, 2007/02/10.

	Extension library (.lsx) for Amzi Prolog.

 *****************************************************************************/

# if ! defined(__CVUTILITIES_H__)
#	define	__CVUTILITIES_H__


# include	<ctype.h>
# include	<time.h>

# include	<tchar.h>
# include	<direct.h>
# include	<io.h>
# include	<afxwin.h>		// MFC core and standard components
# include	<afxext.h>		// MFC extensions (including VB)

# include	"amzi.h"


// Other macros
# define	FileLength(fp)		_filelength( _fileno ( fp ) )



/*****************************************************************************

	Constants.

 *****************************************************************************/

# define	LSXNAME			TEXTSTRING("CVUtilities")
# define	HELPFILE		"CVUtilities.xml"



/*****************************************************************************

	Defines.

 *****************************************************************************/
// I don't like "_T"
# define	TEXTSTRING(x)		_T(x)
# define	characters		_TCHAR

// I don't like "TP EXPFUNC" etc.
# define	AMZIFUNC		TF EXPFUNC
# define	DLLENTRYPOINT		extern "C" __declspec(dllexport) RC EXPFUNC

// Define an extended predicate entry
# define	BEGIN_PREDICATE_TABLE(Name)			\
			PRED_INIT	Name [ ] = {
# define	PREDICATE(Name,Parameters, Function)	\
			{ TEXTSTRING(Name), Parameters, Function }			
# define	END_PREDICATE_TABLE			\
			PREDICATE(NULL, 0, NULL) }



/*****************************************************************************

	Structures.

 *****************************************************************************/

// Correspondence between a bit constant name and a value

typedef struct
   {
		char *		ConstantName ;
		UINT		ConstantValue ;
     }  Association ;


# define	BEGIN_ASSOCIATION(Name)			\
			Association   Name [] = {
# define	ASSOCIATION(Name,Value)			\
			{ Name, Value }
# define	END_ASSOCIATION				\
			{ NULL, 0 } }

# define	ASSOCIATION_SIZE(Name)		( ( sizeof ( Name ) / sizeof ( Association ) ) - 1 )



/*****************************************************************************

	Local includes.

 *****************************************************************************/

# include	"Files.h"
# include	"Math.h"
# include	"Path.h"
# include	"Strings.h"
# include	"System.h"
# include	"Windows.h"

# include	"XMLHelp.h"



/*****************************************************************************

	General extended predicates.

 *****************************************************************************/

AMZIFUNC	cv_help_1			( ENGid		id ) ;
AMZIFUNC	cv_modlist_0			( ENGid		id ) ;
AMZIFUNC	cv_module_0			( ENGid		id ) ;
AMZIFUNC	cv_module_1			( ENGid		id ) ;
AMZIFUNC	cv_usage_1			( ENGid		id ) ;
AMZIFUNC	cv_where_1			( ENGid		id ) ;



/*****************************************************************************

	Prototypes.

 *****************************************************************************/

void	DebugMessage		( char *	Title,
				  char *	Message,
				  ... ) ;

UINT	association_value	( Association *	Table,
				  char *	Name ) ;
char *	association_name	( Association *	Table,
				  UINT		Value ) ;
UINT	association_bits	( Association *	Table,
				  ENGid		engine,
				  TERM		list ) ;


 # endif	/* __CVUTILITIES_H__ */