/******
 ===============================================================================
 *
 * NAME
 *	XMLHelp - XML help handling routines.
 *
 * FILE
 *	XMLHelp.h
 *
 * CONTENTS
 *
 * AUTHOR
 *	Christian Vigh, February 2007.
 *
 * HISTORY
 *	[Version]  [Date]	[Contents]
 *	  1.00	   13/02/2007	Initial version.	   
 *
 ===============================================================================
 ******/

# if	! defined( __XMLHELP_H__ )
#	define     __XMLHELP_H__


# include	<windows.h>
# include	"MsXML2.h"


/*****************************************************************************

	Definition of a module.

 *****************************************************************************/

class Module
   {
	public :
		char *		Name ;
		char *		Description ;

	public :
		Module ( ) 
		   {
			Name		=  NULL ;
			Description	=  NULL ;
		     }

		~Module ( )
		   {
			if  ( Name  !=  NULL )
				delete Name ;
			if ( Description  !=  NULL )
				delete Description ;
		     }

    }  ;




/*****************************************************************************

	Definition of a predicate argument.

 *****************************************************************************/

class Argument 
   {
	public :
		char *		Name ;
		char *		Description ;
		int		Position ;
		char *		Access ;
	
	public :
		Argument ( )
		   {
			Name		=  NULL ;
			Description	=  NULL ;
			Position	=  -1 ;
			Access		=  NULL ;
		    }

		~Argument ( )
		   {
			if  ( Name  !=  NULL )
				delete  Name ;
			if  ( Description  !=  NULL )
				delete  Description ;
			if  ( Access  !=  NULL )
				delete  Access ;
		    }
    } ;




/*****************************************************************************

	Definition of a predicate.

 *****************************************************************************/

class Predicate 
   {
	public :
		char *		Name ;
		int		Arity ;
		char *		ShortDescription ;
		char *		LongDescription ;
		Argument *	Arguments ;
		char *		SucceedsWhen ;
		char *		FailsWhen ;

	public :
		Predicate ( ) 
		   {
			Name			= NULL ;
			ShortDescription	= NULL ;
			SucceedsWhen		= NULL ;
			FailsWhen		= NULL ;
			Arity			= -1 ;
			LongDescription		= NULL ;
			Arguments		= NULL ;
		     }

		~Predicate ( )
		    {
			if  ( Name  !=  NULL )
				delete Name ;
			if  ( ShortDescription  !=  NULL )
				delete ShortDescription ;
			if  ( LongDescription  !=  NULL )
				delete LongDescription ;
			if  ( SucceedsWhen  !=  NULL )
				delete SucceedsWhen ;
			if  ( FailsWhen  !=  NULL )
				delete FailsWhen ;
			if  ( Arguments  !=  NULL )
				delete []  Arguments ;
		     }
    } ;




/*****************************************************************************

	Definition of the PrologHelp class.

 *****************************************************************************/

class  PrologHelp
   {
	public :
		PrologHelp ( char *  Path )
		   {
			CoInitialize ( NULL ) ;
			m_succeeded = LoadXMLHelp ( Path ) ;
		     }

		~PrologHelp ( )
		   {
			XMLHelpDocument -> Release ( ) ;
			CoUninitialize ( ) ;
		     }

	public :
		int		GetModuleCount		( ) ;
		Module *	GetModule		( int		index ) ;

		int		GetPredicateCount	( int		module_index ) ;
		Predicate *	GetPredicate		( int		module_index,
							  int		pred_index ) ;

		int		Succeeded ( )
		   {
			return ( m_succeeded ) ;
		    }


	private :
		IXMLDOMDocument *	XMLHelpDocument ;
		IXMLDOMNodeList *	XMLHelpNodes	;
		int			m_succeeded ;

	private :
		IXMLDOMDocument *	DomFromCOM	( ) ;
		char *			GetAttribute	( IXMLDOMNode *		Node, 
							  char *		AttrName ) ;
		int			LoadXMLHelp	( char *		Document ) ;
		VARIANT			VariantString	( BSTR			str ) ;

    } ;


# endif		/*  __XMLHELP_H__  */