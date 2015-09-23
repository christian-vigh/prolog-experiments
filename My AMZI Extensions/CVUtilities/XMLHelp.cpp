/****h* CVUtilities/XMLHelp
 ===============================================================================
 *
 * NAME
 *	XMLHelp - XML help handling class.
 *
 * FILE
 *	XMLHelp.cpp
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

# include	<string>

using namespace std ;


# include	"XMLHelp.h"




/****if* CVUtilities.XMLHelp/VariantString
 -------------------------------------------------------------------------------
 *
 * MEMBER
 *	PrologHelp :: VariantString
 *
 * SYNTAX
 *	VARIANT PrologHelp :: VariantString ( BSTR str ) ;
 *
 * PURPOSE
 *	Creates a variant whose contents are the BSTR passed as a parameter.
 *
 * ARGUMENTS
 *	BSTR  str -
 *		String to use for variant initialization.
 *
 * RETURN VALUE
 *	Returns the variant created with [str].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

VARIANT PrologHelp :: VariantString ( BSTR str )
   {
	VARIANT		var;

	VariantInit ( & var ) ;
	V_BSTR( & var ) = SysAllocString ( str ) ;
	V_VT ( & var ) = VT_BSTR ;
	return ( var ) ;
    }

/******/





/****if* CVUtilities.XMLHelp/DomFromCOM
 -------------------------------------------------------------------------------
 *
 * MEMBER
 *	PrologHelp :: DomFromCOM
 *
 * SYNTAX
 *	IXMLDOMDocument *	PrologHelp :: DomFromCOM ( )
 *
 * PURPOSE
 *	Creates an instance of an MS XML DOM document.
 *
 * RETURN VALUE
 *	A pointer to the IXMLDOMDocument object.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

IXMLDOMDocument *	PrologHelp :: DomFromCOM ( )
   {
	IXMLDOMDocument *	pxmldoc = NULL ;


	CoCreateInstance( __uuidof(DOMDocument40),
                          NULL,
                          CLSCTX_INPROC_SERVER,
                          __uuidof(IXMLDOMDocument),
                          (void ** ) & pxmldoc ) ;

	pxmldoc -> put_async ( VARIANT_FALSE ) ;
	pxmldoc -> put_validateOnParse ( VARIANT_FALSE ) ;
	pxmldoc -> put_resolveExternals ( VARIANT_FALSE ) ;

	return ( pxmldoc ) ;
    }

/******/





/****if* CVUtilities.XMLHelp/LoadXMLHelp
 -------------------------------------------------------------------------------
 *
 * MEMBER
 *	PrologHelp :: LoadXMLHelp
 *
 * SYNTAX
 *	int	PrologHelp :: LoadXMLHelp ( char *  DocumentPath )
 *
 * PURPOSE
 *	Loads the Prolog XML help file specified by [DocumentPath].
 *
 * ARGUMENTS
 *	char *	DocumentPath -
 *		Path to the document to load.
 *
 * RETURN VALUE
 *	LoadXMLHelp returns TRUE if the loading is successful, or FALSE if an 
 *	error occurred.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

int	PrologHelp :: LoadXMLHelp ( char *  DocumentPath )
   {
	wchar_t		Path  [ MAX_PATH + 1 ] ;	
	BSTR		bstr = NULL ;
	VARIANT_BOOL	status ;
	VARIANT		var ;


	// Create the XML object instance
	XMLHelpDocument = DomFromCOM();

	if  ( ! XMLHelpDocument ) 
		return ( FALSE ) ;

	// Load it ; convert path name to wide char then to VARIANT
	VariantInit ( & var ) ;
		mbstowcs( Path, DocumentPath, MAX_PATH + 1 );
		var = VariantString( Path );
	XMLHelpDocument -> load ( var, & status ) ;

	if ( status != VARIANT_TRUE )
		return ( FALSE ) ;

	// Remember where the top nodes are
	XMLHelpDocument -> selectNodes ( L"prolog/modules/module", & XMLHelpNodes ) ;

	// All done...
	VariantClear ( & var ) ;
	return ( TRUE ) ;
    }

/******/





/****if* CVUtilities.XMLHelp/GetAttribute
 -------------------------------------------------------------------------------
 *
 * MEMBER
 *	PrologHelp :: GetAttribute
 *
 * SYNTAX
 *	char *	PrologHelp :: GetAttribute ( IXMLDOMNode *  Node, char *  AttrName )
 *
 * PURPOSE
 *	Retrieves the attribute [AttrName] from the node [Node].
 *
 * ARGUMENTS
 *	IXMLDOMNode *	Node -
 *		Node to examine.
 *
 *	char *  AttrName -
 *		Name of the attribute whose value is to be retrieved.
 *
 * RETURN VALUE
 *	A pointer to a static buffer containing the attribute value.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

char *	PrologHelp :: GetAttribute ( IXMLDOMNode *  Node, char *  AttrName )
   {
	static char		buffer [ MAX_PATH + 1 ] ;
	wchar_t			WAttrName [ MAX_PATH + 1 ] ;
	IXMLDOMNode *		AttrNode ;
	IXMLDOMNamedNodeMap *	Attributes ;
	VARIANT			var ;	


	// Get attribute list
	Node -> get_attributes ( & Attributes ) ;

	// Convert AttrName to wide char
	mbstowcs ( WAttrName, AttrName, MAX_PATH ) ;

	// Retrieve the node value of AttrName
	Attributes -> getNamedItem ( WAttrName, & AttrNode ) ;
	AttrNode -> get_nodeValue ( & var ) ;

	// and convert it to normal char
	wcstombs( buffer, var. bstrVal, sizeof ( buffer ) ) ;
	return ( buffer ) ;
     }

/******/






/****f* CVUtilities.XMLHelp/GetModuleCount
 -------------------------------------------------------------------------------
 *
 * MEMBER
 *	PrologHelp :: GetModuleCount
 *
 * SYNTAX
 *	int  PrologHelp :: GetModuleCount ( ) ;
 *
 * PURPOSE
 *	Returns the number of modules documented in the Prolog XML help file.
 *
 * RETURN VALUE
 *	Number of modules documented in the XML help file.
 *	GetModuleCount() returns 0 if no modules are documented or if an error
 *	occurred.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

int	PrologHelp :: GetModuleCount ( )
   {
	long int	length ;


	XMLHelpNodes -> get_length( & length ) ;
	return ( ( int ) length ) ;
     }

/******/





/****f* CVUtilities.XMLHelp/GetModule
 -------------------------------------------------------------------------------
 *
 * MEMBER
 *	PrologHelp :: GetModule
 *
 * SYNTAX
 *	Module *	PrologHelp :: GetModule ( int		index ) ;
 *
 * PURPOSE
 *	Retrieves module information about the module whose index is given by
 *	[index]. The module information is defined by the Module structure and
 *	contains the following fields :
 *
 *	- Name :
 *		Name of the module.
 *	- Description :
 *		Short description of the module contents.
 *
 * ARGUMENTS
 *	int  index -
 *		Index of the module to search (the first module has the index 0).
 *
 * RETURN VALUE
 *	A pointer to a dynamically allocated Module structure. It is the 
 *	responsability of the caller to free it using the delete operator.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

Module *	PrologHelp :: GetModule ( int		index )
   {
	IXMLDOMNode *		Node ;
	register char *		p ;
	Module *		module  = new Module ;


	// Get the node corresponding to the specified module index
	XMLHelpNodes -> get_item ( index, & Node ) ;

	if  ( Node  ==  NULL )
		return ( module ) ;

	// Get the attributes
	p = GetAttribute ( Node, "name" ) ;
	module -> Name= strdup ( p ) ;

	p = GetAttribute ( Node, "description" ) ;
	module -> Description = strdup ( p ) ;

	// All done
	return ( module ) ;
    }

/******/





/****f* CVUtilities.XMLHelp/GetPredicateCount
 -------------------------------------------------------------------------------
 *
 * MEMBER
 *	PrologHelp :: GetPredicateCount
 *
 * SYNTAX
 *	int  PrologHelp :: GetPredicateCount ( int  module_index ) ;
 *
 * PURPOSE
 *	Returns the number of predicates documented in the Prolog XML help file,
 *	for the specified index.
 *
 * RETURN VALUE
 *	Number of predicates documented in the XML help file.
 *	GetPredicateCount() returns 0 if no predicates are documented or if an error
 *	occurred.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

int	PrologHelp :: GetPredicateCount ( int  module_index ) 
    {
	IXMLDOMNodeList	*	Nodes ;
	IXMLDOMNode *		Node ;
	long int		length ;


	// Get the node corresponding to the specified module_index
	XMLHelpNodes -> get_item ( module_index, & Node ) ;

	if  ( Node  ==  NULL )
		return ( 0 ) ;

	// Get a pointer to the child predicate nodes
	Node  -> get_childNodes ( & Nodes ) ;

	if  ( Node  ==  NULL )
		return ( 0 ) ;

	// Get predicate count
	Nodes -> get_length ( & length ) ;

	// All done
	return ( ( int ) length ) ;
   }

/******/






/****f* CVUtilities.XMLHelp/GetPredicate
 -------------------------------------------------------------------------------
 *
 * MEMBER
 *	PrologHelp :: GetPredicate
 *
 * SYNTAX
 *	Predicate *	PrologHelp :: GetPredicate ( int	module_index,
 *						     int	predicate_index )
 *
 * PURPOSE
 *	Retrieves predicate information about the module whose index is given by
 *	[module_index] and the predicate given by [predicate_index]. 
 *	The predicate information is defined by the Predicate structure and
 *	contains the following fields :
 *
 *	- Name :
 *		Name of the predicate.
 *	- ShortDescription :
 *		Short description of the predicate.
 *	- LongDescription :
 *		Long description of the predicate.
 *	- Arity :
 *		Arity of the predicate.
 *	- SucceedsWhen :
 *		Describes the conditions that make the predicate to succeed.
 *	- FailsWhen :
 *		Describes the conditions that make the predicate to fail.
 *	- Arguments :
 *		Array of Argument structures containing the following fields :
 *		- Name :
 *			Name of the argument
 *		- Description :
 *			Description of the argument
 *		- Position :
 *			Position of the argument
 *		The last element of the array is filled with null values.
 *
 * ARGUMENTS
 *	int  module_index -
 *		Index of the module to search (the first module has the index 0).
 *	int  predicate_index -
 *		Index of the predicate within the module (the first predicate has
 *		the index 0).
 *
 * RETURN VALUE
 *	A pointer to a dynamically allocated Predicate structure. It is the 
 *	responsability of the caller to free it using the delete operator.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

Predicate *	PrologHelp :: GetPredicate ( int		module_index,
					     int		predicate_index )
   {
	Predicate *		predicate	=  new Predicate ;
	IXMLDOMNodeList *	Nodes,
		        *	PredicateChildren ;
	IXMLDOMNode *		Node,
		    *		PredicateNode,
		    *		PredicateChild ;
	long int		PredicateChildCount ;
	register long int	i, j ; 
	register char *		p ;
	BSTR			str ;
	char			buffer [ 4096 ] ;


	// Get the node corresponding to the specified module index
	XMLHelpNodes -> get_item ( module_index, & Node ) ;

	if  ( Node  ==  NULL )
		return ( predicate ) ;

	// Get the child nodes (predicate list)
	Node -> get_childNodes ( & Nodes ) ;

	if  ( Nodes  ==  NULL )
		return ( predicate ) ;

	// Get the specified predicate within the child nodes
	Nodes -> get_item ( predicate_index, & PredicateNode ) ;

	if  ( PredicateNode  ==  NULL )
		return ( predicate ) ;

	// Retrieve the "name" attribute
	p = GetAttribute ( PredicateNode, "name" ) ;
	predicate -> Name = strdup ( p ) ;

	// Retrieve the "arity" attribute
	p = GetAttribute ( PredicateNode, "arity" ) ;
	predicate -> Arity = atoi ( p ) ;

	// Retrieve the "short-description" attribute
	p = GetAttribute ( PredicateNode, "short-description" ) ;
	predicate -> ShortDescription = strdup ( p ) ;

	// Get the child node list
	PredicateNode -> get_childNodes ( & PredicateChildren ) ;

	if  ( PredicateChildren  ==  NULL )
		return ( predicate ) ;

	// Cycle through the children
	PredicateChildren -> get_length( & PredicateChildCount ) ;

	for  ( i = 0 ; i < PredicateChildCount ; i ++ )
	   {
		// Get the next children
		PredicateChildren -> get_item ( i, & PredicateChild ) ;

		if  ( PredicateChild  ==  NULL ) 
			return ( predicate ) ;

		// Get the child name and convert it to single-char
		PredicateChild -> get_nodeName ( & str ) ;
		wcstombs ( buffer, str, sizeof ( buffer ) ) ;

		// Is it a <succeeds> XML tag ?
		if  ( ! stricmp ( buffer, "succeeds" ) ) 
		    {
			p = GetAttribute ( PredicateChild, "when" ) ;
			predicate -> SucceedsWhen = strdup ( p ) ;
		      }
		// Is it a <fails> XML tag ?
		else if  ( ! stricmp ( buffer, "fails" ) ) 
		    {
			p = GetAttribute ( PredicateChild, "when" ) ;
			predicate -> FailsWhen = strdup ( p ) ;
		      }
		// Is it a <description> tag ?
		else if  ( ! stricmp ( buffer, "description" ) ) 
		    {
			PredicateChild -> get_text ( & str ) ;
			wcstombs ( buffer, str, sizeof ( buffer ) ) ;
			predicate -> LongDescription = strdup ( buffer ) ;
			
		      }
		// Is it an <arguments> tag ?
		else if ( ! stricmp ( buffer, "arguments" ) )
		   {
			IXMLDOMNodeList *	Args ;
			IXMLDOMNode *		Arg ;
			long int		longargc ;
			int			argc ;


			// Get child list (<argument> tags)
			PredicateChild -> get_childNodes ( & Args ) ;

			if  ( Args  ==  NULL )
			   {
				predicate -> Arguments = new Argument [1] ;
				return ( predicate ) ;
			     }

			// Get child count and allocate the Arguments array accordingly
			Args -> get_length ( & longargc ) ;
			argc = ( int ) longargc ;
			predicate -> Arguments = new Argument [argc + 1] ;
			
			// Cycle through each child 
			for  ( j = 0 ; j < argc ; j ++ )
			   {
				Args -> get_item ( j, & Arg ) ;

				if  ( Arg ==  NULL )
					return ( predicate ) ;

				p = GetAttribute ( Arg, "name" ) ;
				predicate -> Arguments [j]. Name = strdup ( p ) ;

				p = GetAttribute ( Arg, "description" ) ;
				predicate -> Arguments [j]. Description = strdup ( p ) ;

				p = GetAttribute ( Arg, "position" ) ;
				predicate -> Arguments [j]. Position = atoi ( p ) ;

				p = GetAttribute ( Arg, "access" ) ;
				predicate -> Arguments [j]. Access = strdup ( p ) ;
			    }
		    }
	    }

	// All done, ensure that the Arguments field is always non null...
	if  ( predicate -> Arguments  ==  NULL )
		predicate -> Arguments = new Argument [1] ;

	return ( predicate ) ;
    }

/******/