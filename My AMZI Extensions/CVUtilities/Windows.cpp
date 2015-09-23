/****h* CVUtilities/Windows
 ===============================================================================
 *
 * NAME
 *	Windows - Extended predicate for Microsoft Windows
 *
 * FILE
 *	Windows.cpp
 *
 * CONTENTS
 *	This file contains a set of AMZI extended predicates for operating under
 *	the Microsoft Windows operating system.
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
# include	"Windows.h"






/****f* CVUtilities.Windows/sysmetric
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	sysmetric/2
 *
 * SYNTAX
 *	sysmetric(Name, Value).
 *
 * PURPOSE
 *	Unifies [Value] with the value of the system metric [Name].
 *
 * ARGUMENTS
 *	[Name] (i) -
 *		Name of the system metric.
 *
 *	[Value] (o) - 
 *		Unified with the contents of the specified system metric.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

/*****************************************************************************

	Possible values for GetSystemMetrics.

 *****************************************************************************/
BEGIN_ASSOCIATION(SystemMetric) 
	
	ASSOCIATION( "SM_CXSCREEN"		, SM_CXSCREEN ),
	ASSOCIATION( "SM_CYSCREEN"		, SM_CYSCREEN ),
	ASSOCIATION( "SM_CXVSCROLL"		, SM_CXVSCROLL ),
	ASSOCIATION( "SM_CYHSCROLL"		, SM_CYHSCROLL ),
	ASSOCIATION( "SM_CYCAPTION"		, SM_CYCAPTION ),
	ASSOCIATION( "SM_CXBORDER"		, SM_CXBORDER ),
	ASSOCIATION( "SM_CYBORDER"		, SM_CYBORDER ),
	ASSOCIATION( "SM_CXDLGFRAME"		, SM_CXDLGFRAME ),
	ASSOCIATION( "SM_CYDLGFRAME"		, SM_CYDLGFRAME ),
	ASSOCIATION( "SM_CYVTHUMB"		, SM_CYVTHUMB ),
	ASSOCIATION( "SM_CXHTHUMB"		, SM_CXHTHUMB ),
	ASSOCIATION( "SM_CXICON"		, SM_CXICON ),
	ASSOCIATION( "SM_CYICON"		, SM_CYICON ),
	ASSOCIATION( "SM_CXCURSOR"		, SM_CXCURSOR ),
	ASSOCIATION( "SM_CYCURSOR"		, SM_CYCURSOR ),
	ASSOCIATION( "SM_CYMENU"		, SM_CYMENU ),
	ASSOCIATION( "SM_CXFULLSCREEN"		, SM_CXFULLSCREEN ),
	ASSOCIATION( "SM_CYFULLSCREEN"		, SM_CYFULLSCREEN ),
	ASSOCIATION( "SM_CYKANJIWINDOW"		, SM_CYKANJIWINDOW ),
	ASSOCIATION( "SM_MOUSEPRESENT"		, SM_MOUSEPRESENT ),
	ASSOCIATION( "SM_CYVSCROLL"		, SM_CYVSCROLL ),
	ASSOCIATION( "SM_CXHSCROLL"		, SM_CXHSCROLL ),
	ASSOCIATION( "SM_DEBUG"			, SM_DEBUG ),
	ASSOCIATION( "SM_SWAPBUTTON"		, SM_SWAPBUTTON ),
	ASSOCIATION( "SM_CXMIN"			, SM_CXMIN ),
	ASSOCIATION( "SM_CYMIN"			, SM_CYMIN ),
	ASSOCIATION( "SM_CXSIZE"		, SM_CXSIZE ),
	ASSOCIATION( "SM_CYSIZE"		, SM_CYSIZE ),
	ASSOCIATION( "SM_CXFRAME"		, SM_CXFRAME ),
	ASSOCIATION( "SM_CYFRAME"		, SM_CYFRAME ),
	ASSOCIATION( "SM_CXMINTRACK"		, SM_CXMINTRACK ),
	ASSOCIATION( "SM_CYMINTRACK"		, SM_CYMINTRACK ),
	ASSOCIATION( "SM_CXDOUBLECLK"		, SM_CXDOUBLECLK ),
	ASSOCIATION( "SM_CYDOUBLECLK"		, SM_CYDOUBLECLK ),
	ASSOCIATION( "SM_CXICONSPACING"		, SM_CXICONSPACING ),
	ASSOCIATION( "SM_CYICONSPACING"		, SM_CYICONSPACING ),
	ASSOCIATION( "SM_MENUDROPALIGNMENT"	, SM_MENUDROPALIGNMENT ),
	ASSOCIATION( "SM_PENWINDOWS"		, SM_PENWINDOWS ),
	ASSOCIATION( "SM_DBCSENABLED"		, SM_DBCSENABLED ),
	ASSOCIATION( "SM_CMOUSEBUTTONS"		, SM_CMOUSEBUTTONS ),
	ASSOCIATION( "SM_CXFIXEDFRAME"		, SM_CXFIXEDFRAME ),
	ASSOCIATION( "SM_CYFIXEDFRAME"		, SM_CYFIXEDFRAME ),
	ASSOCIATION( "SM_CXSIZEFRAME"		, SM_CXSIZEFRAME ),
	ASSOCIATION( "SM_CYSIZEFRAME"		, SM_CYSIZEFRAME ),
	ASSOCIATION( "SM_SECURE"		, SM_SECURE ),
	ASSOCIATION( "SM_CXEDGE"		, SM_CXEDGE ),
	ASSOCIATION( "SM_CYEDGE"		, SM_CYEDGE ),
	ASSOCIATION( "SM_CXMINSPACING"		, SM_CXMINSPACING ),
	ASSOCIATION( "SM_CYMINSPACING"		, SM_CYMINSPACING ),
	ASSOCIATION( "SM_CXSMICON"		, SM_CXSMICON ),
	ASSOCIATION( "SM_CYSMICON"		, SM_CYSMICON ),
	ASSOCIATION( "SM_CYSMCAPTION"		, SM_CYSMCAPTION ),
	ASSOCIATION( "SM_CXSMSIZE"		, SM_CXSMSIZE ),
	ASSOCIATION( "SM_CYSMSIZE"		, SM_CYSMSIZE ),
	ASSOCIATION( "SM_CXMENUSIZE"		, SM_CXMENUSIZE ),
	ASSOCIATION( "SM_CYMENUSIZE"		, SM_CYMENUSIZE ),
	ASSOCIATION( "SM_ARRANGE"		, SM_ARRANGE ),
	ASSOCIATION( "SM_CXMINIMIZED"		, SM_CXMINIMIZED ),
	ASSOCIATION( "SM_CYMINIMIZED"		, SM_CYMINIMIZED ),
	ASSOCIATION( "SM_CXMAXTRACK"		, SM_CXMAXTRACK ),
	ASSOCIATION( "SM_CYMAXTRACK"		, SM_CYMAXTRACK ),
	ASSOCIATION( "SM_CXMAXIMIZED"		, SM_CXMAXIMIZED ),
	ASSOCIATION( "SM_CYMAXIMIZED"		, SM_CYMAXIMIZED ),
	ASSOCIATION( "SM_NETWORK"		, SM_NETWORK ),
	ASSOCIATION( "SM_CLEANBOOT"		, SM_CLEANBOOT ),
	ASSOCIATION( "SM_CXDRAG"		, SM_CXDRAG ),
	ASSOCIATION( "SM_CYDRAG"		, SM_CYDRAG ),
	ASSOCIATION( "SM_SHOWSOUNDS"		, SM_SHOWSOUNDS ),
	ASSOCIATION( "SM_CXMENUCHECK"		, SM_CXMENUCHECK ),
	ASSOCIATION( "SM_CYMENUCHECK"		, SM_CYMENUCHECK ),
	ASSOCIATION( "SM_SLOWMACHINE"		, SM_SLOWMACHINE ),
	ASSOCIATION( "SM_MIDEASTENABLED"	, SM_MIDEASTENABLED ),
	ASSOCIATION( "SM_MOUSEWHEELPRESENT"	, SM_MOUSEWHEELPRESENT ),
	ASSOCIATION( "SM_XVIRTUALSCREEN"	, SM_XVIRTUALSCREEN ),
	ASSOCIATION( "SM_YVIRTUALSCREEN"	, SM_YVIRTUALSCREEN ),
	ASSOCIATION( "SM_CXVIRTUALSCREEN"	, SM_CXVIRTUALSCREEN ),
	ASSOCIATION( "SM_CYVIRTUALSCREEN"	, SM_CYVIRTUALSCREEN ),
	ASSOCIATION( "SM_CMONITORS"		, SM_CMONITORS ),
	ASSOCIATION( "SM_SAMEDISPLAYFORMAT"	, SM_SAMEDISPLAYFORMAT ),
	ASSOCIATION( "SM_IMMENABLED"		, SM_IMMENABLED ),
	ASSOCIATION( "SM_CXFOCUSBORDER"		, SM_CXFOCUSBORDER ),
	ASSOCIATION( "SM_CYFOCUSBORDER"		, SM_CYFOCUSBORDER ),
	ASSOCIATION( "SM_TABLETPC"		, SM_TABLETPC ),
	ASSOCIATION( "SM_MEDIACENTER"		, SM_MEDIACENTER ),
	ASSOCIATION( "SM_CMETRICS"		, SM_CMETRICS ),
	ASSOCIATION( "SM_REMOTESESSION"		, SM_REMOTESESSION ),
	ASSOCIATION( "SM_SHUTTINGDOWN"		, SM_SHUTTINGDOWN ),
	ASSOCIATION( "SM_REMOTECONTROL"		, SM_REMOTECONTROL ),
END_ASSOCIATION ;



/*****************************************************************************

	Predicate : sysmetric(Name, Value).

 *****************************************************************************/
AMZIFUNC cv_getsystemmetric ( ENGid	Engine )
   {
	char		Buffer [ 1024 ] ;
	int		MetricID, Result ;


	// Get the metric name
	lsGetParm ( Engine, 1, cSTR, Buffer ) ;

	// Convert it to a value suitable for GetSystemMetrics
	MetricID = association_value ( SystemMetric, Buffer ) ;
	Result   = GetSystemMetrics ( MetricID ) ;

	// Unify the result
	lsUnifyParm ( Engine, 2, cINT, & Result ) ;
	return ( TRUE ) ;
     }

/******/




/****f* CVUtilities.Windows/message
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	message/4, message/3, message/2
 *
 * SYNTAX
 *	message(Caption, Message, MB_options, Result).
 *	message(Caption, Message).
 *	message(Message).
 *
 * PURPOSE
 *	Displays the specified [Message] in a message box. The window title will
 *	be [Caption] in the message/2 version of the predicate,	otherwise it will
 *	be "Message".
 *
 * ARGUMENTS
 *	[Message] (i) -
 *		Message to display
 *
 *	[Caption] (i) -
 *		Optional caption of the message window.
 *
 *	[MB_Options] (i) -
 *		List of message box options.
 *
 *	[Result] (o) -
 *		Return value from MessageBox.
 *
 * NOTES
 *	This predicate always succeeds.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */



/*****************************************************************************

	Possible return values from the MessageBox Windows function.

 *****************************************************************************/
BEGIN_ASSOCIATION(MsgBoxResult) 

	ASSOCIATION( "idok"			, IDOK		),
	ASSOCIATION( "idcancel"			, IDCANCEL	),
	ASSOCIATION( "idyes"			, IDYES		),
	ASSOCIATION( "idno"			, IDNO		),
	ASSOCIATION( "idretry"			, IDRETRY	),
	ASSOCIATION( "idtryagain"		, IDTRYAGAIN	),
	ASSOCIATION( "idabort"			, IDABORT	),
	ASSOCIATION( "idcontinue"		, IDCONTINUE	),

END_ASSOCIATION ;



/*****************************************************************************

	Possible button options for the MessageBox Windows function.

 *****************************************************************************/
BEGIN_ASSOCIATION(MsgBoxButton)

	ASSOCIATION( "mb_abortretryignore"	, MB_ABORTRETRYIGNORE	),
	ASSOCIATION( "mb_canceltrycontinue"	, MB_CANCELTRYCONTINUE	),
	ASSOCIATION( "mb_help"			, MB_HELP		),
	ASSOCIATION( "mb_ok"			, MB_OK			),
	ASSOCIATION( "mb_okcancel"		, MB_OKCANCEL		),
	ASSOCIATION( "mb_retrycancel"		, MB_RETRYCANCEL	),
	ASSOCIATION( "mb_yesno"			, MB_YESNO		),
	ASSOCIATION( "mb_yesnocancel"		, MB_YESNOCANCEL	),
	ASSOCIATION( "mb_iconexclamation"	, MB_ICONEXCLAMATION	),
	ASSOCIATION( "mb_iconwarning"		, MB_ICONWARNING	),
	ASSOCIATION( "mb_iconinformation"	, MB_ICONINFORMATION	),
	ASSOCIATION( "mb_iconasterisk"		, MB_ICONASTERISK	),
	ASSOCIATION( "mb_iconquestion"		, MB_ICONQUESTION	),
	ASSOCIATION( "mb_iconstop"		, MB_ICONSTOP		),
	ASSOCIATION( "mb_iconerror"		, MB_ICONERROR		),
	ASSOCIATION( "mb_iconhand"		, MB_ICONHAND		),
	ASSOCIATION( "mb_right"			, MB_RIGHT		),
	ASSOCIATION( "mb_rltreading"		, MB_RTLREADING		),
	ASSOCIATION( "mb_setforeground"		, MB_SETFOREGROUND	),
	ASSOCIATION( "mb_topmost"		, MB_TOPMOST		),

END_ASSOCIATION ;




/*****************************************************************************

	predicate : message(Caption, Message, MB_Options, Result).

 *****************************************************************************/
AMZIFUNC cv_message_4 ( ENGid	Engine )
   {
	char *			S1,
	     *			S2,
	     *			ResultString ;
	TERM			MB ;
	int			S1Length,
				S2Length ;
	UINT			Result,
				MBFlags ;
	

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

	// Get Caption, text and MB_ options
	lsGetParm ( Engine, 1, cSTR , S1 ) ;
	lsGetParm ( Engine, 2, cSTR , S2 ) ;

	// MB options can be specified as a list or as a single element
	// Handle this case here
	if  ( lsGetParmType ( Engine, 3 )  ==  pLIST )
	   {
		// Argument is a list : get the MB flags
		lsGetParm ( Engine, 3, cTERM, & MB ) ;
		MBFlags = association_bits ( MsgBoxButton, Engine, MB ) ;
	     }
	else	// Atgument is an atom
	   {
		char		Buffer [ 256 ] ;


		lsGetParm ( Engine, 3, cSTR, Buffer ) ;
		MBFlags = association_value ( MsgBoxButton, Buffer ) ;
	     }

	// Display the message box
	Result  = MessageBox ( NULL, S1, S2, MBFlags ) ;

	// Unify the result
	ResultString = association_name ( MsgBoxResult, Result ) ;
	lsUnifyParm ( Engine, 4, cATOM, ResultString ) ;

	// Cleanup
	free ( S1 ) ;
	free ( S2 ) ;
	return( TRUE ) ;
     }



/*****************************************************************************

	predicate : message(Caption, Message).

 *****************************************************************************/
AMZIFUNC cv_message_2 ( ENGid	Engine )
   {
	char *			S1,
	     *			S2 ;
	int			S1Length,
				S2Length ;


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

	// Display the message
	MessageBox ( NULL, S1, S2, MB_OK ) ;

	// Cleanup
	free ( S1 ) ;
	free ( S2 ) ;
	return( TRUE ) ;
     }



/*****************************************************************************

	predicate : message(Message).

 *****************************************************************************/
AMZIFUNC cv_message_1 ( ENGid	Engine )
   {
	char *			S ;
	int			SLength ;


	// Retrieve the lengths of the string arguments
	SLength = lsStrParmLen( Engine, 1 ) + 1 ;
	
	// Allocate message string
	if ( ( S = (char *) malloc( SLength ) )  ==  NULL ) 
		return ( FALSE ) ;

	// Get Message
	lsGetParm ( Engine, 1, cSTR, S ) ;

	// Display the message
	MessageBox ( NULL, "Message", S, MB_OK ) ;

	// Cleanup
	free ( S ) ;
	return( TRUE ) ;
     }

/******/


