/*****************************************************************************

	System.h - Christian Vigh, 2007/02/10.

	Extension library (.lsx) for Amzi Prolog : System predicates.

 *****************************************************************************/

# if  ! defined(__SYSTEM_H__)
#	define	__SYSTEM_H__


// Predicates
AMZIFUNC	cv_getcomputername	( ENGid		EngineId ) ;
AMZIFUNC	cv_getenv		( ENGid		EngineId ) ;
AMZIFUNC	cv_getenvlist		( ENGid		EngineId ) ;
AMZIFUNC	cv_getusername		( ENGid		EngineId ) ;
AMZIFUNC	cv_setenv		( ENGid		EngineId ) ;


# endif		/* __ SYSTEM_H__ */