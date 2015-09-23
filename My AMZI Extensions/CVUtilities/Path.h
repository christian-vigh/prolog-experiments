/*****************************************************************************

	Path.h - Christian Vigh, 2007/02/10.

	Extension library (.lsx) for Amzi Prolog : predicates operating on
	paths.

 *****************************************************************************/

# if  ! defined(__PATH_H__)
#	define	__PATH_H__


AMZIFUNC	cv_chdir		( ENGid		EngineId ) ;
AMZIFUNC	cv_copyfile_3		( ENGid		EngineId ) ;
AMZIFUNC	cv_copyfile_2		( ENGid		EngineId ) ;
AMZIFUNC	cv_delext		( ENGid		EngineId ) ;
AMZIFUNC	cv_fileexists		( ENGid		EngineId ) ;
AMZIFUNC	cv_fileflag		( ENGid		EngineId ) ;
AMZIFUNC	cv_filematch		( ENGid		EngineId ) ;
AMZIFUNC	cv_filesize		( ENGid		EngineId ) ;
AMZIFUNC	cv_filetime_4		( ENGid		EngineId ) ;
AMZIFUNC	cv_filetime_3		( ENGid		EngineId ) ;
AMZIFUNC	cv_findfiles		( ENGid		EngineId ) ;
AMZIFUNC	cv_renext		( ENGid		EngineId ) ;
AMZIFUNC	cv_getcwd		( ENGid		EngineId ) ;
AMZIFUNC	cv_getfullpath		( ENGid		EngineId ) ;
AMZIFUNC	cv_getlfn		( ENGid		EngineId ) ;
AMZIFUNC	cv_getsfn		( ENGid		EngineId ) ;
AMZIFUNC	cv_gettemppath		( ENGid		EngineId ) ;
AMZIFUNC	cv_gettempfile_4	( ENGid		EngineId ) ;
AMZIFUNC	cv_gettempfile_3	( ENGid		EngineId ) ;
AMZIFUNC	cv_gettempfile_2	( ENGid		EngineId ) ;
AMZIFUNC	cv_isdir		( ENGid		EngineId ) ;
AMZIFUNC	cv_isempty		( ENGid		EngineId ) ;
AMZIFUNC	cv_isexe		( ENGid		EngineId ) ;
AMZIFUNC	cv_isfilespec		( ENGid		EngineId ) ;
AMZIFUNC	cv_ishtmlfile		( ENGid		EngineId ) ;
AMZIFUNC	cv_islfnfilespec	( ENGid		EngineId ) ;
AMZIFUNC	cv_isnetworkpath	( ENGid		EngineId ) ;
AMZIFUNC	cv_isrelative		( ENGid		EngineId ) ;
AMZIFUNC	cv_isroot		( ENGid		EngineId ) ;
AMZIFUNC	cv_issameroot		( ENGid		EngineId ) ;
AMZIFUNC	cv_isunc		( ENGid		EngineId ) ;
AMZIFUNC	cv_isuncserver		( ENGid		EngineId ) ;
AMZIFUNC	cv_isuncservershare	( ENGid		EngineId ) ;
AMZIFUNC	cv_isurl		( ENGid		EngineId ) ;
AMZIFUNC	cv_mkdir		( ENGid		EngineId ) ;
AMZIFUNC	cv_movefile_3		( ENGid		EngineId ) ;
AMZIFUNC	cv_movefile_2		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathdir		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathdrive		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathext		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathfilename		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathmakepretty	( ENGid		EngineId ) ;
AMZIFUNC	cv_pathname		( ENGid		EngineId ) ;
AMZIFUNC	cv_rename		( ENGid		EngineId ) ;
AMZIFUNC	cv_renext		( ENGid		EngineId ) ;
AMZIFUNC	cv_splitpath_5		( ENGid		EngineId ) ;
AMZIFUNC	cv_splitpath_6		( ENGid		EngineId ) ;
AMZIFUNC	cv_pathunexpandenv	( ENGid		EngineId ) ;
AMZIFUNC	cv_removefile		( ENGid		EngineId ) ;
AMZIFUNC	cv_rmdir		( ENGid		EngineId ) ;
AMZIFUNC	cv_splitpath		( ENGid		EngineId ) ;


# endif		/*  __PATH_H__  */