// Test.cpp : définit le point d'entrée pour l'application console.
//

# include	<stdio.h>

# include	"../CVUtilities/XMLHelp.h"


int main ( int  argc,  char *  argv [] )
   {
	PrologHelp		Help ( "D:\\Development\\Prolog\\My AMZI Extensions\\CVUtilities\\Resources\\HelpFile.xml" ) ; 
	Predicate *		predicate ;
	register int		i, j ;
	int			count ;
	char			buffer [ 4096 ] ;
	char			arg [1024] ;


	count = Help.GetPredicateCount ( 0 ) ;
	
	for ( i = 0 ; i < count ; i ++ )
	    {
		predicate = Help.GetPredicate( 0, i ) ;
		sprintf ( buffer, "name=%s, arity = %d, description=%s\nSucceeds when %s\nFails when %s\nLong description = %s", 
			predicate->Name, predicate->Arity, predicate->ShortDescription,
			predicate->SucceedsWhen, predicate->FailsWhen,
			predicate->LongDescription ) ;

		for  ( j = 0 ; predicate -> Arguments [j]. Name  !=  NULL ; j ++ )
		   {
			sprintf ( arg, "\nArgument #%d : name = %s, description = %s",
				predicate -> Arguments [j]. Position,
				predicate -> Arguments [j]. Name,
				predicate -> Arguments [j]. Description ) ;
			strcat ( buffer, arg ) ;
		     }

		MessageBox ( 0, buffer, "title", MB_OK ) ;
		delete predicate ;
	      }
 
	return ( 0 ) ;
    }

