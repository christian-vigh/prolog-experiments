/*============================================================================

	File	 : Helpers.pro
	Author	 : Christian Vigh, 2005/08.
	Contents :
		Pr�dicats humanitaires.

 ============================================================================*/


:- import(list).
 

/****************************************************************************

	parse_command_line - 
		Construit une liste compos�e des diff�rents �l�ments des
		param�tres d'une ligne de commande en enlevant les s�parateurs.
 		Il y a des cuts partout pour �viter � Prolog de backtracker
 		dans pare_command_line et dans append, ce qui rend les 
 		performances d�gueulasses.
 		
 ****************************************************************************/
parse_command_line([], []) :- !.

parse_command_line([H|T], Result) :-
	( H == ',' ; H == ';' ; H == '. ' ; H == '''' ),
	parse_command_line(T, Result), !.

parse_command_line([H|T], Result) :-
	parse_command_line(T, TempResult),
	append([H], TempResult, Result), !.
	