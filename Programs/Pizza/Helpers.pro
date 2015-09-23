/*============================================================================

	File	 : Helpers.pro
	Author	 : Christian Vigh, 2005/08.
	Contents :
		Prédicats humanitaires.

 ============================================================================*/


:- import(list).
 

/****************************************************************************

	parse_command_line - 
		Construit une liste composée des différents éléments des
		paramètres d'une ligne de commande en enlevant les séparateurs.
 		Il y a des cuts partout pour éviter à Prolog de backtracker
 		dans pare_command_line et dans append, ce qui rend les 
 		performances dégueulasses.
 		
 ****************************************************************************/
parse_command_line([], []) :- !.

parse_command_line([H|T], Result) :-
	( H == ',' ; H == ';' ; H == '. ' ; H == '''' ),
	parse_command_line(T, Result), !.

parse_command_line([H|T], Result) :-
	parse_command_line(T, TempResult),
	append([H], TempResult, Result), !.
	