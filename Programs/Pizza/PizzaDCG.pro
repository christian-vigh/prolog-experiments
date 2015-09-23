/*============================================================================

	File	 : pizzaDCG.pro
	Author	 : Christian Vigh, 2005/07.
	Contents :
		Choix d'une pizza (en utilisant une DCG).

 ============================================================================*/


verbose(1). 



/****************************************************************************

	verbose_write -
		Affiche la liste pass�e en argument si verbose(1) est vrai.
 
 ****************************************************************************/
verbose_write(List) :-
	verbose(1),
	do_write(List),
	nl.
verbose_write(_).


do_write([H|T]) :-
	output(H),
	do_write(T).
do_write([]).


output(nl) :-
	nl.
output(tab) :-
	write('	').
output(X) :-
	write(X).


/****************************************************************************

	Initialize.
 
 ****************************************************************************/
initialize :-
	consult('Ingredients.zdb'),		% Liste des ingr�dients
	%consult('Pizzas.zdb'),			% D�finition des pizzas
	define_default_supplier([]),		% Nom de fournisseur par d�faut
	define_default_identity('').		% Identit� par d�faut de l'utilisateur


/****************************************************************************

	Programme principal. 
 
 ****************************************************************************/

main :-
	write('Choisis ta pizza ce soir !'), nl,
	write('--------------------------'), nl, nl,
	initialize,
	input_loop.


/****************************************************************************

	Boucle de commande. 
 
 ****************************************************************************/

input_loop :-
	repeat,
	write('� toi de jouer...> '),
	read_string(String),
	string_tokens(String, Tokens),
 	check_sentence(Tokens, String).


/****************************************************************************

	process_command -
		Process a low level command. The last argument is set to
		true if exit is needed.
 
 ****************************************************************************/
	 	
%
% Sauvegarde
%
process_command(Word, Rest, false) :-
	is_member(Word, [sauvegarde, backup, sauve, sauver, save]),
	do_save(Rest).
	
do_save([]) :-
	save_database('Pizzas.zdb').
do_save(List) :-
	atomlist_concat(List, File),
	save_database(File).


%
% Relecture
%
process_command(Word, Rest, false) :-
	is_member(Word, [load, reload, lire, charger, charge]),
	do_load(Rest).

do_load([]) :-
	load_database('Pizzas.zdb').
do_load(List) :-
	atomlist_concat(List, File),
	load_database(File).



%
% Sortie de la boucle de lecture
%
process_command(Word, _, true) :-
	is_member(Word, [bye, exit, quit, end]),
	write('� bient�t !'), nl.
 

/****************************************************************************

	check_sentence -
		Interpr�te la saisie et cr�e les r�gles n�cessaires.
	 
 ****************************************************************************/
%
% Ligne vide
%
check_sentence([], _) :-
	!, fail.

%
% Mot-cl�
%
check_sentence([Word | Rest], _) :-
	parse_command_line(Rest, Arguments),
	process_command(Word, Arguments, Exit),
	(
		( Exit == false, !, fail ) ;
		( true )
	 ).


%
% Ex�cution d'un pr�dicat prolog
%
check_sentence(['!'| Rest], String ) :-
	sub_string(String, 2, _, StrTerm),
	catch( 
		( string_term(StrTerm, Term), Term ),
		Error, verbose_write([Error, nl])
	      ),
	!, fail.
	

	
	
%
% D�finition d'une pizza.
%
check_sentence(Tokens, _) :-
	sentence(pizza, Name, Supplier, Ingredients, Tokens, []),
	define_pizza(Supplier, Name, Ingredients),
	verbose_write( [
		'D�finition de la pizza ', Name, ' : ', nl,
		tab, 'Fournisseur : ', Supplier, nl,
		tab, 'Ingredients : ', Ingredients
		] ),
	!, fail.


%
% D�finition du fournisseur par d�faut.
%
check_sentence(Tokens, _) :-
	sentence(fournisseur, Supplier, Tokens, []),
	define_default_supplier(Supplier),
	verbose_write( [
		'Le nouveau fournisseur par d�faut est : ',
		Supplier
		] ),
	!, fail.


%
% D�finition de l'utilisateur par d�faut.
%
check_sentence(Tokens, _) :-
	sentence(identite, Identity, Tokens, []),
	define_default_identity(Identity),
	verbose_write( [
		'Bonjour ', Identity, ' !'
		] ),
	!, fail.


%
% D�finition d'une pr�f�rence utilisateur.
%
check_sentence(Tokens, _) :-
	sentence(preference, Name, Ingredients, Notes, Tokens, []),
	define_preference(Name, Ingredients, Notes),
	verbose_write( [
		'D�finition d''une pr�f�rence : ', nl,
		tab, 'Utilisateur : ', Name, nl,
		tab, 'Ingredients : ', Ingredients, nl,
		tab, 'Notes       : ', Notes
		] ),
	!, fail.

%
% D�finition d'une pr�f�rence utilisateur pour une combinaison.
%
check_sentence(Tokens, _) :-
	sentence(combination, Name, Ingredients, Note, Tokens, []),
	define_combination(Name, Ingredients, Note),
	verbose_write( [
		'D�finition d''une pr�f�rence de combinaisons : ', nl,
		tab, 'Utilisateur                : ', Name, nl,
		tab, 'Combinaison d''''ingredients : ', Ingredients, nl,
		tab, 'Note                       : ', Note
		] ),
	!, fail.


%
% Phrase incorrecte.
%
check_sentence(Tokens, String) :-
	verbose_write( [
		'je ne comprends pas la phrase suivante :', nl,
		'"', String, '".'
		]),
	!, fail.
	
	
 