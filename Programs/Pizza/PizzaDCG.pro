/*============================================================================

	File	 : pizzaDCG.pro
	Author	 : Christian Vigh, 2005/07.
	Contents :
		Choix d'une pizza (en utilisant une DCG).

 ============================================================================*/


verbose(1). 



/****************************************************************************

	verbose_write -
		Affiche la liste passée en argument si verbose(1) est vrai.
 
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
	consult('Ingredients.zdb'),		% Liste des ingrédients
	%consult('Pizzas.zdb'),			% Définition des pizzas
	define_default_supplier([]),		% Nom de fournisseur par défaut
	define_default_identity('').		% Identité par défaut de l'utilisateur


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
	write('à toi de jouer...> '),
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
	write('à bientôt !'), nl.
 

/****************************************************************************

	check_sentence -
		Interprète la saisie et crée les règles nécessaires.
	 
 ****************************************************************************/
%
% Ligne vide
%
check_sentence([], _) :-
	!, fail.

%
% Mot-clé
%
check_sentence([Word | Rest], _) :-
	parse_command_line(Rest, Arguments),
	process_command(Word, Arguments, Exit),
	(
		( Exit == false, !, fail ) ;
		( true )
	 ).


%
% Exécution d'un prédicat prolog
%
check_sentence(['!'| Rest], String ) :-
	sub_string(String, 2, _, StrTerm),
	catch( 
		( string_term(StrTerm, Term), Term ),
		Error, verbose_write([Error, nl])
	      ),
	!, fail.
	

	
	
%
% Définition d'une pizza.
%
check_sentence(Tokens, _) :-
	sentence(pizza, Name, Supplier, Ingredients, Tokens, []),
	define_pizza(Supplier, Name, Ingredients),
	verbose_write( [
		'Définition de la pizza ', Name, ' : ', nl,
		tab, 'Fournisseur : ', Supplier, nl,
		tab, 'Ingredients : ', Ingredients
		] ),
	!, fail.


%
% Définition du fournisseur par défaut.
%
check_sentence(Tokens, _) :-
	sentence(fournisseur, Supplier, Tokens, []),
	define_default_supplier(Supplier),
	verbose_write( [
		'Le nouveau fournisseur par défaut est : ',
		Supplier
		] ),
	!, fail.


%
% Définition de l'utilisateur par défaut.
%
check_sentence(Tokens, _) :-
	sentence(identite, Identity, Tokens, []),
	define_default_identity(Identity),
	verbose_write( [
		'Bonjour ', Identity, ' !'
		] ),
	!, fail.


%
% Définition d'une préférence utilisateur.
%
check_sentence(Tokens, _) :-
	sentence(preference, Name, Ingredients, Notes, Tokens, []),
	define_preference(Name, Ingredients, Notes),
	verbose_write( [
		'Définition d''une préférence : ', nl,
		tab, 'Utilisateur : ', Name, nl,
		tab, 'Ingredients : ', Ingredients, nl,
		tab, 'Notes       : ', Notes
		] ),
	!, fail.

%
% Définition d'une préférence utilisateur pour une combinaison.
%
check_sentence(Tokens, _) :-
	sentence(combination, Name, Ingredients, Note, Tokens, []),
	define_combination(Name, Ingredients, Note),
	verbose_write( [
		'Définition d''une préférence de combinaisons : ', nl,
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
	
	
 