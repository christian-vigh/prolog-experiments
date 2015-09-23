/*============================================================================

	File	 : LoadSave.pro
	Author	 : Christian Vigh, 2005/08.
	Contents :
		Chargement/Sauvegarde des règles dynamiques.

 ============================================================================*/

 

/****************************************************************************

	get_article - 
		Récupère l'article correspondant à l'ingrédient spécifié.
 
 ****************************************************************************/
get_article(A, defined, Result) :-
 	article(A, Result, _, _).
 
get_article(A, undefined, Result) :-
 	article(A, _, Result, _).
 	
get_article(A, complement, Result) :-
	article(A, _, _, Result). 
 

/****************************************************************************

	write_wordlist - 
		Ecrit une liste de mots.
 
 ****************************************************************************/
write_wordlist(File, List, ExtraSpace) :-
 	write_wordlist(File, List, '', ExtraSpace).
 
write_wordlist(File, [''''], _, _) :-
	write(File, '''').
write_wordlist(File, [], _, ExtraSpace) :-
	write(File, ExtraSpace).
write_wordlist(File, [Head|Tail], Space, ExtraSpace) :-
	write(File, Space),
	write(File, Head),
	write_wordlist(File, Tail, ' ', ExtraSpace).
 
 
/****************************************************************************

	write_ingredients - 
		Ecrit une liste d'ingrédients, en utilisant le type d'article
		spécifié.
 
 ****************************************************************************/
write_ingredients(_, [], _).
write_ingredients(File, [H|T], Def) :-
	write_ingredient(File, H, Def),
	write_optional_comma(File, T),
	write_ingredients(File, T, Def).
	
write_ingredient(File, Ingredient, Def) :-
	get_article(Ingredient, Def, Result),
	write_wordlist(File, Result, ' '),
	write_wordlist(File, Ingredient, '').
	

write_optional_comma(_, []).
write_optional_comma(File, _) :-
	write(File, ', ').


/****************************************************************************

	save_default_supplier - 
		Sauvegarde le fournisseur par défaut s'il est défini.
 
 ****************************************************************************/
save_default_supplier(File) :-
	default_supplier(X),
	X \== [],	
	write(File, 'le fournisseur par defaut est '),
	write(File, X),
	write(File, '.'), nl(File).
save_default_supplier(File).


/****************************************************************************

	save_default_identity - 
		Sauvegarde l'utilitsateur par défaut s'il est défini.
 
 ****************************************************************************/
save_default_identity(File) :-
	default_identity(X),
	X \== '',	
	write(File, 'je me nomme '),
	write(File, X),
	write(File, '.'), nl(File).
save_default_identity(File).


/****************************************************************************

	save_pizzas - 
		Sauvegarde les définitions de pizza.
 
 ****************************************************************************/
save_pizzas(File) :-
	setof( X, Name ^ Ingredient ^ pizza(X, Name, Ingredient), Suppliers),
	save_pizzas_by_supplier(File, Suppliers).

save_pizzas(File).			% No pizza to save	

save_pizzas_by_supplier(_, []).
save_pizzas_by_supplier(File, [Supplier|Tail]) :-
	setof(X, Ingredient ^ pizza(Supplier, X, Ingredient), Pizzas),
	save_pizza(File, Supplier, Pizzas),
	save_pizzas_by_supplier(File, Tail).
	
save_pizza(_, _, []).
save_pizza(File, Supplier, [Pizza|Tail]) :-
	findall(X, pizza(Supplier, Pizza, X), Ingredients),
	write_pizza(File, Supplier, Pizza, Ingredients),
	save_pizza(File, Supplier, Tail).
	
write_pizza(File, Fournisseur, Nom, Ingredients) :-
	write(File, 'la pizza '), write(File, Nom), write(File, ' '),
	optional_pizza_supplier(File, Fournisseur),
	write(File, 'contient '),
	write_ingredients(File, Ingredients, undefined),
	write(File, '.'),
	nl(File).
	
optional_pizza_supplier(_, []).
optional_pizza_supplier(File, Fournisseur) :-
	write(File, 'du fournisseur '), write(File, Fournisseur), 
	write(File, ' ').

	
/****************************************************************************

	save_preferences - 
		Sauvegarde les préférences.
 
 ****************************************************************************/
save_preferences(File) :-
	setof(X, Ingredient ^ Note ^ preference(X, Ingredient, Note), Users),
	save_preferences_by_user(File, Users).

save_preferences(File).			% No preferences to save.
	
save_preferences_by_user(_, []).
save_preferences_by_user(File, [Identity|Tail]) :-
	setof(X, Note ^ preference(Identity, X, Note), Ingredients),
	write_user_start(File, Identity),
	write_preferences(File, Identity, Ingredients),
	write(File, '.'), nl(File),
	save_preferences_by_user(File, Tail).

write_preferences(_, _, []).
write_preferences(File, Identity, [Ingredient|Tail]) :-
	write_ingredient(File, Ingredient, defined),
	write(File, ' : '),
	preference(Identity, Ingredient, Note),
	write(File, Note),
	write(File, '/20'),
	write_optional_comma(File, Tail),
	write_preferences(File, Identity, Tail).

write_user_start(File, '') :-
	write(File, 'j''aime ').
write_user_start(File, Identity) :-
	write(File, Identity), write(File, ' aime ').
	

/****************************************************************************

	save_combinations - 
		Sauvegarde les préférences.
 
 ****************************************************************************/
save_combinations(File) :-
	setof(X, List ^ Ingredient ^ Note ^ combination(X, List, Ingredient, Note), Users),
	save_combinations_by_user(File, Users).

save_combinations(File).		% No combination to save.

	
save_combinations_by_user(_, []).
save_combinations_by_user(File, [Identity|Tail]) :-
	setof(X, List ^ Note ^ combination(Identity, List, X, Note), Ingredients),
	write_combine_start(File, Identity),
	write_combinations(File, Identity, Ingredients),
	write(File, ' : '),
	combination(Identity, _, Ingredient, Note),
	write(File, Note),
	write(File, '/20'),
	write(File, '.'), nl(File),
	save_combinations_by_user(File, Tail).

write_combinations(_, _, []).
write_combinations(File, Identity, [Ingredient|Tail]) :-
	write_ingredient(File, Ingredient, defined),
	write_optional_comma(File, Tail),
	write_combinations(File, Identity, Tail).

write_combine_start(File, '') :-
	write(File, 'j''aime combiner ').
write_combine_start(File, Identity) :-
	write(File, Identity), write(File, ' aime combiner ').
	
	
	
/****************************************************************************

	save_database - 
		Sauvegarde la base de données complète.
 
 ****************************************************************************/
save_database(Filename) :-
	open(Filename, write, ID),
	save_default_supplier(ID),
	save_default_identity(ID),
	save_pizzas(ID),
	save_preferences(ID),
	save_combinations(ID),
	close(Filename).
	
	
/****************************************************************************

	load_database - 
		Charge une base de données complète.
 
 ****************************************************************************/
load_database(Filename) :-
	open(Filename, read, ID),
	load_file(ID),
	close(ID).
	
	
/****************************************************************************

	load_file - 
		Effectue le chargement.
 
 ****************************************************************************/
load_file(ID) :-
	repeat,
	read_string(ID, String),
	load_line(String).
	
load_line(end_of_file).

load_line(String) :-
	string_tokens(String, Tokens),
 	check_sentence(Tokens, String),
 	!, fail.
