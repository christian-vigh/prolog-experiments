/*============================================================================

	File	 : Grammaire.pro
	Author	 : Christian Vigh, 2005/07.
	Contents :
		Définition de la grammaire pour la saisie.

 ============================================================================*/



/****************************************************************************

	sentence -
		Analyse une phrase.
 
 ****************************************************************************/
sentence(pizza, Name, Supplier, Ingredients) -->	% Définition d'une pizza
	pizza_definition(Name, Supplier, Ingredients).
sentence(pizza, Name, Supplier, Ingredients) --> 
	pizza_definition(Name, Supplier, Ingredients).

sentence(fournisseur,Supplier) -->			% Définition d'un fournisseur
	% 'fournisseur' sert à faire la différence avec la phrase 'Identity'
	% qui contient le même nombre d'arguments.
	supplier_definition(Supplier).		
	
sentence(identite, Identity) --> 			% Définition de l'identité de l'interlocuteur
	identity_definition(Identity).

sentence(preference, Name, Ingredients, Notes) -->	% Définition d'une préférence
	preference_definition(Name, Ingredients, Notes).
	
sentence(combination, Name, Ingredients, Note) -->	% Définition d'une préférence de combinaisons
	combination_definition(Name, Ingredients, Note).


/****************************************************************************
 ****************************************************************************
 ****************************************************************************

	Analyse de phrase : Définition d'une pizza.
	
	Une phrase de définition a la forme suivante :
	
	la pizza [nom] contient de l'ail, des oignons, du fromage a raclette.
	la pizza [nom] est composee d'ail, d'oignons, de fromage a raclette.

	Ou encore, en spécifiant le fabricant de la pizza :
	
	la pizza [nom] du fournisseur [fournisseur] contient...
	la pizza [nom] de [fournisseur] contient...
	 
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************/

/****************************************************************************

	pizza_definition -
		Analyse la définition d'une pizza.
 
 ****************************************************************************/
pizza_definition(Name, Supplier, Ingredients) -->
	[la, pizza],
	[Name],
	optional_supplier(Supplier),
	[contient],
	sentence_ingredients(Ingredients, undefined),
	dot.

pizza_definition(Name, Supplier, Ingredients) -->
	[la, pizza],
	[Name],
	optional_supplier(Supplier),
	[est, composee],
	sentence_ingredients(Ingredients, complement),
	dot.


/****************************************************************************

	optional_supplier -
		Le nom du fournisseur est optionnel dans la définition d'une
		pizza.
	 
 ****************************************************************************/
optional_supplier(Supplier) -->			% Parsing du nom du fournisseur
	([du, fournisseur] ; [de]),
	supplier_name(Supplier).

optional_supplier(Supplier) -->			% ... ou recherche du fournisseur par défaut
	{ supplier(Supplier) },
	[].

optional_supplier(Supplier) -->			% rien de tout ça : le fournisseur sera une liste vide
	{
		Supplier = []
	},
	[].

/****************************************************************************

	sentence_ingredients -
		Analyse une liste d'ingrédients.
 
 ****************************************************************************/
sentence_ingredients([], _) --> [].

sentence_ingredients([H|T], Def) -->
	sentence_ingredient(H, Def),
	comma,
	sentence_ingredients(T, Def).

sentence_ingredients([H|T], Def) -->
	optional_and,
	sentence_ingredient(H, Def),
	sentence_ingredients(T, Def).


	


/****************************************************************************

	sentence_ingredient -
		Analyse un ingredient, avec un article défini ou indéfini.
 
 ****************************************************************************/
sentence_ingredient(Name, Def) -->
	sentence_article(Name, Def),
	sentence_match(Name).
	

sentence_match([]) --> [].
sentence_match([H|T]) -->
	[H],
	sentence_match(T).


/****************************************************************************

	sentence_article -
		Retourne l'article défini ou indéfini correspondant au nom
		d'ingrédient traité par sentence_ingredient.
 
 ****************************************************************************/
sentence_article(Name, defined) -->
	{
		article(Name, Articles, _, _)
	 },
	sentence_article_match(Name, Articles).
		
sentence_article(Name, undefined) -->
	{
		article(Name, _, Articles, _)
	 },
	sentence_article_match(Name, Articles).

sentence_article(Name, complement) -->
	{
		article(Name, _, _, Articles)
	 },
	sentence_article_match(Name, Articles).


%
% Matches all elements of the article one by one
%
sentence_article_match(Name, []) --> [].
sentence_article_match(Name, [H|T]) -->
	[H],
	sentence_article_match(Name, T).	



/****************************************************************************
 ****************************************************************************
 ****************************************************************************

	Analyse de phrase : Définition d'un fournisseur.
	
	La définition d'un fournisseur peut se faire de deux façons :
	. Soit en définissant un fournisseur par défaut à utiliser pour 
	  chaque nouvelle définition de pizza :
	  
	  Les pizzas du fournisseur [nom du fournisseur] :
	  Les pizzas de [nom du fournisseur] :
	  Le fournisseur par defaut est [nom du fournisseur].
	   
	. Soit en référençant le fournisseur dans la définition de pizza :
	
	  la pizza [nom de pizza] du fournisseur [nom du fournisseur] contient...
	 
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************/


/****************************************************************************

	Analyse de la phrase : Définition d'un fournisseur par défaut.
	 
 ****************************************************************************/
supplier_definition(Supplier) -->
	([les, pizzas, du, fournisseur] ; [les, pizzas, de]),
	supplier_name(Supplier),
	colon.
	
supplier_definition(Supplier) --> 
	[le],
	( [nouveau] ; []), 
	[fournisseur], 
	( [par, defaut] ; [] ),
	[est],
	supplier_name(Supplier),
	dot.

	
/****************************************************************************

	supplier_name -
		Analyse le nom d'un fournisseur (qui peut être composé).
	 
 ****************************************************************************/
supplier_name([]) --> [].
supplier_name([H|T]) -->
	[H],
	supplier_name(T).	



/****************************************************************************
 ****************************************************************************
 ****************************************************************************

	Analyse de phrase : Définition de l'identité de l'utilisateur.
	
	La définition d'un utilisateur peut se faire de deux façons :
	. Soit en définissant un utilisateur par défaut à utiliser pour 
	  chaque nouvelle assertion de préférences :
	  
	  je suis [Nom].
	  je prefere le bacon.
	  	   
	. Soit en référençant l'utilisateur dans l'assertion de préférences :
	
	  [albert] prefere le bacon.
	 
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************/
identity_definition(Identity) -->
	( [je, suis] ; [je, m, '''', appelle] ; [je, me, nomme] ),
	[Identity],
	dot.
	

/****************************************************************************
 ****************************************************************************
 ****************************************************************************

	Analyse de phrase : Définition d'une préférence
	
	Une phrase de définition a la forme suivante :
	
	j'aime le bacon : 12/20, l'ail : 13/20, ...

	Ou encore, en spécifiant le nom de l'utilisateur :
	
	[utilisateur] aime 	le bacon : 12/20
		      prefere
		      note
	 
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************/

/****************************************************************************

	preference_definition -
		Analyse la définition d'une préférence.
 
 ****************************************************************************/
preference_definition(Name, Ingredients, Notes) -->
	[j, '''', aime],
	{ identity(Name) },
	sentence_notes(Ingredients, Notes, defined),
	dot.
	
preference_definition(Name, Ingredients, Notes) -->
	[Name],
	( [aime] ; [prefere] ; [note] ),
	sentence_notes(Ingredients, Notes, defined),
	dot.


/****************************************************************************

	sentence_notes -
		Analyse une liste d'ingrédients avec leurs notes.
 
 ****************************************************************************/
sentence_notes([], [],  _) --> [].

sentence_notes([IH|IT], [NH|NT], Def) -->
	sentence_ingredient(IH, Def),
	sentence_note(NH),
	comma,
	sentence_notes(IT, NT, Def).

sentence_notes([IH|IT], [NH|NT], Def) -->
	optional_and,
	sentence_ingredient(IH, Def),
	sentence_note( NH ),
	sentence_notes(IT, NT, Def).
	

/****************************************************************************

	sentence_note -
		Décompose une note.
 
 ****************************************************************************/
sentence_note(Note) -->
	colon, 
	[Note], 
	( ['/', '20'] ; [] ).
sentence_note(Note) -->
	{ Note = 15 },
	[].



/****************************************************************************
 ****************************************************************************
 ****************************************************************************

	Analyse de phrase : Définition d'une préférence pour une combinaison
	d'ingrédient.
	
	Une phrase de définition a la forme suivante :
	
	j'aime combiner le bacon, l'ail , ... : 15/20

	Ou encore, en spécifiant le nom de l'utilisateur :
	
	[utilisateur] aime combiner 	le bacon, l'ail, ... : 15/20
	 	     
	 
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************/

/****************************************************************************

	combination_definition -
		Analyse la définition d'une préférence de combinaison.
 
 ****************************************************************************/
combination_definition(Name, Ingredients, Note) -->
	[Name],
	( [combine] ; [aime, combiner] ),
	combination(Ingredients, Note, defined),
	dot.

combination_definition(Name, Ingredients, Note) -->
	( [je, combine] ; [j, '''', aime, combiner] ),
	{ identity(Name) },
	combination(Ingredients, Note, defined),
	dot.


/****************************************************************************

	combination -
		Analyse une liste d'ingrédients avec leurs notes.
 
 ****************************************************************************/
combination(Ingredients, Note, Def) -->
	sentence_ingredients(Ingredients, Def),
	sentence_note(Note).
	


/****************************************************************************
 ****************************************************************************
 ****************************************************************************

	Différentes règles pour la ponctuation et les mots de liaison.
	 
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************/
	
/****************************************************************************

	Séparateurs.
	 
 ****************************************************************************/

dot 	--> ( [.] ; ['. '] ; [] ).			% dot
comma 	--> [','].
colon   --> [':'].

/****************************************************************************

	Autres éléments.
	 
 ****************************************************************************/
optional_and --> [et].			% Le 'et' est optional avant le dernier élément d'une liste
optional_and --> [].