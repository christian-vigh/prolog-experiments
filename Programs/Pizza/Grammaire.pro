/*============================================================================

	File	 : Grammaire.pro
	Author	 : Christian Vigh, 2005/07.
	Contents :
		D�finition de la grammaire pour la saisie.

 ============================================================================*/



/****************************************************************************

	sentence -
		Analyse une phrase.
 
 ****************************************************************************/
sentence(pizza, Name, Supplier, Ingredients) -->	% D�finition d'une pizza
	pizza_definition(Name, Supplier, Ingredients).
sentence(pizza, Name, Supplier, Ingredients) --> 
	pizza_definition(Name, Supplier, Ingredients).

sentence(fournisseur,Supplier) -->			% D�finition d'un fournisseur
	% 'fournisseur' sert � faire la diff�rence avec la phrase 'Identity'
	% qui contient le m�me nombre d'arguments.
	supplier_definition(Supplier).		
	
sentence(identite, Identity) --> 			% D�finition de l'identit� de l'interlocuteur
	identity_definition(Identity).

sentence(preference, Name, Ingredients, Notes) -->	% D�finition d'une pr�f�rence
	preference_definition(Name, Ingredients, Notes).
	
sentence(combination, Name, Ingredients, Note) -->	% D�finition d'une pr�f�rence de combinaisons
	combination_definition(Name, Ingredients, Note).


/****************************************************************************
 ****************************************************************************
 ****************************************************************************

	Analyse de phrase : D�finition d'une pizza.
	
	Une phrase de d�finition a la forme suivante :
	
	la pizza [nom] contient de l'ail, des oignons, du fromage a raclette.
	la pizza [nom] est composee d'ail, d'oignons, de fromage a raclette.

	Ou encore, en sp�cifiant le fabricant de la pizza :
	
	la pizza [nom] du fournisseur [fournisseur] contient...
	la pizza [nom] de [fournisseur] contient...
	 
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************/

/****************************************************************************

	pizza_definition -
		Analyse la d�finition d'une pizza.
 
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
		Le nom du fournisseur est optionnel dans la d�finition d'une
		pizza.
	 
 ****************************************************************************/
optional_supplier(Supplier) -->			% Parsing du nom du fournisseur
	([du, fournisseur] ; [de]),
	supplier_name(Supplier).

optional_supplier(Supplier) -->			% ... ou recherche du fournisseur par d�faut
	{ supplier(Supplier) },
	[].

optional_supplier(Supplier) -->			% rien de tout �a : le fournisseur sera une liste vide
	{
		Supplier = []
	},
	[].

/****************************************************************************

	sentence_ingredients -
		Analyse une liste d'ingr�dients.
 
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
		Analyse un ingredient, avec un article d�fini ou ind�fini.
 
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
		Retourne l'article d�fini ou ind�fini correspondant au nom
		d'ingr�dient trait� par sentence_ingredient.
 
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

	Analyse de phrase : D�finition d'un fournisseur.
	
	La d�finition d'un fournisseur peut se faire de deux fa�ons :
	. Soit en d�finissant un fournisseur par d�faut � utiliser pour 
	  chaque nouvelle d�finition de pizza :
	  
	  Les pizzas du fournisseur [nom du fournisseur] :
	  Les pizzas de [nom du fournisseur] :
	  Le fournisseur par defaut est [nom du fournisseur].
	   
	. Soit en r�f�ren�ant le fournisseur dans la d�finition de pizza :
	
	  la pizza [nom de pizza] du fournisseur [nom du fournisseur] contient...
	 
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************/


/****************************************************************************

	Analyse de la phrase : D�finition d'un fournisseur par d�faut.
	 
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
		Analyse le nom d'un fournisseur (qui peut �tre compos�).
	 
 ****************************************************************************/
supplier_name([]) --> [].
supplier_name([H|T]) -->
	[H],
	supplier_name(T).	



/****************************************************************************
 ****************************************************************************
 ****************************************************************************

	Analyse de phrase : D�finition de l'identit� de l'utilisateur.
	
	La d�finition d'un utilisateur peut se faire de deux fa�ons :
	. Soit en d�finissant un utilisateur par d�faut � utiliser pour 
	  chaque nouvelle assertion de pr�f�rences :
	  
	  je suis [Nom].
	  je prefere le bacon.
	  	   
	. Soit en r�f�ren�ant l'utilisateur dans l'assertion de pr�f�rences :
	
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

	Analyse de phrase : D�finition d'une pr�f�rence
	
	Une phrase de d�finition a la forme suivante :
	
	j'aime le bacon : 12/20, l'ail : 13/20, ...

	Ou encore, en sp�cifiant le nom de l'utilisateur :
	
	[utilisateur] aime 	le bacon : 12/20
		      prefere
		      note
	 
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************/

/****************************************************************************

	preference_definition -
		Analyse la d�finition d'une pr�f�rence.
 
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
		Analyse une liste d'ingr�dients avec leurs notes.
 
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
		D�compose une note.
 
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

	Analyse de phrase : D�finition d'une pr�f�rence pour une combinaison
	d'ingr�dient.
	
	Une phrase de d�finition a la forme suivante :
	
	j'aime combiner le bacon, l'ail , ... : 15/20

	Ou encore, en sp�cifiant le nom de l'utilisateur :
	
	[utilisateur] aime combiner 	le bacon, l'ail, ... : 15/20
	 	     
	 
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************/

/****************************************************************************

	combination_definition -
		Analyse la d�finition d'une pr�f�rence de combinaison.
 
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
		Analyse une liste d'ingr�dients avec leurs notes.
 
 ****************************************************************************/
combination(Ingredients, Note, Def) -->
	sentence_ingredients(Ingredients, Def),
	sentence_note(Note).
	


/****************************************************************************
 ****************************************************************************
 ****************************************************************************

	Diff�rentes r�gles pour la ponctuation et les mots de liaison.
	 
 ****************************************************************************
 ****************************************************************************
 ****************************************************************************/
	
/****************************************************************************

	S�parateurs.
	 
 ****************************************************************************/

dot 	--> ( [.] ; ['. '] ; [] ).			% dot
comma 	--> [','].
colon   --> [':'].

/****************************************************************************

	Autres �l�ments.
	 
 ****************************************************************************/
optional_and --> [et].			% Le 'et' est optional avant le dernier �l�ment d'une liste
optional_and --> [].