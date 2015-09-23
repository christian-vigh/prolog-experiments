/*============================================================================

	File	 : Query.pro
	Author	 : Christian Vigh, 2005/08.
	Contents :
		Gestion des questions.

 ============================================================================*/

 
 
 /****************************************************************************

	que fabrique 'fournisseur' ?
	
	que contient la pizza 'nom' de 'fournisseur' ?
	quelle pizza contient 'ingredients' ?
	quel est le meilleur choix pour 'fournisseur'
	quelle est la note de la pizza 'nom' de 'fournisseur' ?
	qui fabrique la pizza 'nom' ?
	quelles sont les préférences de 'nom' ?
	quels sont les fournisseurs ?
 
 ****************************************************************************/
when query :
	que fabrique parameter:fournisseur,
execute  show_supplier_pizzas with fournisseur.

when query : 
	quelle est la note de [la pizza] parameter:name 
		[[de, du fournisseur] parameter:fournisseur],
execute show_pizza_note with fournisseur, name.

when query :
	quelle pizza contient parameter:ingredients(ingredients),