/*============================================================================

	File	 : Regles.pro
	Author	 : Christian Vigh, 2005/08.
	Contents :
		Gestion des r�gles dynamiques.

 ============================================================================*/

 
 
 /****************************************************************************

	define_default_supplier, define_default_identity -
		D�finit le fournisseur/utilisateur par d�faut.
 
 ****************************************************************************/
define_default_supplier(Name) :-
 	abolish( default_supplier/1 ),
 	assert( default_supplier( Name) ).

supplier(X) :-
	var(X),
	default_supplier(X).
supplier(X).
 	
 	
define_default_identity(Name) :-
 	abolish( default_identity/1 ),
 	assert( default_identity( Name) ).

identity(X) :-
	var(X),
	default_identity(X).
identity(X).
	

 /****************************************************************************

	define_pizza -
		D�finit une pizza et son contenu.
 
 ****************************************************************************/
 define_pizza(Name, List) :-
 	supplier(X),
 	define_pizza(X, Name, List).
 	
 define_pizza(X, Name, List) :-
 	supplier(X),
 	retractall( pizza(X, Name, _) ),
 	define_pizza_elements(X, Name, List).
 	
 define_pizza_elements(_, _, []).
 define_pizza_elements(Supplier, Name, [H|T]) :-
 	assert( pizza(Supplier, Name, H) ),
 	define_pizza_elements(Supplier, Name, T).


 /****************************************************************************

	define_preference -
		D�finit une pr�f�rence pour un ingredient.
 
 ****************************************************************************/
 define_preference(Ingredients, Notes) :-
 	identity(X),
 	define_preference(X, Ingredients, Notes).
 	
 define_preference(Id, Ingredients, Notes) :-
 	identity(Id),
 	retractall( preference(Id, Ingredients, _) ),
 	define_preference_elements(Id, Ingredients, Notes).
 	
 
 define_preference_elements(_, _, []).
 define_preference_elements(_, [], _).
 define_preference_elements(Identity, [IH|IT], [NH|NT]) :-
 	assert( preference( Identity, IH, NH) ),
 	define_preference_elements(Identity, IT, NT).
 	
 	
/****************************************************************************

	define_combination -
		D�finit une pr�f�rence pour une combinaison d'ingredients.
 
 ****************************************************************************/
define_combination(Ingredients, Note) :-
 	identity(X),
 	define_combination(X, Ingredients, Note).
 	
define_combination(Identity, Ingredients, Note) :-
 	retractall( combination(Identity, Ingredients, _) ),
	define_combination_elements(Identity, Ingredients, Ingredients, Note).

	
define_combination_elements(_, _, [], _).
define_combination_elements(Identity, List, [H|T], Note) :-
	assert( combination( Identity, List, H, Note) ),
	define_combination_elements(Identity, List, T, Note).
