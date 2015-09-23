/****h* Modules/Properties
 ===============================================================================
 *
 * NAME
 *	Properties - Property-handling predicates.
 *
 * FILE
 *	Modules/Properties.pro
 *
 * CONTENTS
 *	Implements property bags as dynamic clauses.
 *
 * AUTHOR
 *	Christian Vigh, July 2005.
 *
 ===============================================================================
 ******/

:- module(properties).

:-	export(property_bag/1).
:-	export(property_names/2).
:- 	export(property/3).
:-	export(property_set/3).
:- 	export(property_get/3).
:-	export(is_property/2).
:-	export(property_delete/2).
:-	export(property_bag_delete/1).
 
:- end_module(properties).
 
 :- body(properties).




/****f* Modules.Properties/property_bag
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	property_bag/1
 *
 * SYNTAX
 *	property_bag(X)
 *
 * PURPOSE
 *	Creates a property bag of name [X].
 *
 * ARGUMENTS
 *	[X] (i) -
 *		Name of new property bag.
 *
 * NOTES
 *	Property bags are implemented as clauses.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

property_bag(X) :-
	assert(properties:propertyBag(X)).

/******/




/****f* Modules.Properties/property, property_get, property_set
 -------------------------------------------------------------------------------
 *
 * PREDICATE
*	 property/3, property_set/3, property_get/3
 *
 * SYNTAX
 *	property(Bag, Name, Value)
 *	property_set(Bag, Name, Value)
 *	property_get((Bag, Name, Value)
 *
 * PURPOSE
 *	property/3 unifies [Value] with the value of the property [Name] in [Bag],
 *	if [Value] is a variable (= property_get/3).
 *	If [Value] is not a variable, then property/3 sets the value of the property 
 *	[Name] in [Bag] to [Value] (= property_get/3).
 *
 * ARGUMENTS
 *	[Bag] (i) -
 *		Property bag to look for.
 *	[Name] (i) -
 *		Name of the property.
 *	[Value] (i,o) -
 *		When bound, specifies the new value for [Name].
 *		Otherwise, unified to the value of property [Name] in [Bag].
 *
 * NOTES
 *	property_set can be used to add new properties in [Bag].
 *	To check for the presence of a property, use is_property/2.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

property(Bag, Name, Value) :-
	var(Value),
	property_get(Bag, Name, Value),
	!.
property(Bag, Name, Value) :-
	property_set(Bag, Name, Value).


%
% Implementation of property_set.
%
property_set(Bag, Name, Value) :-
	propertyBag(Bag),
	property_delete(Bag, Name),
	assert((
		properties:propertyBagItem(Bag, Name, Value) :-
			propertyBag(Bag)
		)).
	

%
% implementation of property_get.
%	
property_get(Bag, Name, Value) :-
	propertyBagItem(Bag, Name, Value).

/******/





/****f* Modules.Properties/is_property
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	is_property/2
 *
 * SYNTAX
 *	is_property(Bag, Name)
 *
 * PURPOSE
 *		Succeeds if [Name] is a property of [Bag].
 *
 * ARGUMENTS
 *	[Bag] (i) -
 *		Property bag to look into.
 *	[Name] (i) -
 *		Name of the property to check.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

is_property(Bag, Name) :-
	propertyBagItem(Bag, Name, _).

/******/





/****f* Modules.Properties/property_delete
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	property_delete/2
 *
 * SYNTAX
 *	property_delete(Bag, Name)
 *
 * PURPOSE
 *	Deletes the property [Name] from property bag [Bag].
 *
 * ARGUMENTS
 *	[Bag] (i) -
 *		Bag to check into.
 *	[Name] (i) -
 *		Name of the property to delete.
 *
 * NOTES
 *		property_delete always succeed.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

property_delete(Bag, Name) :-
	retractall(( 
		properties:propertyBagItem(Bag, Name, _) :- 
			propertyBag(Bag) )).

/******/





/****f* Modules.Properties/property_bag_delete
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	property_bag_delete/1
 *
 * SYNTAX
 *	property_bagdelete(Bag)
 *
 * PURPOSE
 *	Deletes the specified [Bag] and all its properties.
 *
 * ARGUMENTS
 *	[Bag] (i) -
 *		Bag to check into.
 *
 * NOTES
 *	property_bag_delete always succeed.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

property_bag_delete(Bag) :-
	retractall(( 
		properties:propertyBagItem(Bag, _, _) :- 
			propertyBag(Bag) 
		)),
	retractall(
		properties:propertyBag(Bag)
		).

/******/





/****f* Modules.Properties/property_names
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	property_names/2
 *
 * SYNTAX
 *	property_names(Bag, Result)
 *
 * PURPOSE
 *	Unifies [Result] with the list of names of all the properties defined in [Bag].
 *
 * ARGUMENTS
 *	[Bag] (i) -
 *		Property back to look into.
 *	[Result] (o) -
 *		List of properties defined in [Bag].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

property_names(Bag, Result) :-
	findall(X, propertyBagItem(Bag, X, _), Result).

/******/	

 :- end_body(properties).
 