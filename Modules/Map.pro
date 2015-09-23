/****h* Modules/Map
 ===============================================================================
 *
 * NAME
 *	Map - Predicates for handling ordered-maps.
 *
 * FILE
 *	Modules/Map.pro
 *
 * CONTENTS
 *	Implements Maps.
 *	A map is a structure containing an ordered set of key:value pairs. Keys
 *	are unique.
 *	The underlying implementation of a map is similar to Prolog's implementation
 *	of lists ; thus, a map containing :
 *
 *		[key1:value1, key2:value2, key3:value3]
 *
 *	will be represented as :
 *
 *		map( key1, value1, 
 *			map( key2, value2,
 *				map( key3, value3,
 *					map ) ) ).
 *
 *	So, map elements are forward-chained using the map/3 predicate. Note that
 *	the last element of the chain is simply the atom 'map'. 
 *	An empty map is also represented as the atom 'map'.
 *
 *	A map is created using map_from_list/2, which takes a list of key:value
 *	pairs, then creates a map. 
 *	A map can be converted to a list using the map_to_list/2 predicate.
 *	Use map_value/3 to retrieve the value of a map entry.
 *	The map_print/1 predicate can be used to pretty-print the contents of a map.
 * 
 * TODO
 *	Implement other operations on maps such as :
 *	* map_intersect
 *	* map_add
 *	* map_remove
 *	* etc.
 *
 * AUTHOR
 *	Christian Vigh, February 2007 (subset based on R.A. O'Keefe map package).
 *
 ===============================================================================
 ******/


:- module(map).

:- 	export(map_from_list/2).
:-	export(map_to_list/2).
:- 	export(is_map/1).
:-	export(map_union/3).
:-	export(map_value/3).
:-	export(map_print/1).

:- end_module(map).


:- body(map).



/****f* Modules.Map/map_from_list
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	map_from_list/2
 *
 * SYNTAX
 *	map_from_list( List, Map ).
 *
 * PURPOSE
 *	Unifies [Map] with the map formed of all the elements in [List].
 *	Each element in [List] is a Key:Value pair.
 *
 * ARGUMENTS
 *	[List] (i) -
 *		List of Key:Value pairs used to create the map.
 *	[Map] (o) -
 *		Variable which will be unified to the map formed from all the
 *		Key:Value pairs taken from [List].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

map_from_list( List, Map ) :-
	sort(List, Sorted),
	do_map_from_list( Sorted, Map ).
	
do_map_from_list([], map).

do_map_from_list( [Key:Value | Tail], map(Key, Value, Map) ) :-
	do_map_from_list(Tail, Map).

/******/




/****f* Modules.Map/map_to_list
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	map_to_list/2
 *
 * SYNTAX
 *	map_to_list( Map, List )
 *
 * PURPOSE
 *	Generates a list of all the key:value pairs present in [Map].
 *
 * ARGUMENTS
 *	[Map] (i) -
 *		Map containing the elements to be converted into a list.
 *	[List] (o) -
 *		Variable which will be unified with the key:value pairs defined
 *		in [Map].
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

map_to_list( map, [] ).
map_to_list( map(Key, Value, Map), [Key:Value | List] ) :-
	map_to_list(Map, List).


/******/	




/****f* Modules.Map/is_map
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	is_map/1
 *
 * SYNTAX
 *	is_map( Map )
 *
 * PURPOSE
 *	Succeeds if [Map] is a map. is_map/1 also checks the lexical order of the
 *	elements in [Map].
 *
 * ARGUMENTS
 *	[Map] (i) -
 *		Map to be checked.
 *
 * SUCCEEDS IF
 *	[Map] is a map containing an ordered list of Key/Value pairs.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

is_map(map).
is_map( map(Key, _, Map) ) :-	
	nonvar(Map),
	is_map(Map, Key).

is_map( map, _ ).
is_map( map( Key, _, Map ), PreviousKey ) :-
	nonvar(Map),
	PreviousKey @< Key,
	is_map(Map, Key).

/******/





/****f* Modules.Map/map_union
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	map_union/3
 *
 * SYNTAX
 *	map_union( Map1, Map2, Result )
 *
 * PURPOSE
 *	Merges the maps [Map1] and [Map2] and unifies [Result] with the result.
 *	The predicate will fail if [Map1] and [Map2] contain the same key but
 *	with a different value.
 *	The merge is performed according to the lexical order of each element.
 *
 * ARGUMENTS
 *	[Map1], [Map2] (i) -
 *		Maps to be merged.
 *
 *	[Result] (o) -
 *		Variable unified with the results of the merge.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

map_union( Map1,  map, Map1 ).
map_union(  map, Map2, Map2 ).
map_union(  map,  map,  map ).

map_union( map(Key1, Value1, Map1), map(Key2, Value2, Map2), 
			map(Key1, Value1, Union) ) :-
	Key1 @< Key2,
	map_union(Map1, map( Key2, Value2, Map2 ), Union ).

map_union( map(Key1, Value1, Map1), map(Key2, Value2, Map2), 
		map(Key2, Value2, Union) ) :-
	Key1 @> Key2,
	map_union( map( Key1, Value1, Map1 ), Map2, Union).

map_union( map(Key, Value, Map1), map(Key, Value, Map2), 
		map(Key, Value, Union) ) :-
	map_union( Map1, Map2, Union ).

/******/
	
	
	


/****f* Modules.Map/map_print
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	map_print/1
 *
 * SYNTAX
 *	map_print(Map)
 *
 * PURPOSE
 *	pretty prints the specified map.
 *
 * ARGUMENTS
 *	[Map] (i) -
 *		Map whose contents are to be printed. [Map] must either be the atom
 *		'map' (an empty map) or a bound variable.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

map_print( Map ) :-
	nonvar(Map),
	write( 'map' ), nl,
	write( '   {' ), nl,
	map_print_element( Map ),
	write( '    }'), nl.

map_print_element( map(Key, Value, Map) ) :-
	tab(7), write(Key), write( '-> '), write(Value), nl,
	map_print_element( Map ).
map_print_element( map ).

/******/





/****f* Modules.Map/map_value
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	map_value/3
 *
 * SYNTAX
 *	map_value( Map, Key, Result )
 *
 * PURPOSE
 *	Retrieves in [Map] the value of the element whose key is [Key]. [Result]
 *	is instanciated with the corresponding value.
 *
 * ARGUMENTS
 *	[Map] (i) -
 *		Map to search in.
 *	[Key] (i) -
 *		Key to search for.
 *	[Result] (o) -
 *		Corresponding key value.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

map_value( map(Key, Value, Map), Key, Value ) :- !.
map_value( map(  _,     _, Map), Key, Value ) :-
	map_value( Map, Key, Value ), !.

/******/	
	
:- end_body(map).
