/*==========================================================================

	File	 : class.pro
	Author	 : Christian Vigh, 2005/07.
	Contents :
		Implements object classes.

 ============================================================================*/

/*:- module(class).

:- 	export(class).

:- end_module(class).
 

:- body(class).
*/

:- include('Class/ErrorHandler.pro').
:- include('Class/ClassDefinition.pro').
:- include('Class/Properties.pro').
:- include('Class/Write.pro').


/****************************************************************************

	Operator definitions. 
 
 ****************************************************************************/

:- op(900, fx , class).			% Keyword for the definition of a class
:- op(910, xfx, interface).		% Specifies the interface of a class
:- op(900, fx , property).		% Keyword for defining a property
:- op(900, fx , method).		% ... and a method
:- op(910, xfx, =>).			% Operator to specify property attributes
:- op(900, fx , forget).		% Operator to erase the definition of a class
:- op(900, fx , [class, write]).	% Operator to output the definition of a class
:- op(910, xfx, on).			% For use with class write to specify output file id




/****************************************************************************
 
 	operator 	: class Name interface Expression
 	Purpose		:
 		Definition of the interface section for class [Name].
 		[Expression] is a list of properties or method definitions
 		(see the description of the property/method operators).
 	Arguments	:
 		[Name] (i) - 
 			Name of the class to be defined.
 		[Expression] (i) -
 			List of property and method definitions.
 
 ****************************************************************************/

class Name interface Expression :-
	catch(
		( start_class_definition(Name),
			scan_properties( Name, Expression ),
		  end_class_definition(Name) ),
		Error, error_handler(Error)
	       ).


%
% scan_properties -
% 	Analyze the list of property definitions within a class
%
scan_properties(_, []).			% End of list

scan_properties(Name, [ H | T ]) :-	% Normal case
	% [H] contains an expression like 'property p1 => ...'
	% Simply execute it, and the 'property' and '=>' operators will do
	% the rest
	H,				
	scan_properties(Name, T).


/****************************************************************************
 
 	Operator 	: property Name { => [Attribute:Value, ...] }
 			  method   Name { => [Attribute:Value, ...] }
 	Purpose		:
 		Definition of a property or a method [Name]. 
 		Property attributes can be specified as a list of 
 		[Attribute]:[Value] pairs. If only one [Attribute:Value] pair
 		is specified, then the delimiting brackets can be omitted.
 		It is allowed to defined a property or a method without any
 		attribute.

 	Arguments	:
 		[Name] (i) -
 			Name of the property or method to be defined.
 		[Attribute:Value, ...] (i) -
 			List of attribute/value pairs. See the 'Comments' 
 			paragraph for a list of authorized attributes.

 	Comments	:
		An attribute is one of the following keywords :
		. type (properties only) :
			Type of the property. 'Value' can be either a standard 
			Prolog type such as atom, list, string, structure, etc. 
			or a class name.
		
		. set/get (properties only) :
			'Value' represents the predicate to use in order to set 
			or retrieve the value of a property.
 
 		. visibility (properties and methods) :
 			Visibility of the property or method. 'Value' can be
 			either :
 			. public :
 				The property or method is accessible to the
 				user of the class.
 			. protected :
 				The property or method is accessible only to
 				classes derivating from this one.
 			. private :
 				The property or method can only be used in this
 				class.
 
 ****************************************************************************/

%
% Syntax for a property definition
%
property Name :-			% Property without attributes
	property Name => [].

property Name => Something :-		% Property with attributes
	define_property(property, Name),
	define_property_attributes(property, Name, Something).

%
% Syntax for a method definition
%
method Name :-				% Method without attributes
	method Name => [].
	
method Name => Something :-		% Method with attributes
	define_property(method, Name),
	define_property_attributes(method, Name, Something).
	


/****************************************************************************
 
 	Operator 	: class write 
 	Syntax		: class write Class { on FileID }
 	Purpose		:
 		Writes the definition of [Class] to standard output, or to
 		specified [FileId] if the 'on' operator is used.
 	Arguments	:
 		[Class] (i) -
 			Name of the class whose definition is to be printed.
 		[FileId] (i) -
 			Id of the file to print in ; if no [FileId] is specified,
 			the default is current output.
 
 ****************************************************************************/
class write Class :-			% stdout version : error case : class is not defined
	not( class(Class) ),
	throw( undefined_class(Class) ).
	
class write Class :-			% stdout version : normal case
	write_class(Class).

class write Class on ID :-		% fileid version : error case : class is not defined
	not( class(Class) ),
	throw( undefined_class(Class) ).
	
class write Class on ID :-		% fileid version : normal case
	write_class(ID, Class).
	