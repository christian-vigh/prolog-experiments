%******************************************************************************
%*
%* Module 	: relations.pro
%* Description   :
%*	Predicates for handling and declaring entity/relationship models.
%* Author	: Christian Vigh.
%* Date		: 2007/01/24
%* Version	: 1.0
%*	Base version.
%*
%******************************************************************************


:- module(relations).

:- 	export('..'/2).
:-	export(relation/4).
:-	export(islinked/2).
:-	export(entity/1).

:- end_module(relations).
.

:- body(relations).

%------------------------------------------------------------------------------
%
% Predicate 	: x..y
% Description	: Defines a cardinality, or subset. 'x' is the lower value of
%		  the cardinality, and 'y' is the upper value.
%
%------------------------------------------------------------------------------

:- op(500, xfx, ..).

..(0,1).
..(0,n).
..(1,1).
..(1,n).

..(X,Y) :-
	number(X), number(Y),
	X =< Y.


%------------------------------------------------------------------------------
%
% Predicate	: relation(From, Card1, Card2, To) :
% Description	: Defines a relation between entities 'From' and 'To'. 
%		  'From' and 'To' must have been created using the dbentity
%		  predicate. 'Card1' and 'Card2' are the cardinalities of each
%		  end of the relation.
%
%------------------------------------------------------------------------------
relation(From, X1..Y1, X2..Y2, To) :-
 	entity(From), entity(To),
 	assert( dbrelation( From, To, X1, Y1, X2, Y2 ) ).
 	
 
 
%------------------------------------------------------------------------------
%
% Predicate	: islinked(A,B)
% Description	: Succeeds if there is any direct or indirect relation between 
%		  entities 'A' and 'B'.
%
%------------------------------------------------------------------------------
islinked(A,B) :-			% Just for checking that A & B are entities
	entity(A), entity(B),
	islinked_x(A,B).
	
islinked_x(A,B) :-			% Direct relation
 	relation(A, _, _, B).

islinked_x(A,B) :-			% Indirect relation
	relation(A, _, _, X),
	relation(X, _, _, B).
	


%------------------------------------------------------------------------------
%
% Predicate	: entity(X)
% Description	: Succeeds if 'X' is an entity created by the declare_entity
%		  predicate.
%
%------------------------------------------------------------------------------
entity(X) :-
	dbentity(X, _).
 

declare_entity(Name, Description, Fields) :- 
	assertz( dbentity(Name, Description) ),
	declare_entity_fields(Name, Fields).
	
declare_entity_fields(Name, []).
declare_entity_fields(Name, [Field | Tail]) :-
	declare_entity_field(Name, Field),
	declare_entity_fields(Name, Tail).
	
declare_entity_field(Name, [FieldName, FieldType, DefaultValue]) :-
	field_type(FieldType, X),
	allowed_value(FieldType, DefaultValue),
	assertz( dbentityfield( Name, FieldName, X, DefaultValue) ).
	


 
 :- end_body(relations). 