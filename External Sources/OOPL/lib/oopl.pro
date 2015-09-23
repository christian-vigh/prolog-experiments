% OOPL KERNEL         
% Amzi! 7 edition
% OOPL: an Prolog library for Object Oriented Programming with Logic 
% Copyright (C) 
% This program is free software; you can redistribute it and/or modify it under the terms of 
% the GNU General Public License as published by the Free Software Foundation; either version 2 
% of the License, or (at your option) any later version.
% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without 
% even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
% GNU General Public License for more details.
% You should have received a copy of the GNU General Public License along with this program; 
% if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
% You can contact the author H.J. Borgers at borgers@bart.nl or at Salomeschouw 117, Zoetermeer, Holland

ooplVersion( 'OOPL (c) kernel, Amzi! Prolog 7 edition, version 3.1', 310 ).
ooplRights( 'OOPL is running using the GNU General Public License' ).

%%%%%%%%%%%%%%%%%%%%%%%% ERROR & TRACING %%%%%%%%%%%%%%%%%%%%%%%%

% user must define ooplTrace in order to trace
% :- import ooplTrace/1.
oopl_trace( List ):-
	oopl_listToString( List, String ), 
	ooplTrace( String ),!.
oopl_trace( List ).

oopl_error( List ):- 
	oopl_listToString( [ 'OPL ERROR :' | List], String ), 
	ooplError( String ).
oopl_error( List ).

%%%%%%%%%%%%%%%%%%%%%%%% CREATION OF OBJECTS %%%%%%%%%%%%%%%%%%%%%%%%

% old syntax : newInstance( +Name, +Class( ... Initial_Values ... )). 
newInstance( Instance, Description ):-
	Description =.. [ Class | Args ],
	oopl_new( Instance, Class, Args ).

oopl_new( Instance, Class, Args ):-
	oopl_buildObject( Instance, Class, Args ),
	(TheClass = Class ; isParentclass( Class, TheClass )), % use the parent class new on backtracking
	isAttribute( instance_method, TheClass, InstanceMethod ),
	Call =.. [ InstanceMethod, new, Instance ], % no need to send description, already in attributes
	(( call( Call ) ; true )), % new might not be defined for this class
	!.
oopl_new( Instance, Class, Args ):- !, oopl_error( [ 'failed to create instance of class ', Class ] ), fail.

% destroyInstance( +Instance ). 
destroyInstance( Instance ):-
	\+ isClass( Instance ),	% you cannot destrou classes ! 
	( send( Instance, destroy ) ; true ),!,
	oopl_freeObject( Instance ).
destroyInstance( Instance ):- !, oopl_error( [ 'cannot destroy ', Instance ] ), fail.

copyInstance( Object, Original ):-
	% create unique id 
	isObject( Original ),
	oopl_ground_object_name( Object ),	
	% deep copy : copy all attributes first
	oopl_forall( oopl_mem_pair( A, Original, V ), create_oopl_mem_pair( A, Object, V ) ),
	% modify the self attribute
	setAttribute( self, Object, Object ),
	% add it to the class
	isAttribute( class, Original, Class ),
	addAttribute( instance, Class, Object ).

% oopl_buildObject( +Object, +Class, +[ Args ] ).
oopl_buildObject( Object, Class, Value_list ):-
	isAttribute( template, Class, Template ),
	Template =.. [ Class | Attribute_list ],
	( oopl_same_list_length( Value_list, Attribute_list )->  
          once( oopl_ground_object_name( Object ) ),
     	  \+ isObject( Object ),
     	  oopl_valid_object_name( Object ),
     	  setAttribute( self, Object, Object ),
     	  setAttribute( class, Object, Class ),
     	  addAttribute( instance, Class, Object ),
     	  oopl_add_methods( Object ),
	  oopl_fill_template( Object, Attribute_list, Value_list )
	  ;
	  oopl_error( [ 'mismatch on number of attributes when creating object ', Object, ' of class ', Class ] ),
	  fail
	),!.

%% low level 

oopl_add_methods( Object ):-
	oopl_forall( 
		( isA( Object, Class ), isAttribute( instance_method, Class, Method )),
		( isAttribute( method, Object, Method ) ; addAttribute( method, Object, Method )) % multiple inheritance could otherwise have same methods duplicated
	).

oopl_fill_template( Object, [ A | T1 ], [ V | T2 ] ):-
     addAttribute( A, Object, V ),
     oopl_fill_template( Object, T1, T2 ).
oopl_fill_template( Object, [], [] ).

% oopl_freeObject( +Object ).
oopl_freeObject( Object ):-
     oopl_grounded( Object ),
     isAttribute( class, Object, Class ),  
     isAttribute( template, Class, Template ),
     Template =.. [ Class | Attributes ],
     clearAttribute( _Attributes, Object, _Values ),	% destroy all attributes
     clearAttribute( instance, Class, Object ),!.

% mutate( +Instance, +NewClass )
mutateInstance( Instance, Class ):-
	isInstance( Instance ),
	isClass( Class ),
	modifyAttribute( class, Instance, Oldclass, Class ),
	eraseAttribute( instance, Oldclass, Instance ),
	addAttribute( instance, Class, Instance ),
	clearAttribute( method, Instance, _ ),
	oopl_add_methods( Instance ).

%%%%%%%%%%%%%%%%%%%%%%%% MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% send/2

% send( +Object, +Service ). 
send( Object, Service ):-
	oopl_valid_object_name( Object ),
 	('{sys}oopl_trace'->oopl_trace( [ Object, ' <- ', Service ] );true),
	isAttribute( method, Object, Method ),
 	('{sys}oopl_fulltrace'->oopl_trace( [ Method, '( ', Object, ', ', Service, ')' ] ) ;true),
	Call =.. [Method, Service, Object ],
	call( Call ).

% message to all objects on list. 
send( [L|Tail], S ):- \+ var( L ), send( L, S ), !, send( Tail, S ).
send( [], _ ).

% error info 
send( Object, S ):- 
	( var(Object) ; ( \+ isObject(Object) )) ->
	oopl_error( [ 'invalid object in OPL call : ', Object, ' <- ', S ] ),
	fail;
	fail. 

% send/3

% send( +Object, +Service, ?Argument ). 
send( Object, Service, Argument ):-
	oopl_valid_object_name( Object ),
 	('{sys}oopl_trace'->oopl_trace( [ Object, ' <- ', Service, ' @ ', Argument ] );true),
	isAttribute( method, Object, Method ),
 	('{sys}oopl_fulltrace'-> oopl_trace( [ Method, '( ', Object, ', ', Service, ', ', Argument, ' )' ] );true),
	Call =.. [Method, Service, Object, Argument ],
	call( Call ).

send( [L|Tail], S, M ):- \+ var( L ), send( L, S, M ), !, send( Tail, S, M ).
send( [], _, _ ).

send( Object, S, M ):-
	 ( var(Object); ( \+ isObject(Object) ))->
	 oopl_error( [ 'invalid object in OPL call : ', Object, ' <- ', S, ' @ ', M ] ),
	 fail;
	 fail.

% see also oopl_delegation 

% only invoke superclass methods
% sendUp( +Object, +Service ). 
sendUp( Object, Service ):-
	oopl_valid_object_name( Object ),
 	('{sys}oopl_trace'->oopl_trace( [ Object, ' ^- ', Service ] );true),
	isAttribute( class, Object, Class ),
	isAttribute( instance_method, Class, Instance_method ),
	isAttribute( method, Object, Method ),
	\+ Method = Instance_method, % do not use local method
 	('{sys}oopl_fulltrace'->oopl_trace( [ Method, '( ', Object, ', ', Service, ' )' ] );true),
	Call =.. [Method, Service, Object ],
	call( Call ).

% trace info 
sendUp( Object, S ):- 
	( var(Object); (\+ isObject(Object)) ) ->
	oopl_error( ['invalid object in OPL call : ', Object, ' ^- ', S ] ),
	fail;fail. 

% sendUp( +Object, +Service, ?Argument ). 
sendUp( Object, Service, Argument ):-
	oopl_valid_object_name( Object ),
 	('{sys}oopl_trace'->oopl_trace( [ Object, ' ^- ', Service, ' @ ', Argument ] );true),
	isAttribute( class, Object, Class ),
	isAttribute( instance_method, Class, Instance_method ),
	isAttribute( method, Object, Method ),
	\+ Method = Instance_method, % do not use local method
 	('{sys}oopl_fulltrace'-> oopl_trace( [ Method, '( ', Object, ', ', Service, ', ', Argument, ' )' ] );true),
	Call =.. [Method, Service, Object, Argument ],
	call(Call).

sendUp( Object, S, M ):-
	( var(Object); (\+ isObject(Object)) )->
	oopl_error( [ 'invalid object in OPL call : ', Object, ' ^- ', S, ' @ ', M ] ),
	fail;fail.

%%%%%%%%%%%%%%%%%%%%%% MEMORY MANAGEMENT %%%%%%%%%%%%%%%%%%%%%%%%

oopl_newobjectname( O, Name ):- \+ var( O ), oopl_gensym( ${sys}oopl_db_$, Name ).
oopl_valid_object_name( Name ):- var( Name )-> oopl_error( [ 'message send to VAR instead of object' ] );true.

% two approaches

/*

% implementation using a hash tabel for attribute names. 
% Nowadays, Amzi has indexing, which is much faster.
% still here for someone to use this indexing with another Prolog

oopl_mem_pair( A, O, V ):-
    '{sys}oopl_internal_objectname'( A, Name ),             % decode name : must decode since slot MAY contain clause (might be better to forbid this)
    Call =.. [ Name, O, V ],
    call( Call ).
create_oopl_mem_pair( A, O, V ):-
    ( '{sys}oopl_internal_objectname'( A, Name ) ; 
      (
      oopl_newobjectname( A, Name ),                 % get a new code
      assertz( '{sys}oopl_internal_objectname'( A, Name ) ),
      true	
      )
    ),
    Call =.. [ Name, O, V ],
    assert( Call ). 
delete_oopl_mem_pair( A, O, V ):-
    '{sys}oopl_internal_objectname'( A, Name ),
    Call =.. [ Name, O, V ],
    retract( Call ).                 
deleteall_oopl_mem_pair:-
    oopl_forall( '{sys}oopl_internal_objectname'( _, Name ),
            abolish( Name/2 )                   % Name/2  destroyed
    ),
    abolish( '{sys}oopl_internal_objectname'/2 ).
init_oopl_mem_pair:- true.

             
*/

% implementation storing clauses with Amzi supplied index. 

% We assume Object is most of the time known, so index on this
:- indexed '{sys}oopl_db'(1,0,0).

oopl_mem_pair( A, O, V ):-
	'{sys}oopl_db'( O, A, V ).
create_oopl_mem_pair( A, O, V ):-
	assertz( '{sys}oopl_db'( O, A, V ) ).
delete_oopl_mem_pair( A, O, V ):-
	retract( '{sys}oopl_db'( O, A, V ) ).
deleteall_oopl_mem_pair:-
   abolish( '{sys}oopl_db'/3 ).
init_oopl_mem_pair:- true.

              

%%%%%%%%%%%%%%%%%%%%%% ATTRIBUTES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% isAttribute( ?Attribute, +?Object, ?Value ). 
isAttribute( A, O, V ):-
    oopl_mem_pair( A, O, V ).

% areAttributes( ?Attribute, +Object, -List ).
areAttributes( Attribute, Object, List ):-
     setof( Value, isAttribute( Attribute, Object, Value ), List ),!.
 
% setAttribute( ?Attribute, +Object, +Value ).
setAttribute( Attribute, Object, Value ):-
     (clearAttribute( Attribute, Object, _ );true),
     addAttribute( Attribute, Object, Value ),!.

% modifyAttribute( +Attribute, +Object, -Oldvalue, +NewValue ).
modifyAttribute( Attribute, Object, Old, New ):-
     delete_oopl_mem_pair( Attribute, Object, Old ),
     create_oopl_mem_pair( Attribute, Object, New ),!.

% addAttribute( +Attribute, +Object, +Value ).
addAttribute( Attribute, Object, Value ):-
     create_oopl_mem_pair( Attribute, Object, Value ),!.     

% eraseAttribute( ?A, ?O, ?V )
eraseAttribute( Attribute, Object, Value ):-
     delete_oopl_mem_pair( Attribute, Object, Value ),!.

% clearAttribute( ?Attribute, ?Object, ?Value ).
clearAttribute( Attribute, Object, Value ):-
     oopl_forall( isAttribute( Attribute, Object, Value ), delete_oopl_mem_pair( Attribute, Object, Value ) ),!.

%%%%%%%%%%%%%%%%%%%%%%%% BASE CLASS METHOD %%%%%%%%%%%%%%%%%%%%%%%%%%

% base class methods 

baseMethod( print, Self ):-
     oopl_forall( 
            isAttribute( Var, Self, Val ),
            (
               write( Var ), 
               write( ' = ' ), 
               write( Val ), 
               nl
            )
     ). 

% oopl_delegation
baseMethod( Service, Self ):-
     oopl_delegation( Service, Self ).

% baseMethod/3
baseMethod( list, Self, Result ):-
     setof( Slot, Attr^Value^ 
            ( isAttribute( Attr, Self, Value ), 
              Slot =.. [ Attr, Value ] 
            ),
            Result ).

% oopl_delegation 
baseMethod( Service, Self, Argument ):-
     oopl_delegation( Service, Self, Argument ).

%%%%%%%%%%%%%%%%%%%%%%%%%%% CLASS METHOD %%%%%%%%%%%%%%%%%%%

% implements class <- new( Object ) + [ args ] and baseMethod for class objects

% classMethod/2
classMethod( new( Object ), Self ):-
	classMethod( new( Object ), Self, [] ),!.

classMethod( Service, Self ):- baseMethod( Service, Self ).

% classMethod/3
classMethod( new( Object ), Self, ArgumentList ):-
	oopl_new( Object, Self, ArgumentList ),!.

classMethod( Service, Self, Argument ):- baseMethod( Service, Self, Argument ).

%%%%%%%%%%%%%%%%%%%%%%%% CREATION OF CLASSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% instance method == 'classnameMethod'
oopl_instance_method( Prefix, Name ):-
	atom( Prefix ),
	string_atom( Prefix_string, Prefix ),
	strcat( Prefix_string, $Method$, String ), 
	string_atom( String, Name ).   
 
% newClass( +Class( ... Initial_Attributes ...) ). 
newClass( Template ):-
	Template =.. [ Class | _ ],
	oopl_instance_method( Class, InstanceMethod ),
	newClass( Template, InstanceMethod ).

% newClass( +Class( ... Initial_Attributes ...), +InstanceMethod ). 
newClass( Template, InstanceMethod ):-
     Template =.. [ Class | _ ],
     newInstance( Class, oopl( Template, InstanceMethod ) ),!. % class is in fact instance of the oopl meta object 

% subclass inherits data and methods
newSubclass( Template, Parent ):-
     Template =.. [ Class | Special_slots ], 
     oopl_instance_method( Class, Method ),
     newSubclass( Template, Parent, Method ).

% single parent
% newSubclass( +Class( ... Special_Slots ...), +Parent, +Method ).
% init with  newInstance( Instance, Class( ... Parent_Slots, Special_Slots ) ). 
newSubclass( Template, Parent, Method ):-
     Template =.. [ Class | Special_slots ], 
     oopl_parent_attributes( [ Parent ], Parent_slots ),
     oopl_concat( Parent_slots, Special_slots, Total_slots ),
     Total_template =.. [ Class | Total_slots ],
     newClass( Total_template, Method ),
     oopl_add_supers( [ Parent ], Class ),!.

% multiple subclass inherits methods and attributes of all parents
% newSubclass( +Class( ... Initial_Slots ...), +Parent_List, +Method ). 
newSubclass( Template, [ Parent | Tail ], Method ):-
     Template =.. [ Class | Special_slots ],
     oopl_parent_attributes( [ Parent | Tail ], Parent_slots ),
     oopl_concat( Parent_slots, Special_slots, Total_slots ),
     Total_template =.. [ Class | Total_slots ],
     newClass( Total_template, Method ),
     oopl_add_supers( [ Parent | Tail ], Class), !. 

newSubclass( T, P, M ):- !, oopl_error( [ 'cannot create subclass with ', newSubclass( T, P, M ) ] ),  fail.  

%% ll
oopl_add_supers( [Super|Tail], Class ):-
	addAttribute( parent, Class, Super ),
	oopl_add_supers( Tail, Class ).
oopl_add_supers( [], Class ). 

oopl_parent_attributes( [ Parent | Tail ], Slots ):-
    isAttribute( template, Parent, Parent_template ),  
    Parent_template =.. [ _ | Parent_slots ],
    oopl_parent_attributes( Tail, Rest ),
    oopl_concat( Parent_slots, Rest, Slots ).
oopl_parent_attributes( [ ], [ ] ).

%%%%%%%%%%%%%%%%%%%%% OPL META CLASS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init oopl kernel

oopl:-
     ooplVersion( Version, Nr ),
     nl, write( Version ), nl,
     ooplRights( Rights ),
     % DELETE THIS LINE IN A COMMERCIAL EDITION
     write( Rights ),nl,nl,
     % w_tfmsgbox( Rights ),
     % continue after agreeing
     oopl_load_operators,	
     init_oopl_mem_pair,     
     deleteall_oopl_mem_pair, 
     init_oopl_mem_pair,      
     abolish( '{sys}oopl_lastsym'/1 ),
     assert( '{sys}oopl_lastsym'( 1 ) ), 
     addAttribute( self, 'oopl', 'oopl' ),
     addAttribute( class, 'oopl', 'oopl' ),
     addAttribute( method, 'oopl', ooplMethod ),
     addAttribute( instance_method, 'oopl', classMethod ), % so classes (instances of OPL use classMethod)
     addAttribute( template, 'oopl', 'oopl'( template, instance_method ) ),
     send( 'oopl', init ),
     newClass( base ),!.
oopl:-
     oopl_error( [ 'OPL initialisation failed' ] ),fail.
 
ooplMethod( init, Self ):-
     true,!.
ooplMethod( destroy, Self ):-
     deleteall_oopl_mem_pair,!.                
ooplMethod( clear, Self ):- 
     deleteall_oopl_mem_pair,!.                
ooplMethod( notrace, Self ):- retractall( '{sys}oopl_trace' ),retractall( '{sys}oopl_fulltrace' ).
ooplMethod( trace, Self ):- ooplMethod( notrace, Self ), assert( '{sys}oopl_trace' ).
ooplMethod( fulltrace, Self ):- ooplMethod( trace, Self ), assert( '{sys}oopl_fulltrace' ).
ooplMethod( new( Class ), Self ):- ooplMethod( new( Class ), Self, [] ).
ooplMethod( new( Class, Parent ), Self ):- ooplMethod( new( Class, Parent ), Self, [] ).
ooplMethod( A, B ):- baseMethod( A, B ),!.
ooplMethod( gennum, Self, Nr ):-	% generate unique number
     oopl_gennum( Nr ).

ooplMethod( new( Class ), Self, Slotlist ):-
	Template =.. [ Class | Slotlist ],
	newClass( Template ).
ooplMethod( new( Class, Parent ), Self, ExtraSlots ):- % parent may also be list of parents
     	Template =.. [ Class | ExtraSlots ], 
	newSubclass( Template, Parent ).

ooplMethod( saveState, Self, FileName ):- % save all object to file
  	tell(FileName),
	oopl_forall( oopl_mem_pair( A, O, V ), ( writeq( saved_oopl_mem_pair( A, O, V ) ), write( '.' ), nl ) ),
  	told.

ooplMethod( loadState, Self, FileName ):- % restore all object from file (current state is whiped)
	deleteall_oopl_mem_pair,
	consult( FileName ),
	oopl_forall( saved_oopl_mem_pair( A, O, V ), create_oopl_mem_pair( A, O, V ) ),
	abolish( saved_oopl_mem_pair/3 ).

ooplMethod( A, B, C ):- baseMethod( A, B, C ),!.

oopl_gennum( Last ):-
     retract( '{sys}oopl_lastsym'( Last ) ),
     Next is Last + 1,
     assert( '{sys}oopl_lastsym'( Next ) ),!.

oopl_gensym( Letter, Symbol ):-    
     oopl_gennum( Nr ),
     string_integer( StringNr, Nr ),
     strcat( Letter, StringNr, String ),
     string_atom( String, Symbol ).   % generate something likes '@123'  

%%%%%%%%%%%%%%%%%%%%%% DELEGATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% uses the delegation attribute to delegate messages to other objects

% oopl_delegation( Service, Object ).
oopl_delegation( Service, Object ):-
     oopl_valid_object_name( Object ),
     isAttribute( delegation, Object, Delegation_object ),
     send( Delegation_object, Service ). 
     
% oopl_delegation( Service, Object, Argument ).
oopl_delegation( Service, Object, Argument ):-
     oopl_valid_object_name( Object ),
     isAttribute( delegation, Object, Delegation_object ),
     send( Delegation_object, Service, Argument ). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HELP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isClass( Class ):-
     isAttribute( instance, 'oopl', Class );Class='oopl'. 

isParentclass( Class, Super ):-
     isAttribute( parent, Class, Super ).
isParentclass( Class, Super ):-
     isAttribute( parent, Class, Sub ),
     isParentclass( Sub, Super ).

% improved search 2.0
isA( Object, Class ):-
     isInstance( Object, Class ).
isA( Object, Class ):-
	var( Object )-> % optimised search 
     isParentclass( Cl, Class ),
     isInstance( Object, Cl );
     isInstance( Object, Cl ),
     isParentclass( Cl, Class ).

% isInstance( ?Name )
isInstance( Name ):-
     isAttribute( self, Name, Name ),
     \+ isClass( Name ). 
% isInstance( ?Name, ?Class ). 
isInstance( Name, Class ):-
     isAttribute( class, Name, Class ).

% isObject( ?Name )
isObject( Name ):-
     isAttribute( self, Name, Name ). 

%%%%%%%%%%%%%%% low level utils  %%%%%%%%%%%%%%%%%%%%%%%

% local implementation of forall
oopl_forall(X,Y):-call(X),once(Y),fail.
oopl_forall(_,_):-true.

% local implementation of on
oopl_on( Item, [Item|_] ).
oopl_on( Item, [ _|Tail ] ) :- oopl_on(Item,Tail).

oopl_concat( [X|Tail], L2, [X|List] ):- oopl_concat( Tail, L2, List ).
oopl_concat( [], L2, L2 ). 

oopl_same_list_length( [_|X], [_|Y] ):- oopl_same_list_length(X,Y).
oopl_same_list_length( [], [] ).

% succeeded when nonVar or grounds it
oopl_ground_object_name( Name ):-
     oopl_grounded( Name ),!.
oopl_ground_object_name( Var ):-
     var( Var ),  
     repeat,
	oopl_gensym( $@$, Var ),
     	\+ isObject( Var ).

oopl_grounded( X ):-
     \+ var( X ).  	 

oopl_listToString( [ A | Tail ], String ):-
	string_term( S1, A ),
	oopl_listToString( Tail, S2 ),
	strcat( S1, S2, String ),!.
oopl_listToString( [ ], $$ ).

%%%%%%%%%%%%%%%% operator definition

% also see ooplops.pro
oopl_clear_operators :-
	op( 0, xfx, '@is' ),
	op( 0, xfx, '<-' ), 
	op( 0, xfx, '^-' ), 
	op( 0, xfx, '?' ), 
	op( 0, xfx, '??' ),
	op( 0, xfx, '--' ),
	op( 0, xfx, '=' ),
	op( 0, xfx, '@' ).    
oopl_load_operators :- 
	oopl_clear_operators,
 	op( 700, xfx, '@is' ),
	op( 700, xfx, '<-' ),
	op( 700, xfx, '^-' ),
	op( 500, xfx, '?' ),
	op( 500, xfx, '??' ),
	op( 500, xfx, '--' ),
	op( 500, xfx, '=' ),
	op( 500, xfx, '@' ).


% in case this file will be consulted, the ooplops.pro must be consulted before this point (otherwise we get syntax errors)
% this does not seem to work! Compiler ignores this
% ?- oopl_load_operators. % in case this file will be compiled, this loads the operators.
% ?- write( hello), nl, consult( 'lib\ooplops.pro' ).

Object <- Service @ Argument 	:- !,send( Object, Service, Argument ).
Object <- Attribute ? Value 	:- !,isAttribute( Attribute, Object, Value ).
Object <- Attribute ?? List 	:- !,areAttributes( Attribute, Object, List ).
Object <- Attribute = Value 	:- !,setAttribute( Attribute, Object, Value ).
Object <- Attribute + Value 	:- !,addAttribute( Attribute, Object, Value ).
Object <- Attribute - Value 	:- !,eraseAttribute( Attribute, Object, Value ).
Object <- Attribute -- Value 	:- !,clearAttribute( Attribute, Object, Value ).

Object <- destroy  		:- not list( Object ), !, destroyInstance( Object ). 
List   <- destroy		:- list( List ), oopl_forall( oopl_on( Object, List ), destroyInstance( Object ) ).

Object <- Service  		:- !,send( Object, Service ). % must be last, otherwise will match wrong !

Object ^- Service @ Argument 	:- !,sendUp( Object, Service, Argument ).
Object ^- Service  		:- !,sendUp( Object, Service ).

% operator overloading

% make copy of Object, with new name
Object '@is' Original   :- copyInstance( Object, Original ).

Object '@is' A + B 	:- oopl_compound(B, Result), !, Object '@is' A+Result, Result <- destroy, !.
Object '@is' A + B 	:- oopl_compound(A, Result), !, Object '@is' Result+B, Result <- destroy, !.
Object '@is' A + B 	:- Object '@is' A, Object <- '@+' @ B, !.
Object '@is' A - B 	:- oopl_compound(B, Result), !, Object '@is' A-Result, Result <- destroy, !.
Object '@is' A - B 	:- oopl_compound(A, Result), !, Object '@is' Result-B, Result <- destroy, !.
Object '@is' A - B 	:- Object '@is' A, Object <- '@-' @ B, !.
Object '@is' A * B 	:- oopl_compound(B, Result), !, Object '@is' A*Result, Result <- destroy, !.
Object '@is' A * B 	:- oopl_compound(A, Result), !, Object '@is' Result*B, Result <- destroy, !.
Object '@is' A * B 	:- Object '@is' A, Object <- '@*' @ B, !.
Object '@is' A / B 	:- oopl_compound(B, Result), !, Object '@is' A/Result, Result <- destroy, !.
Object '@is' A / B 	:- oopl_compound(A, Result), !, Object '@is' Result/B, Result <- destroy, !.
Object '@is' A / B 	:- Object '@is' A, Object <- '@/' @ B, !.

oopl_compound( X+Y, Result ) :- Result '@is' X+Y.
oopl_compound( X-Y, Result ) :- Result '@is' X-Y.
oopl_compound( X*Y, Result ) :- Result '@is' X*Y.
oopl_compound( X/Y, Result ) :- Result '@is' X/Y.


%%%%% alternative syntax

attribute( Object, Attribute, Value ) 	:- isAttribute( Attribute, Object, Value ).
attributes( Object, Attribute, List ) 	:- areAttributes( Attribute, Object, List ).
set( Object, Attribute, Value )	 	:- setAttribute( Attribute, Object, Value ).
add( Object, Attribute, Value )  	:- addAttribute( Attribute, Object, Value ).
erase( Object, Attribute, Value )	:- eraseAttribute( Attribute, Object, Value ).
clear( Object, Attribute, Value )	:- clearAttribute( Attribute, Object, Value ).

ooplLoaded.



