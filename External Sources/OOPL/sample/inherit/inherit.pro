% consulting this file in the listener runs the multiple inheritance example
% this example shows some use of complex class hierarchies

% load OOPL operators & OOPL engine and create OOPL system objects
:- \+ ooplLoaded, consult( 'lib/ooplops.pro' ), consult( 'lib/oopl.pro' ).
:-	oopl.

% user supplied error messaging handle
ooplError( String ):- w_msgbox( String ).

%%%% class definition : parent 1 and 2

% parent1 has an attribute info1 and a service hello1 printing this attribute
:- oopl <- new( parent1, base ) @ [ info1 ].
parent1Method( hello1, Self ):- 
	Self <- info1 ? Info,
	nl,write( Info ),
	nl.

% parent2 has an attribute info2 and a service hello2 printing this attribute
:- oopl <- new( parent2, base ) @ [ info2 ].
parent2Method( hello2, Self ):- 
	Self <- info2 ? Info,
	write( Info ),
	nl.

% the child class inherits info1 and info2 attributes and hello1 and hello2 services.
:- oopl <- new(  child, [parent1, parent2] ) @ [info3].
childMethod( hello3, Self ):- 
	Self <- info3 ? Info,
	write( Info ),
	nl.

% now test if indeed all these services work
:- child <- new( mychild ) @ [ 'info content 1', 'info content 2', 'info content 3' ].
:- mychild <- hello1, mychild <- hello2, mychild <- hello3. 

