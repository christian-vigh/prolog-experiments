% consulting this file in the listener runs the operator example
% this example shows overloading of build in operators 
% and might give you some idea of more useful OPL code

% load OOPL operators & OOPL engine and create OOPL system objects
:- \+ ooplLoaded, consult( 'lib/ooplops.pro' ), consult( 'lib/oopl.pro' ).
:-	oopl.


% user supplied error messaging handles
ooplError( String ):- w_msgbox( String ).
ooplTrace( String ):- write( $                 $ ), write( String ),nl.

%%%% implementations of class string, a container for a string

% add something to the buffer
stringMethod( add, Self, String ):-
	string( String ),
	Self <- buffer ? Old,
	strcat( Old, String, New ),
	Self <- buffer = New.
stringMethod( add, Self, Charlist ):-
	Charlist = [ _ | _ ],
	string_list( String, Charlist ),
	Self <- add @ String.
stringMethod( add, Self, StringObject ):-
	isA( StringObject, string ),
	StringObject <- buffer ? String,
	Self <- add @ String.
stringMethod( add, Self, Atom ):-
	atom( Atom ),
	name( Atom, String ),
	Self <- add @ String.
stringMethod( write, Self ):-
	Self <- buffer ? B, 
	write( B ).

% use operator overloading of +
stringMethod( '@+', Self, String ):-
	Self <- add @ String.

example1 :- 
	% define string class and object
	oopl <- new( string, base ) @ [ buffer ],   	
	string <- new( string1 ) @ [ $S1 $ ],
	string <- new( string2 ) @ [ $S2 $ ],
	% do some example processing
	StringA '@is' string1 + $extra$,	
	StringA <- write, nl,
	StringB '@is' StringA + ' ' + string2,
	StringB <- write, nl,
	StringC '@is' StringB,
	StringC <- write, nl,
	StringD '@is' StringA + ' and ' + StringB,
	StringD <- write, nl,
	StringA <- destroy,
	StringB <- destroy,
	StringC <- destroy,
	StringD <- destroy, nl.

:- example1.	% do it

%%%%%%% an example with complex numbers

% define operator overloading of + and *
complexMethod( '@+', Self, [Real,Ima] ):-
	number( Real ), number( Ima ),!,
	Self <- real ? OldReal,
	Self <- imaginary ? OldIma,
	NewReal is Real + OldReal,
	NewIma is Ima + OldIma,
	Self <- real = NewReal,
	Self <- imaginary = NewIma.
complexMethod( '@+', Self, Object ):-
	isA( Object, complex ),
	Object <- real ? Real,
	Object <- imaginary ? Ima,
	Self <- '@+' @ [Real,Ima].
complexMethod( '@*', Self, [Real,Ima] ):-
	number( Real ), number( Ima ),!,
	Self <- real ? OldReal,
	Self <- imaginary ? OldIma,
	NewReal is (Real * OldReal) - (Ima*OldIma),
	NewIma is (Ima*OldReal) + (Real*OldIma),
	Self <- real = NewReal,
	Self <- imaginary = NewIma.
complexMethod( '@*', Self, Object ):-
	isA( Object, complex ),
	Object <- real ? Real,
	Object <- imaginary ? Ima,
	Self <- '@*' @ [Real,Ima].
complexMethod( write, Self ):-
	Self <- real ? Real,
	Self <- imaginary ? Ima,
	write( Real ), write( ' + ' ), write( Ima ), write( i ).

example2 :- 
	% define complex number class
	oopl <- new( complex, base ) @ [ real, imaginary ], 
	% do some complex number calculations
	complex <- new( a ) @ [ 1, 1 ],    % 1+i 
	complex <- new( b ) @ [ -1, 1 ],   % -1+i 
	C '@is' a + b, C <- write, nl,
	D '@is' a + [1,2], D <- write, nl,
	E '@is' (a+D) * b, E <- write, nl.

:- example2.
