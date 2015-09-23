% consulting this file in the listener runs the hello example
% this example shows some use of class hierarchies

% load OOPL operators & OOPL engine and create OOPL system objects
:- \+ ooplLoaded, consult( 'lib/ooplops.pro' ), consult( 'lib/oopl.pro' ).
:- oopl.

% user supplied error messaging
ooplError( String ):- w_msgbox( String ).

% we allow tracing for this example, so you can see trace the message passing
ooplTrace( String ):- write( $                  $ ), write( String ), nl.

:- oopl <- trace.

%%%% class definition : parent

parentMethod( info, Self, 'Hello parent' ).
parentMethod( hello, Self ):- 
	Self <- info @ Info,
	write( Info ),
	nl.

% now test: create the parent class object and an instance
:- oopl <- new( parent, base ).   % the base methods are inherited for this object  
:- parent <- new( myparent ).    % create instance 
:- myparent <- hello. 		 % writes 'Hello parent'
:- nl.

%%%% class definition : child

childMethod( info, Self, 'Hello child' ).

% now test: create the child class object and an instance
:- oopl <- new( child, parent ).
:- child <- new( mychild ).
:- mychild <- hello,nl. 	% writes 'Hello child'
:- mychild <- hello, fail.	% writes 'Hello child'+ 'Hello Parent' (backtracking on info attribute) 
:- nl.

%%%% class definition : grandchild

grandchildMethod( info, Self, 'Hello grandchild' ).
grandchildMethod( hello, Self ):- write( 'Overloaded hello' ),nl.

:- oopl <- new( grandchild, child ).
:- grandchild <- new( mygrandchild ).
:- mygrandchild <- hello,nl. 	% writes 'Overloaded hello'
:- mygrandchild ^- hello,nl. 	% writes 'Hello grandchild'
:- mygrandchild <- hello, fail.	% writes 'Overloaded hello' + 'Hello grandchild' (hello of parent with attribute of grandchild) + 'Hello child' (hello of parent with attribute of parent) + 'Hello parent' (hello of parent with attribute of parent)
:- nl.

% end of file

