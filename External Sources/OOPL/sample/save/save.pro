% consulting this file in the listener runs the example
% this example shows the saving and reloading of states

% load OOPL operators & OOPL engine and create OOPL system objects
:- \+ ooplLoaded, consult( 'lib/ooplops.pro' ), consult( 'lib/oopl.pro' ).
:- oopl.

% user supplied error messaging
ooplError( String ):- write( 'ooplError was called with arg: '), write( String ), nl.

% we allow tracing for this example, so you can see trace the message passing
ooplTrace( String ):- write( $                  $ ), write( String ), nl.

:- oopl <- trace.

%%%% class definition : hello

helloMethod( info, Self, 'Hello World' ).
helloMethod( hello, Self ):- 
	Self <- info @ Info,
	write( Info ),
	nl.

% create the parent class object and an instance
:- oopl <- new( hello, base ).   % the base methods are inherited for this object  
:- hello <- new( helloObject ).    % create instance 
:- helloObject <- hello. 		 % writes 'Hello World'
:- nl.

:- oopl <- saveState @ 'sample/save/savestate.pro'.
:- oopl.	% all old objects are deleted now
:- helloObject <- hello. % this should create an error message!

:- oopl <- loadState @ 'sample/save/savestate.pro'.
:- helloObject <- hello. % and the objects are back!	

% end of file

