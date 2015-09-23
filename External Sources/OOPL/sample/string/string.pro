% consulting this file in the listener runs the string example
% this example shows some more overloading of operators 
% and might give you some idea of more useful OPL code

% load OOPL operators & OOPL engine and create OOPL system objects
:- \+ ooplLoaded, consult( 'lib/ooplops.pro' ), consult( 'lib/oopl.pro' ).
:-	oopl, nl.

% user supplied error messaging
ooplError( String ):- w_msgbox( String ).

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
stringMethod( add, Self, Atom ):-
	atom( Atom ),
	name( Atom, String ),
	Self <- add @ String.

% clear the buffer
stringMethod( clear, Self ):-
	Self <- buffer = $$.

example1:-
	% define string class and object
	oopl <- new( string, base ) @ [ buffer ],   % use base methods for new, print etc.
	string <- new( mystring ) @ [ $contents: $ ], 
	% do some example processing
	mystring <- add @ $first part$,	
	mystring <- add @ ' & second part',
	mystring <- add @ " & third part".

:- example1.						% do it
:- isInstance( Object, string ), Object <- print.	% show results

%%%% class capital as a subclass of string, converting the string to capitals

% reimplement new, because otherwise we may initiate object with lower case strings
capitalMethod( new, Self ):-
	% attribute buffer must still be converted to uppercase
	Self <- buffer ? Old,
	Self <- clear,
	Self <- add @ Old.

% the only service we have to redefine is adding a string !
capitalMethod( add, Self, String ):-
	string( String ),
	string_list( String, LowerList ),
	lower_to_upper( LowerList, UpperList ),
	string_list( Upper, UpperList ),
	% directly invoke the string method for adding string
	Self ^- add @ Upper.

lower_to_upper( [ Ch | Tail ], [ UCh | Rest ] ):- upper_case( Ch, UCh ), lower_to_upper( Tail, Rest ).
lower_to_upper( [], [] ).
upper_case( Ch, UCh ):- Ch >96, Ch < 123, !, UCh is Ch - ( 97-65 ).
upper_case( Ch, Ch ).

example2:-
	oopl <- new( capital, string ),   
	capital <- new( mycapital ) @ [ 'contents: ' ], 
	% do some example processing
	mycapital <- add @ "first part in upper",	
	mycapital ^- add @ $, but second part in lower$.

:- example2.
:- isInstance( Object, capital ), Object <- print.	% show results

