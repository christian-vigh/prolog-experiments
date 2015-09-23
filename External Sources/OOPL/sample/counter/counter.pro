% consulting this file in the listener runs the counter example

% load OPL operators
:- consult( 'lib/ooplops.pro' ).

% user supplied error & trace messaging
ooplError( String ):- w_msgbox( String ).
ooplTrace( String ):- write( String ), nl.

%%%%%% methods of counter class : 
% this class implements a container for a value
% the value can be manipulated.
% the class remembers the maximum countervalue
 
% service for instantiation of object (no use of the base class in this example)
counterMethod( new, Self ):-
	% object is now created. However, maximum is not yet set
	Self <- sum ? Value, 
	Self <- maximum = Value.  % attribute 'maximum' is defined and filled with current value

% set counter to a value
counterMethod( set, Self, Value ):- % set value to Value
	Self <- sum = Value,
	Self <- maximum = Value.

% increase counter
counterMethod( up, Self ):- % add 1
	Self <- add @ 1.
% decrease counter
counterMethod( down, Self ):- % substract 1
	Self <- add @  -1.
% add some amount
counterMethod( add, Self, Added ):- % add a number
	number( Added ),
	Self <- sum ? Value,
	Self <- maximum ? Max,
	New is Value + Added, 
	Self <- sum = New,
	( 	( New > Max ) -> 
   		( Self <- maximum = New );
		true 
	).
% add the value of another counter
counterMethod( add, Self, Added ):- % add a number
	isA( Added, counter ),
	Added <- sum ? Sum,
	Self <- add @ Sum.
 
%%%

% create a counter and do some increasing
example1:-
	% define and create class counter. Use base class for inheriting printing service
	oopl <- new( counter, base ) @ [sum],   
	% create the object itself
	counter <-  new( my_counter ) @ [0], 
	% do some example processing
	my_counter <- up, 	% sum is 1
	my_counter <- add @ 10,	% sum is 11
	my_counter <- down, 	% sum is 10
	my_counter <- down, 	% sum is 9, maximum was 11
	my_counter <- print, nl.% show results

%%%%

% reset the counter and loop 100 times
example2:-
	isInstance( Instance, counter ),
	Instance <- set @ 0,		% reset counter
	repeat,
		once( Instance <- up ),
		Instance <- sum ? Sum,
		Sum > 100,
		Instance <- print,
		!.

% add one counter to another
example3:-
	isInstance( Instance, counter ),
	counter <- new( otherCounter ) @ [10],
	Instance <- add @ otherCounter,
	nl,
	Instance <- print,nl,
	otherCounter <- print.

% load oopl engine and create oopl system objects
main:-	
	( \+ ooplLoaded -> consult( 'lib/oopl.pro' ) ; true ),
	oopl,
	example1,
	example2,
	example3.

:-main.
