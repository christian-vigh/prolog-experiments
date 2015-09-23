% consulting this file in the listener runs the multicounter example
% this is like the counter example, but now with many counters 

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

% create a counter plus 1000 extra counters
example1:-
	% define and create class counter. Use base class for inheriting printing service
	oopl <- new( counter, base ) @ [sum],   
	counter <-  new( my_counter ) @ [0], 
	% create 1000 counters
	repeat,
		my_counter <- up,
		my_counter <- sum ? Sum,
		counter <-  new( A_counter ) @ [Sum], 
		Sum > 1000.

%%%%

% find counter with content 500 (this will not be the first in the database, to test the indexing)
% and increase this 3000 times
example2:-
	isInstance( Instance, counter ),
	Instance <- sum ? 500,		% get counter with content 500
	repeat,
		Instance <- up,
		Instance <- sum ? Sum,
		Sum > 3000.


:-load(date_time).

% load oopl engine and create oopl system objects
main:-	
	( \+ ooplLoaded -> consult( 'lib/oopl.pro' ) ; true ),
	oopl,
	time(H1,M1,S1),
	example1,!,
	write( 'all counters created' ), nl,
	example2,!,
	write( 'additions made' ), nl,
	time(H2,M2,S2 ),
	write( 'Duration: ' ), Seconds is 3600*(H2-H1)+60*(M2-M1) + (S2-S1), write( Seconds ), write( ' seconds.'), nl.

:-main.
