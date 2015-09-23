% Meta predicates

verify( X ) :-
	verify_predicate( X ), !.
verify( _ ).


% verify_predicate : concept
verify_predicate( concept( X ) ) :-
	concept( X ),
	print 'Concept ''' + X + ''' has already been defined.' + nl,
	verify_continue.
verify_predicate( concept( _ ) ).


% verify_predicate : action
verify_predicate( action( X, Y ) ) :-
	!, action( X, Y ),
	print 'Action ''' + Y + ''' has already been defined for concept ''' + X + '''.' + nl,
	verify_continue.
verify_predicate( action( X, Y ) ) :-
	!, not( concept( X ) ),
	print 'Concept ''' + X + ''' has not been defined for action ''' + Y + '''.' + nl,
	assert( concept( X ) ),
	verify_continue.
verify_predicate( action( _, _ ) ).


% verify_predicate : property
verify_predicate( property( X, Y ) ) :-
	property( X, Y, _ ),
	print 'Property ''' + Y + ''' has already been defined for concept ''' + X + '''.' + nl,
	verify_continue.
verify_predicate( property( X, Y ) ) :-
	not( concept( X ) ),
	print 'Concept ''' + X + ''' has not been defined for property ''' + Y + '''.' + nl,
	assert( concept( X ) ),
	verify_continue.
verify_predicate( property( _, _ ) ).

% verify_predicate : attribute
verify_predicate( attribute( X, Y, _ ) ) :-
	attribute( X, Y, _ ),
	print 'Attribute ''' + Y + ''' has already been defined for concept ''' + X + '''.' + nl,
	verify_continue.
verify_predicate( attribute( X, Y, _ ) ) :-
	not( concept( X ) ),
	print 'Concept ''' + X + ''' has not been defined for attribute ''' + Y + '''.' + nl,
	assert( concept( X ) ),
	verify_continue.
verify_predicate( attribute( _, _, _ ) ).


% verify_predicate : belongs
verify_predicate( belongs(X, Y) ) :-
	link(X, Y),
	print 'Link has already been defined for concepts ''' + X + ''' and ''' + Y + '''.' + nl, 
	verify_continue.
verify_predicate( belongs(X, Y) ) :-
	link(Y, X),
	print 'Circular link defined for concepts ''' + X + ''' and ''' + Y + '''.' + nl, 
	verify_continue.
verify_predicate( belongs(X, Y) ) :-
	not( concept(X) ), not( concept(Y) ),
	print 'Relation ''belongs'' is linking two undefined concepts : ''' + X +
			''' and ''' + Y + '''.' + nl,
	assert( concept( X ) ),
	assert( concept( Y ) ),
	verify_continue.
verify_predicate( belongs(X, Y) ) :-
	not( concept(X) ), 
	print 'Relation ''belongs'' is linking the concept ''' + Y + ''' to an undefined concept ''' +
			X + '''.' + nl,
	assert( concept( X ) ),
	verify_continue.
verify_predicate( belongs(X, Y) ) :-
	not( concept(Y) ), 
	print 'Relation ''belongs'' is linking an undefined concept ''' + Y + 
			''' to the concept ''' + X + '''.' + nl,
	assert( concept( Y ) ),
	verify_continue.

verify_predicate( belongs( _, _ ) ).
	

% verify_predicate : undefined fact
verify_predicate( Other ) :-
	print 'Fact ' + Other + ' is not a valid database fact.' + nl,
	verify_continue.

	
verify_continue :-
	!, print 'Continue ? ',
	respkey(Char),
	char_code(Code, Char),
	verify_throw(Code), !.
	
verify_throw(Code) :-
	Code \== 'y',
	throw( abort('Database processing aborted.') ).
verify_throw(_).


% Succeeds if there is a direct or indirect belongs link between X and Y
link(X, Y) :-
	belongs(X, Y).
link(X, Y) :-
	belongs(X, Temp),
	link(Temp, Y).


% Finds which entity can do the specified action
find(Predicate, Entity, Action) :-
	Goal =.. [Predicate, Entity, Action],
	call(Goal).
find(Predicate, Entity, Action) :-
	link(X, Entity),
	Goal =.. [Predicate, X, Action],
	call(Goal).
	
find(Predicate, Entity, Action, Result) :-
	Goal =.. [Predicate, Entity, Action, Result],
	call(Goal).
find(Predicate, Entity, Action, Result) :-
	link(X, Entity),
	Goal =.. [Predicate, X, Action, Result],
	call(Goal).
	