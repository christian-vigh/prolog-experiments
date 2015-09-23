portray_clauses( [] ).
portray_clauses( [H|T] ) :-
	portray_clause( H ),
	portray_clauses( T ).
	
