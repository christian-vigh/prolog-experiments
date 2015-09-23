kv_decompose( [], [], [] ).
kv_decompose( [K-V|T], [K|Tk], [V|Tv] ) :-
	kv_decompose( T, Tk, Tv ).
