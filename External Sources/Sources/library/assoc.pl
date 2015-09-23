%   File   : ASSOC.PL
%   Author : R.A.O'Keefe
%   Updated: 9 November 1983
%   Purpose: Binary tree implementation of "association lists".

%   Note   : the keys should be ground, the associated values need not be.

:- public
	assoc_to_list/2,
	gen_assoc/3,
	get_assoc/3,
	list_to_assoc/2,
	map_assoc/3,
	put_assoc/4.

:- mode
	assoc_to_list(+, -),
	    assoc_to_list(+, -, +),
	gen_assoc(+, ?, ?),
	get_assoc(+, +, ?),
	    get_assoc(+, +, +, +, +, ?),
	list_to_assoc(+, -),
	    list_to_assoc(+, +, -, +),
	map_assoc(+, +, -),
	put_assoc(+, +, +, -),
	    put_assoc(+, +, +,+,+,+, +, -).


assoc_to_list(Assoc, List) :-
	assoc_to_list(Assoc, List, []).


	assoc_to_list(t(Key,Val,L,R), List, Rest) :-
		assoc_to_list(L, List, [Key-Val|More]),
		assoc_to_list(R, More, Rest).
	assoc_to_list(t, List, List).



gen_assoc(t(_,_,L,_), Key, Val) :-
	gen_assoc(L, Key, Val).
gen_assoc(t(Key,Val,_,_), Key, Val).
gen_assoc(t(_,_,_,R), Key, Val) :-
	gen_assoc(R, Key, Val).



get_assoc(Key, t(K,V,L,R), Val) :-
	compare(Rel, Key, K),
	get_assoc(Rel, Key, V, L, R, Val).


	get_assoc(=, _, Val, _, _, Val).
	get_assoc(<, Key, _, Tree, _, Val) :-
		get_assoc(Key, Tree, Val).
	get_assoc(>, Key, _, _, Tree, Val) :-
		get_assoc(Key, Tree, Val).



list_to_assoc(List, Assoc) :-
	keysort(List, Keys),
	length(Keys, N),
	list_to_assoc(N, Keys, Assoc, []).


	list_to_assoc(0, List, t, List).
	list_to_assoc(N, List, t(Key,Val,L,R), Rest) :-
		A is (N-1)/2,
		Z is (N-1)-A,
		list_to_assoc(A, List, L, [Key-Val|More]),
		list_to_assoc(Z, More, R, Rest).



map_assoc(Pred, t(Key,Val,L0,R0), t(Key,Ans,L1,R1)) :- !,
	map_assoc(Pred, L0, L1),
	apply(Pred, [Val,Ans]),
	map_assoc(Pred, R0, R1).
map_assoc(_, t, t).



put_assoc(Key, t(K,V,L,R), Val, New) :-
	compare(Rel, Key, K),
	put_assoc(Rel, Key, K, V, L, R, Val, New).
put_assoc(Key, t, Val, t(Key,Val,t,t)).


	put_assoc(=, Key, _, _, L, R, Val, t(Key,Val,L,R)).
	put_assoc(<, Key, K, V, L, R, Val, t(K,V,Tree,R)) :-
		put_assoc(Key, L, Val, Tree).
	put_assoc(>, Key, K, V, L, R, Val, t(K,V,L,Tree)) :-
		put_assoc(Key, R, Val, Tree).

