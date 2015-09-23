body_kernel(clauses(X,Y), (W, fcpclauses(X1,Y))) :- !, c_w(X, W, X1).
body_kernel(do_body_kernel(X,R), (W, do_body_kernel(X1,R))) :- !, c_w(X, W, X1).
% body_kernel(screen(X),screen(X)) :- !. % IS actually kernel, but is not
				       % compiled as such, rather as a
				       % utility
body_kernel(true, true) :- !.
body_kernel(fail, fail) :- !.
body_kernel(prolog(X), (remove_ro(X, XX), call(XX))) :- !.
body_kernel(extract_functor(T,F,A), (WT,functor(T1,F,A)))  :- !,
    c_w(T, WT, T1).
body_kernel(make_term(T,F,A),
	( WF,WA,functor(T,F1,A1)) )  :- !,
    c_w(F, WF, F1), c_w(A, WA, A1).
body_kernel(arg(N,T,A), (WN,WT,arg(N1,T1,A)))  :- !,
    c_w(N,WN,N1), c_w(T,WT,T1).
body_kernel(atom_to_chars(A,C), (W, fcp_atom_to_chars(A1,C)))  :- !,
    c_w_all(A,W,A1).
body_kernel(chars_to_atom(C,A), (W, fcp_chars_to_atom(C1,A)))  :- !,
    c_w_all(C,W,C1).
body_kernel(X:=Y, (W, X is Y1)) :- !, c_w_all(Y, W, Y1).
body_kernel(write(X),fcpwrite(X)).    %  not actually a kernel pred
body_kernel(resolve(Goal,Prog,Body),(W, resolve(G,Body))) :-
    c_w(Goal,W,G).
fcp_atom_to_chars([],[]) :- !.
fcp_atom_to_chars(A,C)  :-
   name(A,C1),
   do_atom_to_chars(C1,C).
do_atom_to_chars([],[]).
do_atom_to_chars([C|Cs],[C1|C1s])  :-
   name(C1,[C]),
   do_atom_to_chars(Cs,C1s).
fcp_chars_to_atom([],[]) :- !.
fcp_chars_to_atom(C,A)  :-
   change_list(C,C1),
   name(A,C1).
change_list([],[]).
change_list([C|Cs],[C1|C1s]) :-
   name(C,[C1]),
   change_list(Cs,C1s).
resolve(Goal,Body)  :-
    put_queue(Goal,Q,[]),
    resolve1(Q,Body).
resolve1([$kernel(K,C)],true)  :-
    call(C).
resolve1([$kernel(K,C)],false)  :-!.
resolve1([$(G,QH,QT)],Body)  :-
    call(G),
    get_queue(QH,QT,BodyList),
    list_to_conj(BodyList,Body).


%  pseudo body kernel predicates for speeding things up
% copy
body_kernel(copy(X,Y),pseu_copy(X,Y)).
pseu_copy(X,Y) :-
   remove_useless_ro(X,X1),
   assert($$(X1)),retract($$(Y)).
