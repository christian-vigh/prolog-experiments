%%% RUNTIME SUPPORT ROUTINES

fcpwait(X?, Y) :- !,
    ( nonvar(X), !, fcpwait(X,Y) ; note_suspension ).
% Here, 1st arg is non-variable.
fcpwait(X, X).

fcpwait_ground(X, Y) :- var(X), !, note_suspension.
fcpwait_ground(X?, Y) :- !, fcpwait_ground(X, Y).
fcpwait_ground(X, Y) :-
     functor(X, F, A), functor(Y, F, A), fcpwait_ground_args(A, X, Y).

fcpwait_ground_args(0, _, _) :- !.
fcpwait_ground_args(K, X, Y) :-
     K1 is K-1, fcpwait_ground_args(K1, X, Y),
     arg(K, X, KX), fcpwait_ground(KX, KY), arg(K, Y, KY).


fcpdif(X, Y?) :- !, ( nonvar(Y), !, fcpdif(X, Y) ; note_suspension ).
fcpdif(X?, Y) :- !, ( nonvar(X), !, fcpdif(X, Y) ; note_suspension ).
% Here, 1st & 2nd args are non-variable.
fcpdif(X, Y) :- functor(X, F, A), functor(Y, F, A), !, fcpdif_args(A, X, Y).
% Succeed when functor symbol or arity mismatch.
fcpdif(_, _).

fcpdif_args(0, _, _) :- !, note_suspension.
fcpdif_args(N, X, Y) :- arg(N, X, XN), arg(N, Y, YN), fcpdif(XN, YN), !.
fcpdif_args(N, X, Y) :- N1 is N-1, fcpdif_args(N1, X, Y).

fcpwrite(X) :-
      remove_useless_ro(X, XXX), write(XXX), nl.

% Works only in PROLOG 20.
fcpread(X) :- ttywait, read(X).

remove_ro(X, X) :- var(X), !.
remove_ro(X?, Y) :- !, remove_ro(X, Y).
remove_ro(X, Y) :-
     functor(X, F, A), functor(Y, F, A), remove_ro_args(A, X, Y).

remove_ro_args(0, _, _) :- !.
remove_ro_args(K, X, Y) :-
     K1 is K-1, remove_ro_args(K1, X, Y),
     arg(K, X, KX), remove_ro(KX, KY), arg(K, Y, KY).

remove_useless_ro(X, X) :- var(X), !.
remove_useless_ro(X?, X?) :- var(X), !.
remove_useless_ro(X?, Y) :- !, remove_useless_ro(X, Y).
remove_useless_ro([X|Xs], [Y|Ys]) :- !,
    remove_useless_ro(X,Y), remove_useless_ro(Xs,Ys).
remove_useless_ro(X, Y) :- !,
     functor(X, F, A), functor(Y, F, A), remove_useless_ro_args(A, X, Y).

remove_useless_ro_args(0, _, _) :- !.
remove_useless_ro_args(K, X, Y) :-
     K1 is K-1, remove_useless_ro_args(K1, X, Y),
     arg(K, X, KX), remove_useless_ro(KX, KY), arg(K, Y, KY).


fcpotherwise  :-
   $suspension, !, fail.
fcpotherwise.
guard_kernel((G1,G2)) :-
   guard_kernel(G1), guard_kernel(G2).
guard_kernel(true).
guard_kernel(otherwise).
guard_kernel(guard_kernel(X)).
guard_kernel(body_kernel(X)).
guard_kernel(var(X)).
guard_kernel(atom(X)).
guard_kernel(atomic(X)).
guard_kernel(nonvar(X)).
guard_kernel(structure(X)).
guard_kernel(functor(X,Y,Z)).
guard_kernel(read_only(X)).
guard_kernel(not_read_only(X)).
guard_kernel(do_guard_kernel(X)).
guard_kernel(X#=Y).
guard_kernel(X#<Y).
guard_kernel(X\=Y).
guard_kernel(X<Y).
guard_kernel(X>Y).
guard_kernel(X=<Y).
guard_kernel(X>=Y).
guard_kernel(X=:=Y).
guard_kernel(X=\=Y).
body_kernel(X:=Y).
body_kernel(clauses(X,Y)).
body_kernel(write(X)).
body_kernel(read(X)).
body_kernel(prolog(X)).
body_kernel(atom_to_chars(A,C)).
body_kernel(chars_to_atom(C,A)).
body_kernel(arg(N,T,A)).
body_kernel(extract_functor(T,F,A)).
body_kernel(make_term(T,F,A)).
body_kernel(true).
body_kernel(do_body_kernel(K,R)).
body_kernel(gc).                                %  not implemented
body_kernel(char_input(F,S)).              %  not implemented
body_kernel(output(F,S)).                   % not implemented
body_kernel(screen(S)).                      % not implemented
body_kernel(tokenize(C,T)).                 % not implemented
do_guard_kernel((X,Y))  :-
   do_guard_kernel(X),do_guard_kernel(Y).    % Check if this is what we want
do_guard_kernel(X)  :-
   comp_guard(X,CX), !, CX.     %  compile the guard and execute it.
do_body_kernel(X,R)  :-
   body_kernel(X,CX), CX,!,     %  compile the body kernel  and execute it.
   R=true.
do_body_kernel(X,false).
fcpclauses(F,Cs)  :-
    remove_ro(F,F1),
    functor(F1,Fu,A),
    functor(Fc,Fu,A+3), c_copy_args(A,F1,Fc),
    get_all_clauses(Fu,Fc,A,Cs).
get_all_clauses(_,Fc,_,[])  :-
   not Fc,!.
get_all_clauses(_,Fc,A,_) :-
   Fc, asserta($clause$(Fc)), fail.
get_all_clauses(Fu,_,A,Cs) :-
    gather_clauses(Fu,A,Cs).
gather_clauses(Fu,A,C)  :-
    setof(X,$clause$(X),Y),
    extract_clauses(Fu,Y,A,C), abolish($clause$,1).
extract_clauses(Fu,[X|Xs],A,[clause(H,G,B)|Cs]) :-
   functor(H,Fu,A), arg(A+3,X,G),c_copy_args(A,X,H),
   arg(A+1,X,Q), arg(A+2,X,QT), get_queue(Q,QT,B1),
   list_to_conj(B1,B),
   extract_clauses(Fu,Xs,A,Cs).
extract_clauses(_,[],_,[]).
list_to_conj([],true).
list_to_conj([X],X).
list_to_conj([X|Xs],(X,X1s))  :-
   list_to_conj(Xs,X1s).
