%%% UNIFICATION

% The following unification predicates FAILS on suspension!

% READ ONLY VARIABLE
% uro(X, Read_only_variable)
uro(X, Y) :- nonvar(Y), !, unify(X, Y).
uro(X, _) :- nonvar(X), note_suspension.
uro(R?, R).

% ATOMIC
% uatom(X, Atom)
uatom(X, X) :- !.
uatom(X?, A) :-
    nonvar(X), !, uatom(X, A) ;
    note_suspension.

% unil(X) : Hacked version of uatom.
unil([]) :- !.
unil(X?) :-
    nonvar(X), !, unil(X) ;
    note_suspension.

% COMPOUND TERM
% uskel(X, Skeleton)
uskel(X, X) :- !.
uskel(X?, S) :-
    nonvar(X), !, uskel(X, S) ;
    note_suspension.

% ulist(X, Car, Cdr) : Hacked version of uskel.
ulist([H|T], H, T) :- !.
ulist(X?, H, T) :-
    nonvar(X), !, ulist(X, H, T) ;
    note_suspension.

% GENERAL CASES
% unify(X, Y)
unify(X, Y) :- nonvar(X) , nonvar(Y), !, unify_nv(X, Y).
%unify(X, X?).                          %  added
%unify(X?, X).
unify(X, X).

% unify_nv(X, Y) : unifier for when arguments are non-variables.
unify_nv(X, Y?) :- !,
    ( nonvar(Y), !, unify_nv(X, Y) ; note_suspension ).
unify_nv(X?, Y) :- !,
    ( nonvar(X), !, unify_nv(X, Y) ; note_suspension ).
unify_nv([X|Xs], [Y|Ys]) :- !, unify(X, Y), unify(Xs, Ys).     % Hack!
unify_nv(X, Y) :- functor(X, F, A), functor(Y, F, A), unify_args(A, X, Y).

% unify_args(Arity, Term_X, Term_Y).
% Hack: Special clauses for small arity cases!
unify_args(0, _, _) :- !.
unify_args(1, X, Y) :- !,
     arg(1, X, X1), arg(1, Y, Y1), unify(X1, Y1).
unify_args(2, X, Y) :- !,
     arg(1, X, X1), arg(1, Y, Y1), unify(X1, Y1),
     arg(2, X, X2), arg(2, Y, Y2), unify(X2, Y2).
unify_args(3, X, Y) :- !,
     arg(1, X, X1), arg(1, Y, Y1), unify(X1, Y1),
     arg(2, X, X2), arg(2, Y, Y2), unify(X2, Y2),
     arg(3, X, X3), arg(3, Y, Y3), unify(X3, Y3).
unify_args(N, X, Y) :-
     N1 is N-1, unify_args(N1, X, Y),
     arg(N, X, Xn), arg(N, Y, Yn), unify(Xn, Yn).



%  Handling suspension
note_suspension   :-
   $suspension, !, fail  ; assert($suspension), !, fail.
clear_suspension  :-
   retract($suspension) ; true.
verify_suspension  :-
   retract($suspension).
% Utilities / System Predicates
