%%% COMPILATION

% extra op's for FCP

:- op(700, xfx, '#<').
:- op(700, xfx, '#=').

:- op(950, xfy, '\').

fcpc(F) :-
   fcpcomp(F).


fcpcomp([]) :- !.
fcpcomp([H|T]) :- !, fcpcomp(H), fcpcomp(T).
fcpcomp(F) :-
     seeing(F0), nofileerrors, see(F), !,
     abolish($abolished,1),
     display('Compiling '), St is cputime, Sh is heapused,
     do_fcpcomp(X),
     close(F), see(F0), !,
     nl, display(F), display(' compiled  '),
     Eh is heapused-Sh, write(Eh),write(' bytes '),
     En is cputime-St, display(En), write(' sec.'),
     nl.
fcpcomp(F) :-
     fileerrors,
     display('Cannot open file: '), display(F), nl.

do_fcpcomp(X)  :- repeat, read(X), fcpcomp1(X), !.


fcpcomp1(end_of_file) :- !, abolish($abolished,1).
fcpcomp1(X) :- !,
  (  X = (Head:-_), !; X = (Head<-_),! ; X = Head ),
     functor(Head, F, A),
  ( $abolished(F/A) ;
     functor(XX, F, A), A2 is A+3, abolish(F,A2),
     assert($abolished(F/A)),nl, display(F/A)  ),
     c_clause(X, C0), c_post(C0, C),
     c_otherwise(C), assert(C), display('.'),!, fail.

member1(X, [X|_]) :- !.
member1(X, [_|L]) :- member1(X, L).

c_post(V, V) :- var(V), !.
c_post($$$(V0), V) :- !, c_post(V0, V).
c_post(X, XX) :-
     functor(X, F, A), functor(XX, F, A), c_post_args(A, X, XX).

c_post_args(0, _, _) :- !.
c_post_args(K, X, XX) :-
     K1 is K-1, c_post_args(K1, X, XX),
     arg(K, X, XK), c_post(XK, XXK), arg(K, XX, XXK).

c_clause((Head <- S_Guard '|' S_Body), (H:-C_Head_Guard)) :- !,
     c_c(Head, S_Guard, H0, C_Head_Guard), put_queue(S_Body, Q, T),
     c_make_head(H0, H, Q, T, S_Guard).
c_clause((Head <- S_Body), (H:-C_Head_Guard)) :- !,
     c_c(Head, true, H0, C_Head_Guard), put_queue(S_Body, Q, T),
     c_make_head(H0, H, Q, T, true).

c_clause((Head :- S_Guard ; S_Body), (H:-C_Head_Guard)) :- !,
     c_c(Head, S_Guard, H0, C_Head_Guard), put_queue(S_Body, Q, T),
     c_make_head(H0, H, Q, T, S_Guard).
c_clause((Head :- S_Body), (H:-C_Head_Guard)) :- !,
     c_c(Head, true, H0, C_Head_Guard), put_queue(S_Body, Q, T),
     c_make_head(H0, H, Q, T, true).
c_clause(Head, (H:-C_Head_Guard)) :-
     c_c(Head, true, H0, C_Head_Guard),
     c_make_head(H0, H, Q, Q, true).

put_queue((X,Y), Q, T) :- !,
     put_queue(X, Q, Q0), put_queue(Y, Q0, T).
put_queue(true, Q, Q) :- !.
put_queue(Source,[$kernel(Source,Code)|Q],Q)  :-
   body_kernel(Source,Code1),!, c_opt(Code1,Code,_).
put_queue(X, [$(XX, Q1, Q2)|Q], Q) :- c_make_head(X, XX, Q1, Q2, _).


% body_kernel is in kernel.pl

c_make_head(X, XX, Q, T, S_Guard) :-
     functor(X, F, A),
     A1 is A+1, A2 is A+2, A3 is A+3,
     functor(XX, F, A3),
     c_copy_args(A, X, XX),
     arg(A1, XX, Q), arg(A2, XX, T),
     arg(A3, XX, S_Guard).


c_copy_args(0, _, _) :- !.
c_copy_args(K, X, XX) :-
     arg(K, X, XK), arg(K, XX, XK), K1 is K-1, c_copy_args(K1, X, XX).

c_c(Head, Guard, H, G) :-
     functor(Head, F, A), functor(H, F, A),
     c_args(0, A, Head, H, GH),
     comp_guard(Guard, GG),
     c_opt((GH,GG),G,_).

c_args(N, N, Head, _, true) :- !.
c_args(K, N, Head, H, (GK, G)) :-
     K1 is K+1, arg(K1, Head, AK), arg(K1, H, HK),
     c_unify(AK, HK, GK, 3), c_args(K1, N, Head, H, G).

% c_unify(Original_argument, Generated_argument, Unification_code, Level).
%     Note that f(X,X) must be compiled into f(X, Y) :- unify(X, Y).
%     This is because X and Y may be A? and B?, in which case the
%     unification should be suspended.
%     "Level" is for stopping expansion at certain level.

c_unify(X, X, true, _) :- var(X), !, X= $$$(Y).     % mark X as "used!"
c_unify($$$(X), Y, unify(X, Y), _) :- !.
c_unify(V?, X, uro(X, V), _) :- var(V), !.
c_unify(T?, X, G, L) :- !, c_unify(T, X, G, L).
c_unify([], X, unil(X), _) :- !.
c_unify(A, X, uatom(X, A), _) :- atomic(A), !.
c_unify(X, Y, unify(X, Y), 0) :- !.     % Stop expansion at given level
c_unify([H|T], X, (ulist(X, H0, T0), UH, UT), L) :- !,
     L1 is L-1, c_unify(H, H0, UH, L1), c_unify(T, T0, UT, L1).
c_unify(S, X, (uskel(X, S0), UA), L) :-
     L1 is L-1, functor(S, F, A), functor(S0, F, A),
     c_unify_args(0, A, S, S0, UA, L1).

c_unify_args(N, N, _, _, true, _) :- !.
c_unify_args(K, N, S, X, (UK, UA), L) :-
     K1 is K+1, arg(K1, S, SK), arg(K1, X, XK), c_unify(SK, XK, UK, L),
     c_unify_args(K1, N, S, X, UA, L).

comp_guard(Guard, G)  :-
     c_guard(Guard, G),!.
comp_guard(Guard, fail)  :-
     write(illegal_guard(Guard)), nl.

c_guard(X, _)  :- var(X),!, fail.
c_guard((G1,G2),(Gc1,Gc2)) :- !, c_guard(G1,Gc1), c_guard(G2,Gc2).
c_guard(true, true) :- !.
c_guard(otherwise, fcpotherwise) :- !.
c_guard(guard_kernel(X), (W, guard_kernel(X1)))  :- !, c_w(X, W, X1).
c_guard(body_kernel(X), (W, body_kernel(X1)))  :- !, c_w(X, W, X1).
