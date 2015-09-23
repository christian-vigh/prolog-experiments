
/* envir(N): 3 arguments environment creation */
/*         creates 79 environments and 158 calls     */
/* suggested value for N: 1000 (interp), 1000 (comp) */
/* results for Cprolog: N=1000           */
/* Tloop=38.6 Tcomp=0.97 Tnet=37.6 Klips=4.23 */

envir(N):-statistics(runtime,[T1|_]),
        cre_env(N), statistics(runtime,[T2|_]),
        compens_loop(N), statistics(runtime,[T3|_]),
        print_times(T1,T2,T3,N,159).

cre_env(0).
cre_env(N):-M is N-1, env0(X,Y,Z), cre_env(M).

compens_loop(0).
compens_loop(N):-M is N-1,compens_loop(M).

env0(X,Y,Z):-env1(Z,X,Y),env2(Y,Z,X). /* creates 79 environments */
env1(X,Y,Z):-env3(Z,Y,X),env4(Y,Z,X).
env2(X,Y,Z):-env3(Z,Y,X),env4(Y,Z,X).  /* and 158 calls */
env3(X,Y,Z):-env5(Z,Y,X),env6(Y,Z,X).
env4(X,Y,Z):-env5(Z,Y,X),env6(Y,Z,X).
env5(X,Y,Z):-env7(Z,Y,X),env8(Y,Z,X).
env6(X,Y,Z):-env7(Z,Y,X),env8(Y,Z,X).
env7(X,Y,Z):-env9(Z,Y,X),env10(Y,Z,X).
env8(X,Y,Z):-env9(Z,Y,X),env10(Y,Z,X).
env9(X,Y,Z):-env11(Z,Y,X),env12(Y,Z,X).
env10(X,Y,Z):-env12(Z,Y,X),env12(Y,Z,X).
env11(X,Y,Z):-env12(Z,Y,X),env12(Y,Z,X).
env12(X,Y,Z).


/* envir0ar(N): zero argument environment creation */
/*       creates 79 environments and 158 calls     */
/* suggested value for N: 1000 (interp), 1000 (comp) */
/* results for Cprolog: N=1000           */
/* Tloop=18.88 Tcomp=1.01 Tnet=17.87 Klips=8.9 */

envir0ar(N):-statistics(runtime,[T1|_]),
        cre_env0ar(N), statistics(runtime,[T2|_]),
        compens_loop(N), statistics(runtime,[T3|_]),
        print_times(T1,T2,T3,N,159).

cre_env0ar(0).
cre_env0ar(N):-M is N-1, env0, cre_env(M).


env0:-env1,env2. /* creates 79 environments */
env1:-env3,env4.
env2:-env3,env4.  /* and 158 calls */
env3:-env5,env6.
env4:-env5,env6.
env5:-env7,env8.
env6:-env7,env8.
env7:-env9,env10.
env8:-env9,env10.
env9:-env11,env12.
env10:-env12,env12.
env11:-env12,env12.
env12.


print_times(T1,T2,T3,X,I) :-        /* prints the results */
        TT1 is T2 - T1,
        TT2 is T3 - T2,
        TT is TT1 - TT2,
        write('T overall loop:   '),write(TT1), nl,
        write('T compens loop:   '),write(TT2), nl,
        write('T net:            '),write(TT),nl,
        write('KLips:            '),
        Li is I * X,
        Lips is Li / TT,
        KLips is Lips / 1000,
        write(KLips),nl,nl.
