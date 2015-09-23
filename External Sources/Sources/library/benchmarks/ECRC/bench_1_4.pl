

/* This program is called with "index(N)"                         */
/* It tests the efficiency of simple indexing on the 1st argument */
/* suggested value for N: 500 (interp), 2000(comp) */
/* results for Cprolog: N=500  */
/* Tloop=8.98 Tcomp=0.52 Tnet=8.47 Klips=1.24  */

index(N)
     :- statistics(runtime,[T1|_]),
        index_loop(N), statistics(runtime,[T2|_]),
        compens_loop(N), statistics(runtime,[T3|_]),
        print_times(T1,T2,T3,N,21).

/* loop with calls to the actual benchmark program for indexing */
index_loop(0).
index_loop(X) :- p(a), p([a]), p(s(a)), /* queries to the actual */
                 p(b), p([b]), p(t(b)), /* benchmark program     */
                 p(c), p([c]), p(u(c)),
                 p(d), p([d]), p(v(d)),
                 p(e), p([e]), p(w(e)),
                 p(f), p([f]), p(x(f)),
                 p(g), p([g]), p(y(g)),
                 Y is X - 1, index_loop(Y).

/* compensation loop */
compens_loop(0).
compens_loop(X) :- Y is X - 1, compens_loop(Y).

/* test program which can be optimised by indexing */
p(a).
p([a]).
p(s(a)).
p(b).
p([b]).
p(t(b)).
p(c).
p([c]).
p(u(c)).
p(d).
p([d]).
p(v(d)).
p(e).
p([e]).
p(w(e)).
p(f).
p([f]).
p(x(f)).
p(g).
p([g]).
p(y(g)).


print_times(T1,T2,T3,X,I) :-        /* prints the results */
        TT1 is T2 - T1,
        TT2 is T3 - T2,
        TT is TT1 - TT2,
        write('T first loop:     '),write(TT1), nl,
        write('T compens loop:   '),write(TT2), nl,
        write('T net:            '),write(TT),nl,
        write('KLips:            '),
        Li is I * X,
        Lips is Li / TT,
        KLips is Lips / 1000,
        write(KLips),nl,nl.
