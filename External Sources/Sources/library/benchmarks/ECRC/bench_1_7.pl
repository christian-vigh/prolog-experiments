
cuttest(N):-statistics(runtime,[T1|_]),
        cutit11(N), statistics(runtime,[T2|_]),
        compens_loop(N), statistics(runtime,[T3|_]),
        print_times(T1,T2,T3,N,300).

compens_loop(0).
compens_loop(X) :- Y is X - 1, compens_loop(Y).

/* cutit11(N)    */
cutit11(0).
cutit11(N):- cutt1([100,100,100,100,100,100,100,100,100,100,
                 100,100,100,100,100,100,100,100,100,100,
                 100,100,100,100,100,100,100,100,100,100,
                 100,100,100,100,100,100,100,100,100,100,
                 100,100,100,100,100,100,100,100,100,100,
                 100,100,100,100,100,100,100,100,100,100,
                 100,100,100,100,100,100,100,100,100,100,
                 100,100,100,100,100,100,100,100,100,100,
                 100,100,100,100,100,100,100,100,100,100,
                 100,100,100,100,100,100,100,100,100,100]),
           M is N-1, cutit11(M).

cutt1([]).
cutt1([X|L]):-X=100, !, cutt1(L).
cutt1([X|L]):-X > 100, cutt1(L).

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
