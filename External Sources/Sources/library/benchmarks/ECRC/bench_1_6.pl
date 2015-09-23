deref(N):-
        make_list(500, L1, _),
        make_list(500, L2, Last),
        bind_forward(L1),
        bind_backward(L2),
        L2 = [a|_],
        statistics(runtime,[T1|_]),
        ref(N, L1),
        statistics(runtime,[T2|_]),
        ref(N, Last),
        statistics(runtime,[T3|_]),
        print_times(T1,T2,T3,N,12).


print_times(T1,T2,T3,X,I) :-        /* prints the results */
        TT1 is T2 - T1,
        TT2 is T3 - T2,
        abs_diff(TT1, TT2, TT),
        write('T first loop:    '),write(TT1), nl,
        write('T second loop:   '),write(TT2), nl,
        write('T net:           '),write(TT),nl,
        write('KLips:           '),
        Li is I * X,
        Lips is Li / TT,
        KLips is Lips / 1000,
        write(KLips),nl,nl.

abs_diff(X,Y,Z) :- X > Y, !, Z is X - Y.
abs_diff(X,Y,Z) :- Z is Y - X.

/*
 * Bind repeatively a cons cell to another one.
 */
ref(0, _) :- !.
ref(N, Cons) :-
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        Cons = [a|_],
        N1 is N - 1,
        ref(N1, Cons).

/*
 * Create a variable chain if in ?- equal(X, Y) the system binds
 * X to Y.
 */
bind_forward([a]) :- !.
bind_forward([X, Y|T]) :-
        equal(X, Y),
        bind_forward([Y|T]).

/*
 * Create a variable chain if in ?- equal(X, Y) the system binds
 * Y to X.
 */
bind_backward([X]) :- !.
bind_backward([X, Y|T]) :-
        bind_backward([Y|T]),
        equal(X, Y).

equal(X, X).

/*
 * Create a list containing variables and return the pointer to the
 * first and to the last cons cell.
 */
make_list(1, L, L) :-
        L = [X],
        !.
make_list(N, [X|Rest], Last) :-
        N1 is N - 1,
        make_list(N1, Rest, Last).
