/* Common functions...       */

print_times(T1,T2,T3,L) :-
        TT1 is T2 - T1,
        TT2 is T3 - T2,
        TT is TT1 - TT2,
        write('Net Time is: '), write(TT), nl,
        Lips is L / TT,
        Klips is Lips / 1000,
        write('KLips are: '), write(Klips), nl.

compens_loop(0).
compens_loop(X) :- Y is X - 1, compens_loop(Y).

el(X,[X|L]).
el(X,[Y|L]):-el(X,L).

list50([27,74,17,33,94,18,46,83,65,2,
       32,53,28,85,99,47,28,82,6,11,
       55,29,39,81,90,37,10,0,66,51,
        7,21,85,27,31,63,75,4,95,99,
       11,28,61,74,18,92,40,53,59,8]).

/* Fibonacci Series the slow way            */
/* fibonacci(1) will do...                  */

fibonacci(N) :- statistics(runtime,[X|_]),
          fib_loop(N),
          statistics(runtime,[Now|_]),
          compens_loop(N),
          statistics(runtime,[M|_]),
          Li is 4932 * N,
          print_times(X,Now,M,Li).


fib_loop(0).
fib_loop(X) :- \+ \+ top_fib(15,Z), Y is X - 1, fib_loop(Y).

top_fib(0,1).
top_fib(1,1).
top_fib(X,Y):-X1 is X-1,X2 is X-2,top_fib(X1,Y1),
             top_fib(X2,Y2),Y is Y1+Y2.

/* ------------------------------------ */
/* Map colouring problem                */
/*  map(200) is advised.                */

map(N) :- statistics(runtime,[X|_]),
          map_loop(N),
          statistics(runtime,[Now|_]),
          compens_loop(N),
          statistics(runtime,[M|_]),
          Li is 68 * N,
          print_times(X,Now,M,Li).

map_loop(0).
map_loop(X) :- \+ \+ map_top, Y is X - 1, map_loop(Y).

map_top:-
	el(X1,[b]),
	el(X2,[r]),
	el(X7,[g]),
	el(X13,[w]),
	el(X3,[b,r,g,w]),
	\+(X2=X3),
	\+(X3=X13),
	el(X4,[b,r,g,w]),
	\+(X2=X4),
	\+(X7=X4),
	\+(X3=X4),
	el(X5,[b,r,g,w]),
	\+(X13=X5),
	\+(X3=X5),
	\+(X4=X5),
	el(X6,[b,r,g,w]),
	\+(X13=X6),
	\+(X5=X6),
	el(X8,[b,r,g,w]),
	\+(X7=X8),
	\+(X13=X8),
	el(X9,[b,r,g,w]),
	\+(X13=X9),
	\+(X4=X9),
	\+(X8=X9),
	el(X10,[b,r,g,w]),
	\+(X4=X10),
	\+(X5=X10),
	\+(X6=X10),
	\+(X9=X10),
	el(X11,[b,r,g,w]),
	\+(X11=X13),
	\+(X11=X10),
	\+(X11=X6),
	el(X12,[b,r,g,w]),
	\+(X12=X13),
	\+(X12=X11),
	\+(X12=X9),
	write(user,[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13]),nl.

map_top.

/* ---------------------------------------------- */
/*  Hamiltonian Graphs...                         */
/*  Extremely long (nearly half a million LI's !) */
/*  Only 1 advised !                              */

mham(N) :- statistics(runtime,[X|_]),
          mham_loop(N),
          statistics(runtime,[Now|_]),
          compens_loop(N),
          statistics(runtime,[M|_]),
          Li is 493824 * N,
          print_times(X,Now,M,Li).

mham_loop(0).
mham_loop(X) :- \+ \+ mham_top, Y is X - 1, mham_loop(Y).

mham_top:-
        cycle_ham([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t],X),
        fail.
mham_top.

cycle_ham([X|Y],[X,T|L]):-
        chain_ham([X|Y],[],[T|L]),
        edge(T,X).

chain_ham([X],L,[X|L]).
chain_ham([X|Y],K,L):-
        delete(Z,Y,T),
        edge(X,Z),
        chain_ham([Z|T],[X|K],L).

delete(X,[X|Y],Y).
delete(X,[U|Y],[U|Z]):-
        delete(X,Y,Z).

edge(X,Y):-
        connect(X,L),
        el(Y,L).

connect(0,[1,2,3,4,5,6,7,8,9]).
connect(1,[0,2,3,4,5,6,7,8,9]).
connect(2,[0,1,3,4,5,6,7,8,9]).
connect(3,[0,1,2,4,5,6,7,8,9]).
connect(4,[0,1,2,3,5,6,7,8,9]).
connect(5,[0,1,2,3,4,6,7,8,9]).
connect(6,[0,1,2,3,4,5,7,8,9]).
connect(7,[0,1,2,3,4,5,6,8,9]).
connect(8,[0,1,2,3,4,5,6,7,9]).
connect(9,[0,1,2,3,4,5,6,7,8]).

connect(a,[b,j,k]).
connect(b,[a,c,p]).
connect(c,[b,d,l]).
connect(d,[c,e,q]).
connect(e,[d,f,m]).
connect(f,[e,g,r]).
connect(g,[f,h,n]).
connect(h,[i,g,s]).
connect(i,[j,h,o]).
connect(j,[a,i,t]).
connect(k,[o,l,a]).
connect(l,[k,m,c]).
connect(m,[l,n,e]).
connect(n,[m,o,g]).
connect(o,[n,k,i]).
connect(p,[b,q,t]).
connect(q,[p,r,d]).
connect(r,[q,s,f]).
connect(s,[r,t,h]).
connect(t,[p,s,j]).

/* -------------------------------------------- */
/*  Hofstader's mu math (mutest) proving muiiu  */
/*  from Godel Escher Bach                      */
mutest(N) :- statistics(runtime,[X|_]),
          mu_loop(N),
          statistics(runtime,[Now|_]),
          compens_loop(N),
          statistics(runtime,[M|_]),
          Li is 1366 * N,
          print_times(X,Now,M,Li).

mu_loop(0).
mu_loop(X) :- \+ \+ mu_top, Y is X - 1, mu_loop(Y).

mu_top:- theorem(5,[m,u,i,i,u]).

rules(S, R) :- rule3(S,R).
rules(S, R) :- rule4(S,R).
rules(S, R) :- rule1(S,R).
rules(S, R) :- rule2(S,R).

rule1(S,R) :-
        append(X, [i], S),
        append(X, [i,u], R).

rule2([m | T], [m | R]) :- append(T, T, R).

rule3([], -) :- fail.
rule3(R, T) :-
        append([i,i,i], S, R),
        append([u], S, T).
rule3([H | T], [H | R]) :- rule3(T, R).

rule4([], -) :- fail.
rule4(R, T) :- append([u, u], T, R).
rule4([H | T], [H | R]) :- rule4(T, R).

theorem(Depth, [m, i]).
theorem(Depth, []) :- fail.

theorem(Depth, R) :-
        Depth > 0,
        D is Depth - 1,
        theorem(D, S),
        rules(S, R).

append([], X, X).
append([A | B], X, [A | B1]) :-
        !,
        append(B, X, B1).
/* ------------------------------------  */
/*  Quicksort of 50 element list         */
/*                                       */

qs(N) :-  list50(L),
          statistics(runtime,[X|_]),
          qs_loop(N,L),
          statistics(runtime,[Now|_]),
          compens_loop(N),
          statistics(runtime,[M|_]),
          Li is 601 * N,
          print_times(X,Now,M,Li).

qs_loop(0,_).
qs_loop(X,L) :- qsort(L,Z,[]), Y is X - 1,qs_loop(Y,L).

qsort([X|L],R,R0) :-
      partition(L,X,L1,L2),
      qsort(L2,R1,R0),
      qsort(L1,R,[X|R1]).
qsort([],R,R).

partition([X|L],Y,[X|L1],L2) :- X =< Y,!,
      partition(L,Y,L1,L2).
partition([X|L],Y,L1,[X|L2]) :-
      partition(L,Y,L1,L2).
partition([],_,[],[]).

/* ------------------------------------ */
/*  Queens on a chess board problem...  */
/*  Only two solution on a 4x4 board... */
/*  about 5 - 10 is advised for N.      */
qu(N) :- statistics(runtime,[X|_]),
          qu_nloop(N),
          statistics(runtime,[Now|_]),
          compens_loop(N),
          statistics(runtime,[M|_]),
          Li is 684 * N,
          print_times(X,Now,M,Li).

qu_nloop(0).
qu_nloop(X) :- qu_top, Y is X - 1, qu_nloop(Y).

qu_top :-  run(4,X), fail.
qu_top.

size(4).
snint(1).
snint(2).
snint(3).
snint(4).

run(Size, Soln) :- get_solutions(Size, Soln).

get_solutions(Board_size, Soln) :- solve(Board_size, [], Soln).

/*  newsquare generates legal positions for next queen     */

newsquare([], square(1, X)) :- snint(X).
newsquare([square(I, J) | Rest], square(X, Y)) :-
        X is I + 1,
        snint(Y),
        \+(threatened(I, J, X, Y)),
        safe(X, Y, Rest).

/*   safe checks whether square(X, Y) is threatened by any */
/*   existing queens                                       */

safe(X, Y, []).
safe(X, Y, [square(I, J) | L]) :-
        \+(threatened(I, J, X, Y)),
        safe(X, Y, L).

/*    threatened checks whether squares (I, J) and (X, Y) */
/*    threaten each other                                 */

threatened(I, J, X, Y) :-
        (I = X),
        !.
threatened(I, J, X, Y) :-
        (J = Y),
        !.
threatened(I, J, X, Y) :-
        (U is I - J),
        (V is X - Y),
        (U = V),
        !.
threatened(I, J, X, Y) :-
        (U is I + J),
        (V is X + Y),
        (U = V),
        !.

/* solve accumulates the positions of occupied squares */

solve(Bs, [square(Bs, Y) | L], [square(Bs, Y) | L]) :- size(Bs).
solve(Board_size, Initial, Final) :-
        newsquare(Initial, Next),
        solve(Board_size, [Next | Initial], Final).

/* ------------------------------------ */
/* Query does simple database queries.  */
/*                                      */

query(N) :- statistics(runtime,[X|_]),
          que_nloop(N),
          statistics(runtime,[Now|_]),
          compens_loop(N),
          statistics(runtime,[M|_]),
          Li is 2294 * N,
          print_times(X,Now,M,Li).

que_nloop(0).
que_nloop(X) :- que_top, Y is X - 1, que_nloop(Y).

que_top:- que(X), fail.
que_top.

que([C1,D1,C2,D2]) :-
      density(C1,D1),
      density(C2,D2),
      D1>D2,
      20*D1<21*D2.

density(C,D) :-
      pop(C,P),
      area(C,A),
      D is (P*100)/A.

pop(china,8250).
pop(india,5863).
pop(ussr,2521).
pop(usa,2119).
pop(indonesia,1276).
pop(japan,1097).
pop(brazil,1042).
pop(bangladesh,750).
pop(pakistan,682).
pop(w_germany,620).
pop(nigeria,613).
pop(mexico,581).
pop(uk,559).
pop(italy,554).
pop(france,525).
pop(philippines,415).
pop(thailand,410).
pop(turkey,383).
pop(egypt,364).
pop(spain,352).
pop(poland,337).
pop(s_korea,335).
pop(iran,320).
pop(ethiopia,272).
pop(argentina,251).

area(china,3380).
area(india,1139).
area(ussr,8708).
area(usa,3609).
area(indonesia,570).
area(japan,148).
area(brazil,3288).
area(bangladesh,55).
area(pakistan,311).
area(w_germany,96).
area(nigeria,373).
area(mexico,764).
area(uk,86).
area(italy,116).
area(france,213).
area(philippines,90).
area(thailand,200).
area(turkey,296).
area(egypt,386).
area(spain,190).
area(poland,121).
area(s_korea,37).
area(iran,628).
area(ethiopia,350).
area(argentina,1080).

/* --------------------------------------------------*/
/*       differen (times10,divide10,log10,ops8)      */
/*       These 4 examples are from Warren's thesis   */
/*       differen(150) will do.                      */

differen(N) :- statistics(runtime,[X|_]),
          differenloop(N),
          statistics(runtime,[Now|_]),
          compens_loop(N),
          statistics(runtime,[M|_]),
          Li is 71 * N,
          print_times(X,Now,M,Li).

differenloop(0).
differenloop(X) :- \+ \+(differen_top), Y is X - 1, differenloop(Y).

differen_top:-
        times10(I1),
        d(I1,x,D1),
        divide10(I2),
        d(I2,x,D2),
        log10(I3),
        d(I3,x,D3),
        ops8(I4),
        d(I4,x,D4).

d(U+V,X,DU+DV) :- !, d(U,X,DU), d(V,X,DV).
d(U-V,X,DU-DV) :- !, d(U,X,DU), d(V,X,DV).
d(U*V,X,DU*V+U*DV) :- !, d(U,X,DU), d(V,X,DV).
d(U/V,X,(DU*V-U*DV)/(^(V,2))) :- !, d(U,X,DU), d(V,X,DV).
d(^(U,N),X,DU*N*(^(U,N1))) :- !, integer(N), N1 is N - 1, d(U,X,DU).
d(-U,X,-DU) :- !, d(U,X,DU).
d(exp(U),X,exp(U)*DU) :- !, d(U,X,DU).
d(log(U),X,DU/U) :- !, d(U,X,DU).
d(X,X,1).
d(C,X,0).

times10( ((((((((x*x)*x)*x)*x)*x)*x)*x)*x)*x ).
divide10( ((((((((x/x)/x)/x)/x)/x)/x)/x)/x)/x ).
log10( log(log(log(log(log(log(log(log(log(log(x)))))))))) ).
ops8( (x+1)*((^(x,2)+2)*(^(x,3)+3)) ).

/* --------------------------------------------------- */
/*  Difference Lists                                   */
/*       quicksort on 50 items (difference lists)      */

diff(N) :- list50(L),
          statistics(runtime,[X|_]),
          difflistloop(N,L),
          statistics(runtime,[Now|_]),
          compens_loop(N),
          statistics(runtime,[M|_]),
          Li is 608 * N,
          print_times(X,Now,M,Li).

difflistloop(0,_).
difflistloop(X,L) :- qdsort(L,Z), Y is X - 1, difflistloop(Y,L).

qdsort([X|L],R-R0) :-
        dpartition(L,X,L1,L2),
        qdsort(L1,R-[X|R1]),
        qdsort(L2,R1-R0).
qdsort([],R0-R0).

dpartition([X|L],Y,[X|L1],L2) :-
        X<Y, !,
        dpartition(L,Y,L1,L2).
dpartition([X|L],Y,L1,[X|L2]) :-
        dpartition(L,Y,L1,L2).
dpartition([],_,[],[]).

/* -------------------------------------------------- */
/*  Naive reverse for variable length lists...        */
/*  try with 10, 30, 50, 100, 150, 200.               */

nrev:- write('list length: '),
        read(X),
        conslist(X, List),
        statistics(runtime,[T1|_]),
        nreverse(List, _),
        statistics(runtime,[T2|_]),
        T is T2 - T1,
        I is (X*(X+3))/2 + 1,
        LIPS is I/T,
        write('LIPS= '),
        write(LIPS).

nreverse([], []).
nreverse([X|L0],L) :- nreverse(L0, L1),
    concatenate(L1, [X], L).

concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :- concatenate(L1, L2, L3).

conslist(0, []) :- !.
conslist(N, [N|L]) :-
        N1 is N-1,
        conslist(N1, L).
