%%% TRACING
(fs) :- fcpspy.
(fns) :- fcpnospy.
fs([]).
fs([X|Xs]) :-
   fcpspy(X), fs(Xs).
fs(X)  :-
   fcpspy(X).
fns([]).
fns([X|Xs]) :-
   fcpspy(X), fns(Xs).

(fcpspy) :- recorded(fcpspy, _, R), erase(R), fail.
(fcpspy) :- fcpdebug_on, recorda(fcpspy, everything, _),
     write('All FCP reductions will be traced.'), ttynl.

fcpspy(X) :- fcpdebug_on,
  (  X=F/A, !, recorda(fcpspy, X, _); recorda(fcpspy, X/_, _) ),
     write('FCP spy-point placed on '), write(X), put("."), nl.

(fcpnospy) :- recorded(fcpspy, _, R), erase(R), fail.
(fcpnospy) :- fcpdebug_off.

fcpnospy(A/A) :-
     recorded(fcpspy, F/A, R), erase(R),
     write('FCP spy-point on '), write(F/A), write(' removed.'), nl, fail.
fcpnospy(X) :-
     recorded(fcpspy, X/_, R), erase(R),
     write('FCP spy-point on '), write(X), write(' removed.'), nl, fail.
fcpnospy(X) :- recorded(fcpspy, _, _), !.
fcpnospy(_) :- fcpdebug_off.

fcpdebugging :- recorded(fcpdebug, on, _), !,
write('FCP debug mode is switched on.'), nl,
  (  setof(X, R^recorded(fcpspy, X, R)), !,
     ( S=[V|_], var(V), !, write('All FCP reductions are traced.');
       write('FCP spy-points set on: '), write(S) ), nl;
     true ).
fcpdebugging :- write('FCP debug mode is switched off.'), nl.

fcpdebug_on :- recorded(fcpdebug, on, _), !.
fcpdebug_on :- recorda(fcpdebug, on, _), write('FCP debug mode switched on.'),
		nl.

fcpdebug_off :- recorded(fcpdebug, _, R), !,
     erase(R), write('FCP debug mode switched off.'), nl.
fcpdebug_off.

fcpspied(G) :- recorded(fcpspy, everything, _), !.
fcpspied(G) :- functor(G, F, A), A2 is A+2, recorded(fcpspy, F/A2, _), !.

trace_reduction(G0, Q, QT, R) :-
     fcpspied(G0), get_queue(Q, QT, B),
     functor(G0, F, A3), A is A3-3, functor(G, F, A), c_copy_args(A, G0, G),
     remove_useless_ro(G, GG), remove_useless_ro(B, BB),
     arg(A3,G0,Gu),
     remove_useless_ro(Gu,GGu),
     lettervars((GG,GGu, BB)),
     put(" "), write(R), write(':'),
     print(GG), print(' <- '), print(GGu), print(' | '),
     print_reduction('',BB),
     fail.
trace_reduction(_, _, _, _).

print_reduction(Delim,[H|T]) :- !,
     print(Delim), print(H),  print_reduction(' , ',T).
print_reduction('',[]) :- print('true.'), nl.
print_reduction(' , ',[]) :- put("."), nl.

get_queue(X, Y, []) :- X==Y, !.
get_queue([$kernel(P,G)|T], TT, [P|T1]) :-
     get_queue(T, TT, T1).
get_queue([$(H0, _, _)|T], TT, [H|T1]) :-
     functor(H0, F, A3), A is A3-3, functor(H, F, A),
     c_copy_args(A, H0, H), get_queue(T, TT, T1).
trace_kernel_reduction(P, R) :-
     fcpspied(P),
     remove_useless_ro(P, PP),
     lettervars(PP),
     put(" "), write(R), write(':'),
     print(PP), print(-kernel), nl, fail.
trace_kernel_reduction(_,_).

trace_suspension($(G0,_,_), S) :-
     fcpspied(G0),
     functor(G0, F, A2), A is A2-3, functor(G, F, A), c_copy_args(A, G0, G),
     remove_useless_ro(G, GG),
     lettervars(GG),
     write(' *** '), write(S), write(':'),
     print(GG), put("."), nl, fail.
trace_suspension(_, _).
trace_kernel_suspension(G, S) :-
     fcpspied(G),
     remove_useless_ro(G, GG), lettervars(GG),
     write(' *** '), write(S), write(':'),
     print(GG), put("."), nl, fail.
trace_kernel_suspension(_, _).
lettervars(X) :-
	varlist(X,V1),
	sort(V1,V2),
	% V1=V2,
	unify_vars(V2,
	['X','Y','Z','U','V','W','X1','Y1','Z1','U1','V1','W1',
	'X2','Y2','Z2','U2','V2','W2','X3','Y3','Z3','U3','V3','W3',
	'X4','Y4','Z4','U4','V4','W4']), !.
unify_vars([X|L1],[X|L2]) :-  !,
	unify_vars(L1,L2).
unify_vars([_|L1],[_|L2]) :-  !,
	unify_vars(L1,L2).
unify_vars(_,_).
% varlist(T,L,[]) :- L is all occurences of distinct variables in term T
varlist(X,L) :- varlist(X,L,[]), !.
varlist(X,[X|L],L) :- var(X),!.
varlist(T,L0,L) :- T =.. [F|A], !, varlist1(A,L0,L).
varlist1([T|A],L0,L) :- varlist(T,L0,L1), !, varlist1(A,L1,L).
varlist1([],L,L).
