% Body System Predicates

X=X.

functor(T,F,A) :- structure(T) ; extract_functor(T,F1,A1), F=F1?, A=A1? .
functor(T,T,0) :- atomic(T) ; true.
functor(T,F,A) :- atomic(F) ; functor1(T,F,A).
functor(T,F,A) :- integer(A) ; functor2(T,F,A).
functor2(T,F,A) :- atomic(F) ; make_term(T1,F,A), T=T1? .
functor1(T,F,A) :- integer(A) ; make_term(T1,F,A), T=T1? .

A =.. [F|Args] :-
    nonvar(A) ;
	 extract_functor(A,F,Arity),args(Arity?,A,[],Args).
A =.. [F|Args] :-
     nonvar(F) ;
	 larg(0,A,F,Args?).
args(0,_,Args,Args).
args(N,A,Args,Args1) :-
    N > 0 ; arg(N,A,An), N1:=N-1, args(N1?,A,[An|Args],Args1).
larg(N,A,F,[]) :- make_term(A,F,N).
larg(N,A,F,[An|Arg]) :-
    N1 := N + 1, larg(N1,A,F,Arg?), arg(N1,A,An).

% compare(X,Y,C)
compare(X,Y,Result)  :-
    structure(X) ; compare_structure(X,Y,Result).
compare(X,Y,Result)  :-
    atomic(X) ; compare_non_structure(X,Y,Result).
compare(X,Y,Result)  :-
    var(X) ; compare_non_structure(X,Y,Result).
compare_non_structure(X,Y,=)  :-
    X #= Y ; true.
compare_non_structure(X,Y,<)  :-
    X #< Y ; true.
compare_non_structure(X,Y,>)  :-
    otherwise ; true.
compare_structure(X,Y,Result)  :-
    structure(Y) ;
	extract_functor(X,Fx,Ax),
	extract_functor(Y,Fy,Ay),
	compare_non_structure(Ax,Ay,Result_a),
	compare_functors(Result_a?,X,Y,Fx,Fy,Ax,Result).
compare_structure(X,Y,>)  :-
    otherwise ; true.
compare_functors(<,_,_,_,_,_,<).
compare_functors(>,_,_,_,_,_,>).
compare_functors(=,X,Y,Fx,Fy,A,Result)  :-
    compare_non_structure(Fx,Fy,Result_f),
    compare_args(Result_f?,X,Y,0,A,Result).
compare_args(<,_,_,_,_,<).
compare_args(>,_,_,_,_,>).
compare_args(=,_,_,N,N,=).
compare_args(=,X,Y,N,A,Result)  :-
    otherwise ;
	N1 := N + 1,
	arg(N1,X,Xn), arg(N1,Y,Yn),
	compare(Xn,Yn,Result1),
	compare_args(Result1?,X,Y,N1?,A,Result).
% Utilities
out([X|Xs]) :-wait(X) ; write(X), out(Xs?).
out([]).
%copy
% copy
copy(X,Y) :-
   copy(X,Y,[],_).
copy(X,X,L,L) :-
   atomic(X) ; true.
copy(X,Y,L1,L2) :-
   var(X) ;
	lookup(X,Y,L1,L2).
copy(X,Y,L1,L2) :-
   structure(X) ;
	extract_functor(X,F,A),
	make_term(Y,F,A),
	copy_args(X,Y,0,A?,L1,L2).
copy_args(_,_,N,N,L,L).
copy_args(X,Y,M,N,L1,L3) :-
   M #< N ;
	M1 := M + 1,
	arg(M1,X,Arg),
	copy(Arg,Argc,L1,L2),
	arg(M1,Y,Argc),
	copy_args(X,Y,M1?,N,L2,L3).
lookup(X,Y,[],[v(X,Y)]).
lookup(X,Y,[v(A,Y)|Ls],[v(A,Y)|Ls]) :-
   X #= A ; true.
lookup(X,Y,[v(A,B)|Ls],[v(A,B)|L1s]) :-
   otherwise ;
       lookup(X,Y,Ls,L1s).
append([],X,X).
append([X|Xs],Y,[X|Zs]) :-
   append(Xs?,Y,Zs).
% actually a kernel pred
screen([X|Xs]) :- write(X), screen(Xs?).
screen([]).
