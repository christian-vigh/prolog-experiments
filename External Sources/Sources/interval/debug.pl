%Copyright 1992
%John G. Cleary
%University of Calgary
%cleary@cpsc.ucalgary.ca

%This file provides the implementation of a tracing/debugging/statistics
%system
%See also nodebug.pl which when loaded sets up macros that omit calls to the
%debugging (for efficiency reasons)

:-dynamic ii_tracing/0.
:- export ii_trace/0, noii_trace/0, ii_stats/0.

ii_trace:- ii_tracing, !, ii_tr_on.
ii_trace:- assert(ii_tracing), ii_tr_on.

ii_tr_on:- debug_init, print('Interval tracing on'), nl.

noii_trace:- retract_all(ii_tracing), print('Interval tracing off'), nl.

debug_ii(Mode,Term,D):-
	ii_tracing, !,
	D=debug(Term,N),
	getval(debug_counter,N),
	incval(debug_counter),
	debug_print(Mode,D).
debug_ii(_,Term,debug(Term,notrace)). %ii-Tracing not on

debug_set(V,L,H,debug(Term,N)):- 
	meta_term(T,ii(L,H)), debug_i(set,debug(set(V,T,Term),N)).
debug_i(Mode,D):- ii_tracing, !, 
	(not(debug_mode(Mode))
            ->(print('MODE IN ERROR'), print(Mode),nl)
             ;true),
	debug_print(Mode,D).
debug_i(_,_).

debug_print(Mode,debug(Term,N)):-
	print('**'), print(N), print(':'), print(Mode), 
	print('+'), print(Term), nl,
	incval(Mode).
debug_print(Mode,debug(Term,N)):-
	print('**'), print(N), print(':'), print(Mode), 
	print('-'), print(Term), nl, 
	fail.

debug_init:- exec_all((debug_mode(M),setval(M,0))).

debug_mode(debug_counter).
debug_mode(start).
debug_mode(split).
debug_mode(wake).
debug_mode(cases).
debug_mode(set).
debug_mode(unify).
debug_mode(bind).
debug_mode(done).
debug_mode(retry).
debug_mode(delay).

ii_stats:- 
	print('Integer interval arithmetic constraint statistics:'), nl,
	exec_all((debug_mode(M),d_st(M))),
	debug_init.

d_st(M):- getval(M,C), print(':'), print(M), print(':'), print(C), nl.

debug_check(Goal):- not(not(Goal)), !.
debug_check(Goal):- 
	nl,
	print('****'),
	print('Debug assertion failed:'), nl,
	print(Goal),
	nl.
