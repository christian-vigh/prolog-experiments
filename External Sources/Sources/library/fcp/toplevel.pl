% adapted from <CHIKAYAMA.CP>CPI.PL.7, 16-May-84 16:40:11, Edit by CHIKAYAMA

% Flat Concurrent PROLOG System -- -- C-Prolog Version
% In this version, the commit bar "|" is replaced by ";".
% This is required because of one of the incompatibilities of C-Prolog
% with DEC-10 Prolog.
:- op(1200,xfy,'<-').
:- op(1100,xfy,'|').
:- op(1150,fx,((fcp))).
:- op(1150,fx,((fcpd))).
:- op(1000,xfy,'<<').
:- op(950,xfy,&).
:- op(900,fy,fs).
:- op(900,fy,fns).
:- op(900,fy,fcpspy).
:- op(900,fy,fcpnospy).
:- op(700,xfx,:=).
:- op(700,xfx,\=).
:- op(450,xf,'?').

%%% SAVE

%%% TOP LEVEL


fcp((Goals<<Input)) :-!, Begin is cputime, do_solve_in(Goals, R, S, Input), !,
     T is cputime-Begin, fcpstats(R, S, T).
fcp(Goals) :- Begin is cputime, do_solve(Goals, R, S), !,
     T is cputime-Begin, fcpstats(R, S, T).
fcp(Goals) :- nl.

fcpd(Goals) :- Begin is cputime, do_solve_df(Goals, R, S), !,
     T is cputime-Begin, fcpstats(R, S, T).
fcpd(Goals) :- nl.

do_solve_in(Goals, R, S, Input) :-
     put_queue(Goals, [G|Q], [$$$(Input)|QT]),
   ( recorded(fcpdebug, on, _), !, solve_in_t(G, Q, QT, 0, 0, R-S);
     solve_in(G, Q, QT, 0, 0, R-S) ).
do_solve(Goals, R, S) :-
     put_queue(Goals, [G|Q], [$$$|QT]),
   ( recorded(fcpdebug, on, _), !, solve_t(G, Q, QT, 0, 0, R-S);
     solve(G, Q, QT, 0, 0, R-S) ).

do_solve_df(Goals, R, S) :-
     put_queue(Goals, [G|Q], [$$$|QT]),
   ( recorded(fcpdebug, on, _), !, solve_df_t(G, Q, QT, 0, 0, R-S);
     solve_df(G, Q, QT, 0, 0, R-S) ).

fcpstats(R, S, T) :-
     write(R), write(' reductions and '),
     write(S), write(' suspensions in '),
     write(T), write(' sec. ('),
     RPS is R/T, write(RPS), write(' rps.)'), nl.




%%% SCHEDULING

% solve(Goal, Ready_queue, Queue_tail, Reductions, Suspensions, Result).
%   There are 8 versions of "solve" for souping up the kernel.
%   All of them do almost the same thing, however, ...
%     Ones with "_nd" are used after some reductions in a cycle.
%     Ones with "_t"  are for tracing.
%     Ones with "_df" are for depth-first scheduling.

solve($$$, Q, Qt, _, _, _) :- !, deadlock(Q,Qt).
solve($kernel(P,G), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, solve_nd(N, Q, T, R1, S, C).
solve($kernel(P,G), [N|Q], [$kernel(P,G)|T], R, S, C) :- !,
    S1 is S+1, solve(N, Q, T, R, S1, C).
solve($(G, T, TT), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, solve_nd(N, Q, TT, R1, S, C).
solve(G, [N|Q], [G|T], R, S, C) :- S1 is S+1, solve(N, Q, T, R, S, C).

solve_nd($$$, [], _, R, S, R-S) :- !.
solve_nd($$$, [G|Q], [$$$|T], R, S, C) :- !, solve(G, Q, T, R, S, C).
solve_nd($kernel(P,G), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, solve_nd(N, Q, T, R1, S, C).
solve_nd($kernel(P,G), [N|Q], [$kernel(P,G)|T], R, S, C) :- !,
     S1 is S+1, solve_nd(N, Q, T, R, S1, C).
solve_nd($(G, T, TT), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, solve_nd(N, Q, TT, R1, S, C).
solve_nd(G, [N|Q], [G|T], R, S, C) :-
     S1 is S+1, solve_nd(N, Q, T, R, S1, C).

solve_t($$$, Q, Qt, _, _, _) :- !, deadlock(Q,Qt).
solve_t($kernel(P,G), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, trace_kernel_reduction(P, R1),
     solve_t_nd(N, Q, T, R1, S, C).
solve_t($kernel(P,G), [N|Q], [$kernel(P,G)|T], R, S, C) :- !,
     S1 is S+1, trace_kernel_suspension(P, S1), solve_t(N, Q, T, R, S1, C).
solve_t($(G, T, TT), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, trace_reduction(G, T, TT, R1),
     solve_t_nd(N, Q, TT, R1, S, C).
solve_t(G, [N|Q], [G|T], R, S, C) :-
     S1 is S+1, trace_suspension(G, S1), solve_t(N, Q, T, R, S1, C).

solve_t_nd($$$, [], _, R, S, R-S) :- !.
solve_t_nd($$$, [G|Q], [$$$|T], R, S, C) :- !, solve_t(G, Q, T, R, S, C).
solve_t_nd($kernel(P,G), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, trace_kernel_reduction(P, R1),
     solve_t_nd(N, Q, T, R1, S, C).
solve_t_nd($kernel(P,G), [N|Q], [$kernel(P,G)|T], R, S, C) :- !,
     S1 is S+1, trace_kernel_suspension(P, S1), solve_t_nd(N, Q, T, R, S1, C).
solve_t_nd($(G, T, TT), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, trace_reduction(G, T, TT, R1),
     solve_t_nd(N, Q, TT, R1, S, C).
solve_t_nd(G, [N|Q], [G|T], R, S, C) :-
     S1 is S+1, trace_suspension(G, S1), solve_t_nd(N, Q, T, R, S1, C).

%  with input stream
solve_in($$$(I), Q, T, R, S, C) :- !,
     display('? '),
     read(Ip), do_read_solve(Ip,I,Q,T,R,S,C).
do_read_solve(Ip,[],[G|Q],[$$$|T],R,S,C)  :-
      Ip == end_of_file,!,display(end_of_file),nl,
      solve(G, Q, T, R, S, C).
do_read_solve(I,[I|Is],[G|Q],[$$$(Is)|T],R,S,C)  :- !,
     solve_in(G, Q, T, R, S, C).
solve_in($kernel(P,G), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, solve_in_nd(N, Q, T, R1, S, C).
solve_in($kernel(P,G), [N|Q], [$kernel(P,G)|T], R, S, C) :- !,
    S1 is S+1, solve_in(N, Q, T, R, S1, C).
solve_in($(G, T, TT), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, solve_in_nd(N, Q, TT, R1, S, C).
solve_in(G, [N|Q], [G|T], R, S, C) :- S1 is S+1, solve_in(N, Q, T, R, S, C).

solve_in_nd($$$(I), [], _, R, S, R-S) :- !.
solve_in_nd($$$(I), [G|Q], [$$$(I)|T], R, S, C) :- !,
    solve_in(G, Q, T, R, S, C).
solve_in_nd($kernel(P,G), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, solve_in_nd(N, Q, T, R1, S, C).
solve_in_nd($kernel(P,G), [N|Q], [$kernel(P,G)|T], R, S, C) :- !,
     S1 is S+1, solve_in_nd(N, Q, T, R, S1, C).
solve_in_nd($(G, T, TT), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, solve_in_nd(N, Q, TT, R1, S, C).
solve_in_nd(G, [N|Q], [G|T], R, S, C) :-
     S1 is S+1, solve_in_nd(N, Q, T, R, S1, C).

solve_in_t($$$(I), Q, T, R, S, C) :- !,
     display('** Input? '),
     read(Ip), do_read_solve_t(Ip,I,Q,T,R,S,C).
do_read_solve_t(Ip,[],[G|Q],[$$$|T],R,S,C)  :-
     Ip == end_of_file,!, display(end_of_file),nl,
     solve_t(G, Q, T, R, S, C).
do_read_solve_t(I,[I|Is],[G|Q],[$$$(Is)|T],R,S,C)  :- !,
      solve_in_t(G, Q, T, R, S, C).
solve_in_t($kernel(P,G), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, trace_kernel_reduction(P, R1),
     solve_in_t_nd(N, Q, T, R1, S, C).
solve_in_t($kernel(P,G), [N|Q], [$kernel(P,G)|T], R, S, C) :- !,
     S1 is S+1, trace_kernel_suspension(P, S1), solve_in_t(N, Q, T, R, S1, C).
solve_in_t($(G, T, TT), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, trace_reduction(G, T, TT, R1),
     solve_in_t_nd(N, Q, TT, R1, S, C).
solve_in_t(G, [N|Q], [G|T], R, S, C) :-
     S1 is S+1, trace_suspension(G, S1), solve_in_t(N, Q, T, R, S1, C).

solve_in_t_nd($$$(I), [], _, R, S, R-S) :- !.
solve_in_t_nd($$$(I), [G|Q], [$$$(I)|T], R, S, C) :- !,
    solve_in_t(G, Q, T, R, S, C).
solve_in_t_nd($kernel(P,G), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, trace_kernel_reduction(P, R1),
     solve_in_t_nd(N, Q, T, R1, S, C).
solve_in_t_nd($kernel(P,G), [N|Q], [$kernel(P,G)|T], R, S, C) :- !,
     S1 is S+1, trace_kernel_suspension(P, S1),
     solve_in_t_nd(N, Q, T, R, S1, C).
solve_in_t_nd($(G, T, TT), [N|Q], T, R, S, C) :- call(G), !,
     R1 is R+1, trace_reduction(G, T, TT, R1),
     solve_in_t_nd(N, Q, TT, R1, S, C).
solve_in_t_nd(G, [N|Q], [G|T], R, S, C) :-
     S1 is S+1, trace_suspension(G, S1), solve_in_t_nd(N, Q, T, R, S1, C).
get_input(I) :-
   read(X),
   do_read(X,I).
do_read(end_of_file,[]) :- !.
do_read(X,X).
%  Deadlock

deadlock(Q,Qt)  :-
   write(' ** deadlock ** '),nl,
   get_queue(Q,Qt,B),
   remove_useless_ro(B,BB), lettervars(BB),
   write_all_susp(BB), fail,!.
write_all_susp([])  :- nl.
write_all_susp([$kernel(P,G)|Xs]) :-
   write('*** '), write(G),nl,
   write_all_susp(Xs).
write_all_susp([X|Xs]) :-
   write('*** '), write(X),nl,
   write_all_susp(Xs).
