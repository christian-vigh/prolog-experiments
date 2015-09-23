%   File: Timing.Pl	Author: R.A.O'Keefe	Updated: 10 August 82

%   To obtain call count information, consult this file AFTER you have
%   built your Prolog program.  It will slow the program down a lot, but
%   that can't be helped.  You have to stop the program by calling
%   clock_out explicitly, otherwise the file Timing.Log won't be closed
%   properly.  Timing.Log is a trace of {F A<nl>} for each CALL.

:-
	current_predicate(_, Head),
	functor(Head, Functor, Arity),
	asserta((
		Head :- clock_in(Functor, Arity)
	)),
%	write(Functor/Arity), write(' clocked'), nl,
	fail
;	write('remember to call clock_out before halting.'), nl
.

clock_in(Functor, Arity) :-
	telling(OldFile),
	tell('timing.log'),
	write(Functor), put(" "),
	write(Arity), nl,
	tell(OldFile), !,
	fail.

clock_out :-
	telling(OldFile),
	tell('timing.log'),
	told,
	tell(OldFile).



