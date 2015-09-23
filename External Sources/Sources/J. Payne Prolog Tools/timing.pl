%   File: Timing.Pl	Author: R.A.O'Keefe	Updated: 10 August 82

%   To obtain call count information, consult this file AFTER you have
%   built your Prolog program.  It will slow the program down a lot, but
%   that can't be helped.  You have to stop the program by calling
%   clock_out explicitly, otherwise the file Timing.Log won't be closed
%   properly.  Timing.Log is a trace of {F A<nl>} for each CALL.

%	Note (KJ 12-8-87) I have no evidence that this will work
%	on any current system. There is an associated file called
%	TIMING.POP written in Pop-2 which, presumably, works
%	closely with this one.

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
