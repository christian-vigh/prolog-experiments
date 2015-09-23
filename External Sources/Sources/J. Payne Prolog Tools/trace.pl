%   File   : TRACE.PL
%   Author : Lawrence
%   Updated: 24 February 1984
%   Purpose: Tracing routines.
%   Needs  : writef.pl, flag.pl
	
% FIXES
%
%  (11 May 81)
%
%	Split the (now obsolete) module IOROUT into two: WRITEF and
%	TRACE (this one).
%

:- public
 	error/3,
 	tlim/1,
 	ton/1,
 	toff/1,
 	toff/0,
 	trace/2,
 	trace/3.
 
:- mode
 	error(+, +, +),
 	tlim(?),
 	ton(?),
 	toff(?),
 	toff,
 	trace(+, +),
 	trace(+, +, +).
 
			% Error message handler
			%  Prints a (writef style) message and then performs
			%  the specified action.

error(Format, List, Action) :-
	nl,
	write('** ERROR '),
	writef(Format, List),
	writef('\n   ( %t after error )\n', [Action]),
	Action.


			% Set tracing level for level conditional tracing

tlim(N) :-
	flag(tflag, Old, N),
	fwritef(user, '\nTracing level reset from %t to %t.\n', [Old,N]).


			% Set/unset various name conditional trace messages
			%  The Name "all" is treated specially by trace/3 to
			%  effectively switch on ALL named tracing messages.

ton(Name) :-
	tracing(Name),
	!,
	display('You are already tracing '),
	display(Name), ttynl.
ton(Name) :-
	asserta(tracing(Name)),
	display('Now tracing '),
	display(Name), ttynl.



toff(Name) :-
	retract(tracing(Name)),
	!,
	display('No longer tracing '),
	display(Name), ttynl.
toff(Name) :-
	display('You were not tracing '),
	display(Name), ttynl.



toff :-
	abolish(tracing, 1),
	display('All named tracing switched off'), ttynl.


			% Print out a trace message
			%  There are two styles of trace message;
			%  Those conditional on a specific name and those
			%  conditional on a numeric tracing level.
			%   Name conditional trace message are switched
			%   on and of using ton(_) and toff(_)
			%   Number conditional trace messages are dependent
			%   on the tlim(_) flag which specifies the current
			%   level of tracing.

trace(Format, N) :-
	trace(Format, [], N).


trace(Format, List, Name) :-
	atom(Name),
	( tracing(Name)  ;  tracing(all) ),
	!,
	writef(Format, List).
trace(Format, List, N) :-
	integer(N),
	flag(tflag, M, M),
	N =< M,
	!,
	writef(Format, List).
trace(_, _, _).
