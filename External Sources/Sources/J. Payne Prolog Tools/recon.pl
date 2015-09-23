%   File   : RECON.PL
%   Author : Mike Uschold + R.A.O'Keefe
%   Updated: 5 October 1984
%   Purpose: Handy versions of consult and reconsult using try_hard_to_see
%   Needs  : try_hard_to_see/4 from Util:TrySee.Pl

	%		BEWARE!!!
	%	This file contains user-modifiable tables
	%	(libdevice and libextension). Therefore,
	%	DO NOT COMPILE IT!!!

:- public
	(re)/1,
	(con)/1.

:- mode
	re(+),
	con(+),
	recon(+, +).

:- true, true.		%  the compiler doesn't like this.

:- op(1050, fx, (re)).		%   REconsult
:- op(1050, fx, (con)).		%   CONsult


(re Files) :-
	recon(Files, (re)).

(con Files) :-
	recon(Files, (con)).


recon(File, Flag) :-
	var(File), !,
	write('! recon: can''t '), write(Flag), put(32), write(File), nl.
recon([], _) :- !.
recon([File|Files], Flag) :- !,
	recon(File,  Flag),
	recon(Files, Flag).
recon((File,Files), Flag) :- !,
	recon(File,  Flag),
	recon(Files, Flag).
recon(File, Flag) :-
	atomic(File),
	bagof(D, libdevice(D), Devs),
	bagof(E, libextension(E), Exts),
	try_hard_to_see(File, Devs, Exts, FullTitle),
	(   Flag = (con), consult(FullTitle)
	;   Flag = (re),  reconsult(FullTitle)
	),  !.
recon(File, Flag) :-
	write('! recon: can''t '), write(Flag), put(32), write(File), nl.


libdevice(eco).
libdevice(mec).
libdevice(util).

libextension(pl).
