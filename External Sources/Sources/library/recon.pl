%   File   : RECON.PL
%   Author : Mike Uschold + R.A.O'Keefe
%   Updated: 16 December 1983
%   Purpose: Handy versions of consult and reconsult using try_hard_to_see
%   Needs  : try_hard_to_see/4 from Util:TrySee.Pl

	%		BEWARE!!!
	%	This file contains user-modifiable tables
	%	(libdevice and libextension). Therefore,
	%	DO NOT COMPILE IT!!!

:- true, true.		%  the compiler doesn't like this.

:- op(1050, fx, (re)).		%   REconsult
:- op(1050, fx, (con)).		%   CONsult

:- public
	(re)/1,
	(con)/1..

:- mode
	re(+),
	con(+),
	recon(+, +).


(re Files) :-
	recon(Files, (re)).

(con Files) :-
	recon(Files, (con)).


recon(File, Flag) :-
	var(File), !,
	writeq('! recon: can''t '), writeq(Flag), tab(1), writeq(File), nl.
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
	writeq('! recon: can''t '), writeq(Flag), tab(1), writeq(File), nl.


libdevice(eco).
libdevice(mec).
libdevice(util).

libextension(pl).



