% atom_prefix( +Atom, +Prefix ) :-
% True if Prefix and Full are atoms and Prefix is a prefix of Full.
% The order convention is from Swi (5:0:6) which already defines this.
%
atom_prefix( Atom, Prefix ) :-
	atom_concat( Prefix, _, Atom ).
