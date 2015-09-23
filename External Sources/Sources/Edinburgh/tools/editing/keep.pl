% keep a predicate(s) in a file
% The file name is remembered from one keep to the next
% The existing content of the file is NOT PRESERVED

keep(Predicate,File) :-
	nonvar(Predicate),
	atomic(File),
	!,
	(
			keep_file(Old_file)
		->	retract(keep_file(Old_file))
	;
		true
	),
	asserta(keep_file(File)),
	keep(Predicate).

keep(_,_) :-
	write('keep/2 must have both arguments instantiated.'), nl,
	fail.

keep(Predicate) :-
	keep_file(File),
	nonvar(Predicate),
	!,
	tell(File),
	listall(Predicate),
	told,
	write('saved in '), write(File), nl.

keep(Pred) :-
	nonvar(Pred),
	!,
	write('You must specify a keep file'), nl,
	write('Use keep/2 this time and keep/1 thereafter.'), nl,
	fail.

keep(_) :-
	write('The argument to keep/1 must be instantiated'), nl,
	fail.

listall([Predicate|More_predicates]) :-	% Listall has to be able to cope with
	listing(Predicate),		% an argument which is a predicate
	listall(More_predicates).	% name or a list of predicate names.
					% Hence the odd clause about
listall([]).				% Predicate not being [] (below)

listall(Predicate) :-
	Predicate \== [],
        listing(Predicate).
